%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) nisbus 2011, 
%%% @doc
%%%   Manages the FIX Session state and everything else :)
%%% @end
%%% Created :  8 Nov 2011 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(fix_session).

-behaviour(gen_fsm).
-include("../include/session_records.hrl").
%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, 
	 terminate/3, code_change/4]).

%%gen_fsm states
-export([disconnected/2, connected/2, 
	 logged_on/2, logged_off/2]).

-export([send/2]).

-compile([{parse_transform, lager_transform}]).

-define(SERVER, ?MODULE).

-record(session_state, 
	{
	  next_expected_incoming :: integer(),
	  next_expected_outgoing :: integer(),
	  session :: #session_settings{},
	  last_sequence_number :: integer(),
	  socket :: port() | undefined,
	  heartbeat_ref :: any(),
	  next_sender_seq_num :: integer(),
	  next_target_seq_num :: integer(),
	  messages :: [#message{}]
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%Starts the server and begins connecting
-spec start_link(#session_settings{}) -> ok.
start_link(#session_settings{session_id = Id} = SessionSettings) ->
    lager:debug("Starting session ~p~n",[Id]),
    Name = list_to_atom(Id),
    gen_fsm:start_link({local, Name}, ?MODULE,[SessionSettings],[]).

send(Id,Msg) ->
    gen_fsm:send_event(Id,{send,Msg}).
%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%Initializes the state with session info and starts connecting
init([#session_settings{data_dictionary = Dict, session_id = _Id}= Session_Settings]) ->
    PrivDir = case code:priv_dir(quickfix_erl) of
		  {error,_} -> 
		      {ok,F} = file:get_cwd(),
		      F++"/../priv/";
		  F -> F
	      end,
    {_Major,_Minor,Messages} = session_utils:parse_dict(Dict,PrivDir++"/spec.xsd"),
    lager:debug("Connecting ~p~n",[_Id]),
    self() ! connect,
    {ok, disconnected, #session_state{session = Session_Settings, messages = Messages}}.

%%##########################################################################
%%                              STATE MATRIX
%%##########################################################################
%%==========================================================================
%%|              | disconnected |  connected  |  logged_off  |  logged_on  |
%%|--------------|--------------|-------------|--------------|-------------|
%%| disconnected |      X       |      X      |              |             |
%%|--------------|--------------|-------------|--------------|-------------|
%%|  connected   |      X       |             |              |      X      |
%%|--------------|--------------|-------------|--------------|-------------|
%%|  logged_on   |      X       |             |      X       |      X      |
%%|--------------|--------------|-------------|--------------|-------------|
%%|  logged_off  |      X       |             |      X       |             |
%%|--------------|--------------|-------------|--------------|-------------|
%%==========================================================================

%%##########################################################################
%%                          DISCONNECTED
%%##########################################################################
disconnected(connect, #session_state{socket = Socket, session = Session} = State) when Socket == undefined ->
    lager:debug("disconnected, with tcp = undefined~n"),
    %%Create TCP and move on
    IP = Session#session_settings.socket_connect_host,
    Port = Session#session_settings.socket_connect_port,
    lager:error("IP ~p and port ~p~n",[IP,Port]),
    case gen_tcp:connect(IP,Port,[binary, {packet, 0}, {active, false}, {reuseaddr, true}, {send_timeout, 5000}],10000) of
	{ok,S} ->
	    ?MODULE:connected(connected, State#session_state{socket = S});
	{error, E} ->
	    lager:error("Error connecting ~p, doing failover~n",[E]),
	    ?MODULE:disconnected(failover, State)
    end;

disconnected(failover, #session_state{session=Session} =  State) ->
    lager:debug("disconnected, failover~n"),
    IP = Session#session_settings.socket_failover_host,
    Port = Session#session_settings.socket_failover_port,
    case Session#session_settings.reset_on_disconnect of
	true ->
	    set_sequence_number(1);
	_ -> void
    end,
    case gen_tcp:connect(IP,Port,[{packet,2}]) of
	{ok,S} ->
	    ?MODULE:connected(connected, State#session_state{socket = S});
	{error, E} ->
	    lager:error("Error connecting ~p, doing disaster~n",[E]),
	    ?MODULE:disconnected(disaster, State)
    end;

disconnected(disaster, #session_state{session=Session} =  State) ->
    lager:debug("disconnected, disaster~n"),
    IP = Session#session_settings.socket_disaster_host,
    Port = Session#session_settings.socket_disaster_port,
    case Session#session_settings.reset_on_disconnect of
	true ->
	    set_sequence_number(1);
	_ -> void
    end,
    case gen_tcp:connect(IP,Port,[{packet,2}]) of 
	{ok,S} -> 
	    ?MODULE:connected(connected, State#session_state{socket = S});
	{error, E} ->
	    lager:error("Error connecting ~p, back to main~n",[E]),
	    ?MODULE:disconnected(connect, State)
    end;

disconnected({send, _}, State) ->
    lager:error("Can't send message in disconnected state~n"),
    throw({error, disconnected}),
    {next_state, disconnected, State}.


%%##########################################################################
%%                          CONNECTED
%%##########################################################################

connected({error, _Reason}, State) ->
    lager:debug("connected, error~n"),
    {next_state, disconnected, State};

%%We are connected and send the login message
connected(_, #session_state{session = Session, socket = Socket} = State) ->
    lager:debug("connected~n"),
    Seq = get_sequence_number(Session#session_settings.session_id),

    %%Send logon
    Logon = message_utils:create_logon(<<"1">>,Session),
    message_store:save_msg(Session#session_settings.session_id,Logon),
    try gen_tcp:send(Socket, Logon) of
	{error, Reason} ->
	    lager:error("Error logging in ~p~n",[Reason]);
	ok ->
	    lager:debug("Sent logon: ~s~n",[Logon]),
	    NextSeq = increment_sequence(Seq),
	    set_sequence_number(NextSeq),
	    ?MODULE:logged_on(undefined,State)
    catch
	Ex ->
	    lager:error("Error sending login~p~n",[Ex])
    end,
    {next_state, logged_on, State#session_state{last_sequence_number = Seq}}.

%%##########################################################################
%%                          LOGGED ON
%%##########################################################################

logged_on({send,Msg}, #session_state{socket = Socket, session = Session} = State) ->
    I = message_utils:proplist_to_msg(Msg,Session),
    FIX = message_utils:create_msg(I,Session),
    message_store:save_msg(Session#session_settings.session_id,FIX),
    gen_tcp:send(Socket,FIX),
    ?MODULE:logged_on(undefined, State);

logged_on(_, #session_state{socket = Socket, messages = Messages, session = Session} = State) ->
    case gen_tcp:recv(Socket, 0) of
	{ok,D} ->
	    message_store:save_msg(Session#session_settings.session_id,D),
	    MessageDef = session_utils:get_msg_type(D,Messages),
	    Msg = session_utils:parse_fix_msg(D,MessageDef),
	    handle_message(MessageDef,Msg,State),
	    ?MODULE:logged_on(undefined,State);	    
	_Other ->
	    ?MODULE:logged_off(undefined,State)
    end,
    {next_state, logged_on, State}.

%%We are logged off so we connect again
logged_off(_, State) ->
    {next_state, logged_on, State}.


handle_event({send,Msg}, StateName, State)->
    ?MODULE:StateName({send, Msg},State);

handle_event(_Event, StateName, State) ->
    lager:debug("Received unknown event ~p~n",[_Event]),
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(connect, StateName, State) ->
    lager:debug("handle info, connect~n"),
    ?MODULE:StateName(connect, State);

handle_info(Info, StateName, StateData) ->
    lager:debug("handle info, ~p, ~p~n",[Info, StateName]),
    {noreply, StateName, StateData}.

terminate(_Reason, _StateName, #session_state{socket = Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%=======================================================================================
%%%             ADMIN MESSAGES
%%%=======================================================================================
%%LOGON
handle_message(#message{is_admin = true, tag = <<"A">>}, ParsedMsg, State) ->
    ?MODULE:logged_on(undefined,State);

%%HEARTBEAT
handle_message(#message{is_admin = true, tag = <<"0">>}, ParsedMsg, #session_state{session = Session, socket = Socket} = State) ->
    Seq = get_sequence_number(Session#session_settings.session_id),
    Heartbeat = message_utils:create_heartbeat(Seq,Session),
    message_store:save_msg(Session#session_settings.session_id,Heartbeat),
    gen_tcp:send(Socket,Heartbeat),
    NextSeq = increment_sequence(Seq),
    set_sequence_number(NextSeq),
    ?MODULE:logged_on(undefined,State);

 %%TEST REQUEST
handle_message(#message{is_admin = true, tag = <<"1">>, fields = Fields}, ParsedMsg, #session_state{session = Session, socket = Socket} = State) ->
    ReqId = proplists:get_value(testreqid,ParsedMsg),
    Seq = get_sequence_number(Session#session_settings.session_id),
    Heartbeat = message_utils:create_heartbeat(Seq,Session,ReqId),
    message_store:save_msg(Session#session_settings.session_id,Heartbeat),
    gen_tcp:send(Socket,Heartbeat),
    NextSeq = increment_sequence(Seq),
    set_sequence_number(NextSeq),
    ?MODULE:logged_on(undefined,State);

%%RESEND REQUEST
handle_message(#message{is_admin = true, tag = <<"2">>}, ParsedMsg, State) ->
    ?MODULE:logged_on(undefined,State);
%%REJECT
handle_message(#message{is_admin = true, tag = <<"3">>, fields = Fields}, ParsedMsg, _State) ->
    Text = proplists:get_value(text,Fields),
    lager:error("Rejected ~p~n",[Text]);
%%SEQUENCE RESET
handle_message(#message{is_admin = true, tag = <<"4">>}, ParsedMsg, State) ->
    ?MODULE:logged_on(undefined,State);
%%LOGOUT
handle_message(#message{is_admin = true, tag = <<"5">>,fields = Fields}, ParsedMsg, _State) ->
    Text = proplists:get_value(text,Fields),
    lager:error("Logged off ~p~n",[Text]);

%%%=======================================================================================
%%%             APPLICATION MESSAGES
%%%=======================================================================================
handle_message(_MsgDef, _ParsedMsg, State) ->
    ?MODULE:logged_on(undefined,State).

%%%=======================================================================================
%%%             Sequence number handling
%%%=======================================================================================

get_sequence_number(SessionID) ->
    case erlang:get(seq) of
	undefined -> 
	    S = get_seq_from_file(SessionID),
	    set_sequence_number(S),
	    lager:debug("Returning seq ~p~n",[S]),
	    S;
	Seq when is_binary(Seq) ->
	    Seq
    end.

set_sequence_number(Seq) when is_integer(Seq) ->
    S = list_to_binary(integer_to_list(Seq)),
    erlang:put(seq, S);
set_sequence_number(Seq) when is_binary(Seq) ->
    erlang:put(seq,Seq).

get_seq_from_file(SessionID) ->
    File = SessionID++".seq",
    case file:read_file(File) of
	{ok, S} ->
	    S;
	{error, Reason} ->	    
	    lager:error("Error reading seq file ~p~n",[Reason]),
	    list_to_binary(integer_to_list(1))
    end.

increment_sequence(Seq) when is_binary(Seq) ->
    list_to_integer(binary_to_list(Seq))+1;
increment_sequence(Seq) when is_integer(Seq) ->
    Seq+1.
