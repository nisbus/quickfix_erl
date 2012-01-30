%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) nisbus 2011, 
%%% @doc
%%%
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
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-compile([{parse_transform, lager_transform}]).

%%gen_fsm states
-export([disconnected/2, connected/2, logged_on/2, logged_off/2, receiving/2]).

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
	  admin_handler :: fun(),
	  msg_handler :: fun()
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%Starts the server and begins connecting
-spec start_link(#session_settings{}) -> ok.
start_link(SessionSettings) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE,[SessionSettings],[]).

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
    {_Major,_Minor,_Header,_Messages,_Trailer,_Comp,_Fields} = session_utils:parse_dict(Dict,PrivDir++"/spec.xsd"),
    self() ! connect,
    
    {ok, disconnected, #session_state{session = Session_Settings}}.

%%Initial connection
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

%%Connect failed
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

%%Failover didn't manage to connect
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

%%The connection is closed with the market
disconnected(closed, #session_state{session = Session, heartbeat_ref = H} = State) ->
    lager:debug("disconnected, closed~n"),
    case Session#session_settings.reset_on_disconnect of
	true ->
	    set_sequence_number(1);
	_ -> void
    end,

    stop_heartbeat(H),
    {next_state, disconnected, State}.

connected({error, _Reason}, State) ->
    lager:debug("connected, error~n"),
    {next_state, disconnected, State};

%%We are connected and send the login message
connected(_, #session_state{session = Session, socket = Socket} = State) ->
    lager:error("connected~n"),
    %%Send logon
    Seq = get_sequence_number(Session#session_settings.session_id),
    I = Session#session_settings.heartbeat_interval,
%    H = start_heartbeat(I),
    Logon = message_utils:create_logon(<<"1">>,Session),
    io:format("Logon = ~s~n",[Logon]),
    try gen_tcp:send(Socket, Logon) of
	{error, Reason} ->
	    lager:error("Error logging in ~p~n",[Reason]);
	ok ->
	    lager:error("Sent logon~n")
    catch
	Ex ->
	    lager:error("Error sending login~p~n",[Ex])
    end,
    case gen_tcp:recv(Socket, 0, 10000) of
	{ok,D} ->
	    lager:error("Data received ~p~n",[D]);
	Other ->
	    lager:error("Other received ~p~n",[Other])
    end,
    {next_state, logged_on, State#session_state{last_sequence_number = Seq}}.

logged_on({error, _Reason}, State) ->
    {next_state, disconnected, State};

%%We are logged on, start the heartbeat timer and wait for data
logged_on(_, State) ->
    {next_state, receiving, State}.

%%We are logged off so we connect again
logged_off(_, State) ->
    {next_state, connect, State}.

receiving(error, State) ->
    lager:error("receiving, error~n"),
    {next_state, connect, State};

receiving(send_heartbeat, #session_state{socket = Socket, last_sequence_number = Seq, session = Info} = State) ->
    lager:debug("sending heartbeat~n"),
    Version = Info#session_settings.begin_string,
    gen_tcp:send(Socket, create_heartbeat_message(Seq+1, Version)),
    {next_state, receiving, State};

%%We are receiving data, all is well
receiving(Data, #session_state{session = Settings,admin_handler = Admin, msg_handler = Msg} = State) ->
    lager:debug("receiving data ~p~n",[Data]),
    save_message(Settings,Data),
    Fix_Version = Settings#session_settings.begin_string,
    TagValueList = binary:split(?SOH, Data),
    lager:debug("Msg ~p~n",[TagValueList]),
    <<"8=",Fix_Version,?SOH,"35=",Rest>> = Data,
    TagsAndValues = re:split(Rest,<<"=">>),
    Type = hd(TagsAndValues),
    case is_admin_message(binary_to_list(Type)) of
	true ->
	    Admin(Data);
	false ->
	    Msg(Data)
    end,
    {next_state, receiving, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({tcp, Socket, Data}, StateName, #session_state{socket=Socket} = State) ->
    lager:debug("handle info, data ~p~n",[Data]),
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Data}, State);

handle_info({tcp_closed, Socket}, _StateName, #session_state{socket=Socket} = StateData) ->
    lager:debug("handle info, closed~n"),
    error_logger:info_msg("~p Client disconnected.\n", [self()]),
    {stop, normal, StateData};

handle_info(connect, StateName, State) ->
    lager:debug("handle info, connect~n"),
    ?MODULE:StateName(connect, State);

handle_info(Info, StateName, StateData) ->
    lager:debug("handle info, ~p, ~p~n",[Info, StateName]),
    {noreply, StateName, StateData}.

terminate(_Reason, _StateName, #session_state{socket = Socket, heartbeat_ref = H}) ->
    (catch gen_tcp:close(Socket)),
    stop_heartbeat(H),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_sequence_number(SessionID) ->
    case erlang:get(seq) of
	undefined -> get_seq_from_file(SessionID);
	Seq when is_integer(Seq) ->
	    Seq
    end.

set_sequence_number(Seq) ->
    erlang:put(seq, Seq).

%% save_seq_to_file(SessionID) ->
%%     Seq = get_sequence_number(SessionID),
%%     File = SessionID++".seq",
%%     file:write_file(File,list_to_binary(integer_to_list(Seq))).

get_seq_from_file(SessionID) ->
    File = SessionID++".seq",
    case file:read_file(File) of
	{ok, S} ->
	    list_to_binary(list_to_integer(S));
	{error, Reason} ->
	    lager:error("Error reading seq file ~p~n",[Reason]),
	    list_to_binary(integer_to_list(1))
    end.

create_heartbeat_message(Seq, _Protocol) ->
    Seq.

-spec start_heartbeat(integer()) -> any().
start_heartbeat(Interval) ->
    {ok, Ref} = timer:apply_interval(Interval, ?MODULE, send_heartbeat, [self()]),
    Ref.

stop_heartbeat(Pid) ->
%    exit(Pid,normal),
    ok.

%% -spec send_heartbeat(pid()) -> ok.
%% send_heartbeat(Self) ->
%%     Self ! send_heartbeat.

save_message(_Settings, _Msg) ->
    ok.

is_admin_message(MsgType) ->
    lists:any(fun(X) ->
		      X == MsgType
	      end, ["0","A","1","1","2","3","4","5"]).    
