%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created :  8 Nov 2011 by  <>
%%%-------------------------------------------------------------------
-module(fix_session).

-behaviour(gen_fsm).
-include("../include/session_records.hrl").
%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


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
-spec start_link(#session_settings{},fun(), fun()) -> ok.
start_link(SessionSettings,HandleAdminMsg, HandleMsg) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE,[SessionSettings,HandleAdminMsg,HandleMsg],[]).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%Initializes the state with session info and starts connecting
init([#session_settings{data_dictionary = Dict, session_id = Id}= Session_Settings,HandleAdmin,HandleMsg]) ->
    PrivDir = code:priv_dir(quickfix_erl),
    io:format("~p~n",[PrivDir]),
    session_utils:parse_dict(Dict,PrivDir++"spec.xsd"),
    self() ! connect,
    {ok, disconnected, #session_state{session = Session_Settings,admin_handler = HandleAdmin, msg_handler = HandleMsg}}.

%%Initial connection
disconnected(connect, #session_state{socket = Socket, session = Session} = State) when Socket == undefined ->
    io:format("disconnected, with tcp = undefined~n"),
    %%Create TCP and move on
    IP = Session#session_settings.socket_connect_host,
    Port = Session#session_settings.socket_connect_port,
    case gen_tcp:connect(IP,Port,[{packet,2}]) of
	{ok,S} ->
	    {next_state, connected, State#session_state{socket = S}};
	{error, E} ->
	    io:format("Error connecting ~p, doing failover~n",[E]),
	    ?MODULE:disconnected(failover, State)
    end;

%%Connect failed
disconnected(failover, #session_state{session=Session} =  State) ->
    io:format("disconnected, failover~n"),
    IP = Session#session_settings.socket_failover_host,
    Port = Session#session_settings.socket_failover_port,
    case Session#session_settings.reset_on_disconnect of
	true ->
	    set_sequence_number(1);
	_ -> void
    end,
    case gen_tcp:connect(IP,Port,[{packet,2}]) of
	{ok,S} ->
	    {next_state, connected, State#session_state{socket = S}};
	{error, E} ->
	    io:format("Error connecting ~p, doing disaster~n",[E]),
	    ?MODULE:disconnected(disaster, State)
    end;

%%Failover didn't manage to connect
disconnected(disaster, #session_state{session=Session} =  State) ->
    io:format("disconnected, disaster~n"),
    IP = Session#session_settings.socket_disaster_host,
    Port = Session#session_settings.socket_disaster_port,
    case Session#session_settings.reset_on_disconnect of
	true ->
	    set_sequence_number(1);
	_ -> void
    end,
    case gen_tcp:connect(IP,Port,[{packet,2}]) of 
	{ok,S} -> 
	    {next_state, connected, State#session_state{socket = S}};
	{error, E} ->
	    io:format("Error connecting ~p, back to main~n",[E]),
	    ?MODULE:disconnected(connect, State)
    end;

%%The connection is closed with the market
disconnected(closed, #session_state{session = Session, heartbeat_ref = H} = State) ->
    io:format("disconnected, closed~n"),
    case Session#session_settings.reset_on_disconnect of
	true ->
	    set_sequence_number(1);
	_ -> void
    end,

    stop_heartbeat(H),
    {next_state, disconnected, State}.

connected({error, _Reason}, State) ->
    io:format("connected, error~n"),
    {next_state, disconnected, State};

%%We are connected and send the login message
connected(_, #session_state{session = Session} = State) ->
    io:format("connected~n"),
    %%Send logon
    Seq = get_sequence_number(Session#session_settings.session_id),
    I = Session#session_settings.heartbeat_interval,
    H = start_heartbeat(I),
    {next_state, logged_on, State#session_state{last_sequence_number = Seq, heartbeat_ref = H}}.

logged_on({error, _Reason}, State) ->
    {next_state, disconnected, State};

%%We are logged on, start the heartbeat timer and wait for data
logged_on(_, State) ->
    {next_state, receiving, State}.

%%We are logged off so we connect again
logged_off(_, State) ->
    {next_state, connect, State}.

receiving(error, State) ->
    io:format("receiving, error~n"),
    {next_state, connect, State};

receiving(send_heartbeat, #session_state{socket = Socket, last_sequence_number = Seq, session = Info} = State) ->
    io:format("sending heartbeat~n"),
    Version = Info#session_settings.begin_string,
    gen_tcp:send(Socket, create_heartbeat_message(Seq+1, Version)),
    {next_state, receiving, State};

%%We are receiving data, all is well
receiving(Data, #session_state{session = Settings,admin_handler = Admin, msg_handler = Msg} = State) ->
    io:format("receiving data ~p~n",[Data]),
    save_message(Settings,Data),
    Fix_Version = Settings#session_settings.begin_string,
    TagValueList = binary:split(?SOH, Data),
    io:format("Msg ~p~n",[TagValueList]),
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
    io:format("handle info, data~n"),
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Data}, State);

handle_info({tcp_closed, Socket}, _StateName, #session_state{socket=Socket} = StateData) ->
    io:format("handle info, closed~n"),
    error_logger:info_msg("~p Client disconnected.\n", [self()]),
    {stop, normal, StateData};

handle_info(connect, StateName, State) ->
    io:format("handle info, connect~n"),
    ?MODULE:StateName(connect, State);

handle_info(Info, StateName, StateData) ->
    io:format("handle info, ~p, ~p~n",[Info, StateName]),
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
	    binary_to_list(list_to_integer(S));
	{error, Reason} ->
	    io:format("Error reading seq file ~p~n",[Reason]),
	    1
    end.

create_heartbeat_message(Seq, _Protocol) ->
    Seq.

-spec start_heartbeat(integer()) -> any().
start_heartbeat(Interval) ->
    {ok, Ref} = timer:apply_interval(Interval, ?MODULE, send_heartbeat, [self()]),
    Ref.

stop_heartbeat(Pid) ->
    exit(Pid,normal),
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

%% is_session_time(#session_settings{start_day = SD, start_time = ST, end_day = ED, end_time = ET}) ->
%%     true.
    
