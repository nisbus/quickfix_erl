%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) nisbus 2012, 
%%% @doc
%%%
%%% @end
%%% Created : 31 Jan 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(message_store).
%% API
-export([initialize/1, save_msg/4, save_seq/3, get_last_in_seq/1, get_last_out_seq/1,get_messages/4]).

-record(msg, 
	{
	  timestamp,
	  direction,
	  session_id,
	  sequence_number,
	  message
	}).
-record(seq,
	{
	  direction,
	  session_id,
	  number
	}).
%%%===================================================================
%%% API
%%%===================================================================
-spec initialize(atom()|string()) -> {atomic, ok} | {aborted, string()}.
initialize(SessionId) when is_atom(SessionId) ->    
    initialize(atom_to_list(SessionId));

initialize(SessionId) when is_list(SessionId) -> 
    mnesia:create_table(msg,  [{attributes, record_info(fields,msg)}]),
    mnesia:create_table(seq,  [{attributes, record_info(fields,seq)}]).

-spec save_msg(atom(),atom(),integer(), binary()) -> {atomic, any()} | {aborted, any()}.
save_msg(SessionId,Direction,Seq,Msg) when is_atom(SessionId) ->
    save_msg(atom_to_list(SessionId),Direction,Seq, Msg);
save_msg(SessionId,Direction,Seq, Msg) when is_list(SessionId) ->
    lager:debug("Saving message ~p,~p,~p",[SessionId,Direction,Seq]),
    Table = list_to_atom(SessionId++"messages"),
    Timestamp = get_timestamp(),
    Trans = fun() ->      
		    mnesia:write(#msg{session_id = Table,direction = Direction,sequence_number = to_integer(Seq),timestamp = Timestamp, message = Msg})	    
    end,
    mnesia:transaction(Trans),
    save_seq(SessionId,Direction,to_integer(Seq)).

save_seq(SessionId, Direction,Seq) when is_atom(SessionId) ->
    save_seq(atom_to_list(SessionId), Direction, Seq);
save_seq(SessionId, Direction,Seq) when is_list(SessionId) ->
    Table = list_to_atom(SessionId++"seq"),
    S = to_integer(Seq),
    Trans = fun() ->
		    mnesia:write(Table,#seq{direction = Direction, number = S})
	    end,
    mnesia:transaction(Trans).

    


get_last_in_seq(SessionId) when is_atom(SessionId) ->
    get_last_in_seq(atom_to_list(SessionId));
get_last_in_seq(SessionId) when is_list(SessionId) ->
    Table = list_to_atom(SessionId++"seq"),
    Trans = fun() ->
		    MatchHead = #seq{direction=in, session_id = Table, number='$1'},
		    mnesia:select(seq,[{MatchHead, [], ['$1']}])
	    end, 
    {atomic, S} = mnesia:transaction(Trans),
    case S of
	[] -> 1;
	_ -> tl(S)
    end.

get_last_out_seq(SessionId) when is_atom(SessionId) ->
    get_last_out_seq(atom_to_list(SessionId));
get_last_out_seq(SessionId) ->
    Table = list_to_atom(SessionId++"seq"),
    Trans = fun() ->
		    MatchHead = #seq{direction=out, session_id = Table, number='$1'},
		    mnesia:select(seq,[{MatchHead, [], ['$1']}])
	    end, 
    {atomic, S} = mnesia:transaction(Trans),
    case S of
	[] -> 1;
	_ -> tl(S)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_timestamp() ->
    S = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {_,_,M} = erlang:now(),
    list_to_float(integer_to_list(S)++"."++integer_to_list(M)).

get_messages(SessionId, Direction, From, To) ->
    Table = list_to_atom(SessionId++"msg"),
    Trans = fun() ->
		    MatchHead = #msg{direction=Direction, session_id = Table, sequence_number='$1'},
		    Guard = [{'>=', '$1', to_integer(From)},{'<=','$1',to_integer(To)}],
		    mnesia:select(msg,[{MatchHead, Guard, ['$1']}])
	    end, 
    {atomic, S} = mnesia:transaction(Trans),
    S.

to_integer(S) when is_integer(S) ->
    S;
to_integer(S) when is_binary(S) ->
    to_integer(binary_to_list(S));
to_integer(S) when is_list(S) ->
    list_to_integer(S).
