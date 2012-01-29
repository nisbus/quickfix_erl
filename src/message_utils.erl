%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) nisbus 2012, 
%%% @doc
%%%  Utilities for creating, validating and parsing messages
%%% @end
%%% Created : 27 Jan 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(message_utils).
-include("../include/session_records.hrl").
%% API
-export([create_msg/2, msg_to_proplist/2]).

%%%===================================================================
%%% API
%%%===================================================================
create_msg(Msg, S) when is_list(Msg) ->
    create_msg(lists:foldl(fun({Key,Value},Acc) ->
				   K = to_binary(Key),
				   V = to_binary(Value),
				      case Acc of
					  <<"">> -> <<K/binary,<<"=">>/binary,V/binary,?SOHB/binary>>;
					  _ -> 
					      <<Acc/binary,K/binary,<<"=">>/binary,V/binary,?SOHB/binary>>
				      end
			      end,<<"">>,Msg), S);
		      
create_msg(Msg, #session_settings{begin_string = B}) when is_binary(Msg) ->
    L = bodylength(Msg),
    LTag = <<"9=">>,
    Msg0 = <<<<"8=">>/binary,B/binary,?SOHB/binary,LTag/binary,L/binary,?SOHB/binary,Msg/binary>>,
    C = list_to_binary(integer_to_list(checksum(Msg0))),
    <<Msg0/binary,?SOHB/binary,LTag/binary,L/binary,?SOHB/binary,Msg/binary,<<"10=">>/binary,C/binary>>.

msg_to_proplist(Msg, _SessionId) ->
    KeyValue = binary:split(Msg, ?SOHB,[global]),
    lists:map(fun(KV) ->
		      [Key|Value] = binary:split(KV,<<"=">>,[global]),
		      {Key,Value}
	      end,KeyValue).		      
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
checksum(Data) when is_binary(Data) -> checksum(Data,0).
checksum(<<I,T/binary>>,Acc) -> checksum(T, I+Acc);
checksum(<<>>,Acc) -> (Acc rem 256).

bodylength(Msg) ->
    list_to_binary(string:right(integer_to_list(length(binary_to_list(Msg))),4,$0)).    

to_binary(Key) when is_binary(Key) ->
    Key;
to_binary(Key) when is_list(Key) ->
    list_to_binary(Key);
to_binary(Key) when is_atom(Key) ->
    to_binary(atom_to_list(Key)).
