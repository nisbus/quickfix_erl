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
-export([create_msg/2, create_logon/1, msg_to_proplist/2]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
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

create_logon(#session_settings{begin_string = BS,sender_comp_id = Sender, target_comp_id = Target, heartbeat_interval = Heartbeat,reset_on_logon=Reset}) ->
    Start = <<"8=",BS/binary>>,
    Time = create_timestamp(),
    Type = <<"34=A">>,
    SendTag = <<"49=">>,
    TimeTag = <<"52=">>,
    TargetTag = <<"56=">>,
    EncodingTag = <<"98=0">>,
    HTag = <<"108=">>,
    ResetTag = <<"141=">>,
    Msg = <<Type/binary,?SOHB/binary,SendTag/binary,Sender/binary,?SOHB/binary,TimeTag/binary,Time/binary,?SOHB/binary,TargetTag/binary,Target/binary,?SOHB/binary,EncodingTag/binary,?SOHB/binary,HTag/binary,Heartbeat/binary,?SOHB/binary,ResetTag/binary,Reset/binary,?SOHB/binary>>,
    L = bodylength(Msg),
    LengthTag = <<"9=">>,
    Msg0 = <<Start/binary,LengthTag/binary,L/binary,?SOHB/binary,Msg/binary>>,
    C = checksum(Msg0),
    CTag = <<"10=">>,
    <<Msg0/binary,CTag/binary,C/binary>>.


create_timestamp() ->
    {{Y,M,D},{H,Min,Sec}} = erlang:localtime_to_universaltime(erlang:localtime()),
    {_,_,Mill} = erlang:now(),				      
    Y0 = integer_to_list(Y),
    M0 = string:right(integer_to_list(M),2,$0),
    D0 = string:right(integer_to_list(D),2,$0),
    DS = "-",
    H0 = string:right(integer_to_list(H),2,$0),
    TS = ":",
    Min0 = string:right(integer_to_list(Min),2,$0),    
    Sec0 = string:right(integer_to_list(Sec),2,$0),
    Mil0 = string:right(integer_to_list(Mill),3,$0),
    list_to_binary(Y0++M0++D0++DS++H0++TS++Min0++TS++Sec0++"."++Mil0).

    
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
checksum(<<>>,Acc) -> list_to_binary(integer_to_list((Acc rem 256))).

bodylength(Msg) ->
    list_to_binary(string:right(integer_to_list(length(binary_to_list(Msg))),4,$0)).    

to_binary(Key) when is_binary(Key) ->
    Key;
to_binary(Key) when is_list(Key) ->
    list_to_binary(Key);
to_binary(Key) when is_atom(Key) ->
    to_binary(atom_to_list(Key)).

-ifdef(TEST).

create_timestamp_test() ->
    TS = create_timestamp(),
    io:format(user,"Timestamp ~p~n",[TS]).

create_logon_test() ->
    S = #session_settings{begin_string = <<"FIX.4.2">>,sender_comp_id = <<"CLI_44D">>, target_comp_id = <<"KYTEUAT_EBP">>, heartbeat_interval = list_to_binary(integer_to_list(60)),reset_on_logon= <<"Y">>},
    create_logon(S).

checksum_test() ->
    M = <<51,52,61,65,1,52,57,61,67,76,73,95,52,52,68,1,53,50,61,50,48,49,50,48,49,50,57,45,50,51,58,50,57,58,51,53,46,50,49,48,1,53,54,61,75,89,84,69,85,65,84,95,69,66,80,1,57,56,61,48,1,49,48,56,61,54,48,1,49,52,49,61,89,1>>,
    C = checksum(M),
    io:format(user,"Check ~p~n",[C]).
    

-endif.
