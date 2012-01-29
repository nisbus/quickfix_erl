%%%-------------------------------------------------------------------
%%% @author nisbus  <nisbus@gmail.com>
%%% @copyright (C) nisbus 2012, 
%%% @doc
%%%  Module for composing session id's and reading configurations.
%%% @end
%%% Created : 25 Jan 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(session_utils).
-include("../include/session_records.hrl").

%% API
-export([compose_session_id/1,get_sessions/0,parse_dict/2, parse_fix_msg/2,test4/0,test/0,test2/0]).

%%%===================================================================
%%% API
%%%===================================================================

compose_session_id(#session_settings{begin_string = B, sender_comp_id = SC, sender_sub_id = SS, sender_location_id = SL, target_comp_id = TC, target_sub_id = TS, target_location_id = TL, session_qualifier = SQ} = _SessionSettings) ->
    ID = B++":"++SC,
    ID0 = append_defined(ID, [SS,SL]) ++ "->"++TC,
    ID1 = append_defined(ID0, [TS,TL]),
    case SQ of
	"" -> ID1;
	_ -> ID1++":"++SQ
    end.

%%@Doc Read the session settings from the app.conf
get_sessions() ->
    Sessions = application:get_env(sessions),
    lists:map(fun(Session) ->
		   S = #session_settings{
		     begin_string = proplists:get_value(Session,begin_string),
		     sender_comp_id = proplists:get_value(Session,sender_comp_id),
		     sender_sub_id = proplists:get_value(Session,sender_sub_id),
		     sender_location_id = proplists:get_value(Session,sender_location_id),
		     target_comp_id = proplists:get_value(Session,target_comp_id),
		     target_sub_id = proplists:get_value(Session,target_sub_id),
		     target_location_id = proplists:get_value(Session,target_location_id),
		     session_qualifier = proplists:get_value(Session,session_qualifier),
		     default_appver_id = proplists:get_value(Session,default_appver_id),
		     connection_type = proplists:get_value(Session,connection_type),
		     use_data_dictionary = proplists:get_value(Session,use_data_dictionary,false),
		     use_local_time = proplists:get_value(Session,use_local_time,false),
		     start_day = proplists:get_value(Session,start_day,"mon"),
		     start_time = proplists:get_value(Session,start_time),
		     end_day = proplists:get_value(Session,end_day,"fri"),
		     end_time = proplists:get_value(Session,end_time),
		     max_latency = proplists:get_value(Session,max_latency),
		     heartbeat_interval = proplists:get_value(Session,heartbeat_interval,60),
		     socket_connect_host = proplists:get_value(Session,socket_connect_host),
		     socket_connect_port = proplists:get_value(Session,socket_connect_port),
		     socket_failover_host = proplists:get_value(Session,socket_failover_host),
		     socket_failover_port = proplists:get_value(Session,socket_failover_port),
		     socket_disaster_host = proplists:get_value(Session,socket_disaster_host),
		     socket_disaster_port = proplists:get_value(Session,socket_disaster_port),
		     reconnect_interval = proplists:get_value(Session,reconnect_interval,60),
		     refresh_on_logon = proplists:get_value(Session,refresh_on_logon, false),
		     reset_on_logon = proplists:get_value(Session,reset_on_logon, false),
		     reset_on_logout = proplists:get_value(Session,reset_on_logout, false),
		     reset_on_disconnect = proplists:get_value(Session,reset_on_disconnect, false),
		     data_dictionary = proplists:get_value(Session,data_dictionary),
		     logon_timeout = proplists:get_value(Session, logon_timeout, 60),
		     logout_timeout = proplists:get_value(Session, logout_timeout, 60),
		     send_redundant_resend_requests = proplists:get_value(Session,send_redundant_resend_requests, false),
		     milliseconds_in_timestamp = proplists:get_value(Session,milliseconds_in_timestamp, false)
		    },
		      S#session_settings{session_id = compose_session_id(S)}
	      end, Sessions).

-spec parse_dict({string(),string()}, string) -> {integer(),integer(),
						  [#field{}],[#message{}],
						  [#field{}],[#field{}]|undefined,
						  [#field{}]}. 
parse_dict(XMLFile, XSDFile) ->
    {ok, F} = file:read_file(XSDFile),
    {ok,Model} = erlsom:compile_xsd(binary_to_list(F)),
    {ok, Out, _Rest} = erlsom:scan_file(XMLFile, Model),
    case Out of
	{fix,[],Maj,Min,_Rev,_Type,{header,[],Header}, {messages,[],Msgs}, {trailer,[],Trailer},_Comp,Fields} ->    
	    PF = parse_fields(Fields),
	    PM = parse_messages(Msgs),
	    HF = parse_fields(Header),
	    TF = parse_fields(Trailer),
	    PM0 = fix_message_fields(PM,PF),
	    HF0 = fix_fields(HF,PF),
	    TF0 = fix_fields(TF,PF),
	    {Maj,Min,HF0,PM0,TF0,_Comp,PF};
	Other ->
	    io:format("Invalid spec ~p~n",[Other]),
	    {0,0,[],[],[],[],[]}
    end.

get_messages(T,Header,Trailer) ->
    lists:flatten([[T#message.fields|Header]|Trailer]).
    
get_msg_type(FIX,Messages) ->
    <<"8=FIX.",_Maj:1/binary,".",_Min:1/binary,?SOH,Rest/binary>> = FIX,
    KV = binary:split(Rest,?SOHB,[global]),
    KeyVals = [X || X <- KV, X =/= <<>>],
    [_|R] = KeyVals,
    Tp = hd(R),
    T0 = binary:split(Tp,<<"=">>),
    [Type] = tl(T0),
    [M] = [X || #message{tag = T} = X <- Messages, T =:= Type],
    M.

parse_fix_msg(FIX, Message) ->
    <<"8=FIX.",_Maj:1/binary,".",_Min:1/binary,?SOH,Rest/binary>> = FIX,
    KV = binary:split(Rest,?SOHB,[global]),
    KeyVals = [X || X <- KV, X =/= <<>>],
    I = lists:map(fun(B) ->		      
			  [Key,Value] = binary:split(B,<<"=">>),			  
			  case get_field_from_tag(Key,Message) of
			      {ok, M} ->
				  case M#field.is_group of
				      true ->
					  io:format("Found group field ~p~n",[M#field.name]);
				      false ->
					  void
				  end,
				  case M#field.values of
				      undefined -> {M#field.name, Value};
				      Values ->
					  case get_value_from_val(Value,Values) of
					      {ok, X} -> 
						  {M#field.name,X#value.name};
					      {error,_Reason} ->
%						  throw({error, Reason, Value})
						  {M#field.name, Value}
					  end
				  end;
			      {error, _R} -> 
				  {Key, Value}
%				  throw({error, R, Key})
			  end
		  end,KeyVals),
    I.


    
get_value_from_val(Value,Values) ->
    case [Val || #value{value=V} = Val <- Values, V =:= Value] of
	[] ->
	    {error, value_not_found};
	[X] ->
	    {ok, X}
    end.

get_field_from_tag(Tag,Fields) ->
    case [M || #field{tag =T} = M <- Fields, T =:= Tag] of
	[] ->
	    {error, field_not_found};
	[U] ->
	    {ok, U}
    end.

%%%===================================================================================
%%% Internal Functions for generating lookups
%%%===================================================================================

fix_fields(undefined, _) ->
    undefined;
fix_fields([#field{} = F|[]], Fields) ->
    F2 = lookup_field([F],Fields),
    [fix_field(F,F2,Fields)];
fix_fields(MF ,Fields) ->
    lists:map(fun(F) ->
		      F2 = lookup_field([F],Fields),
		      fix_field(F,F2,Fields)
	      end,MF).

fix_field(MF, #field{tag = Tag, is_group=IsGroup, is_group_member = GM, values = Values, type = Type, fields = Fields}, Lookup) ->
    MF0 = case Tag of
	      undefined -> MF;
	      _ -> MF#field{tag = Tag}
	  end,
    MF1 = case IsGroup of
	      undefined -> MF0;
	      _ -> MF0#field{is_group = IsGroup}
	  end,
    MF2 = case GM of
	      undefined ->
		  MF1;
	      _ -> MF1#field{is_group_member = GM}
	  end,
    MF3 = case Values of
	      undefined -> MF2;
	      _ -> MF2#field{values = Values}
	  end,
    MF4 = case Type of
	      undefined ->MF3;
	      _ -> MF3#field{type = Type}
	  end,		  
    case Fields of
	undefined -> MF4;
	_ -> MF4#field{fields = fix_fields(Fields,Lookup)}
    end.
	    
fix_message_fields(Messages,Fields) ->
    lists:map(fun(M) ->
		      F = M#message.fields,
		      M#message{fields = fix_fields(F,Fields)}
	      end,Messages).

lookup_field([{field, Name,_Tag,_Req,_G,_GM,_F,_T,_V}|[]], Fields) ->
    [Found] = [X || X = #field{name = N} <- Fields, Name =:= N],
    Found;
lookup_field([_H|_T] = FieldList, Fields) ->
    lists:map(fun(X) ->
		      lookup_field([X],Fields)
	      end,FieldList).

parse_fields(undefined) ->
    undefined;
parse_fields(Fields) when is_list(Fields)->
    lists:map(fun(X) ->
		      parse_field(X)
	      end,Fields);
parse_fields({fields, [], Fields}) ->
    lists:map(fun(X) ->
		      parse_field(X)
	      end,Fields).
parse_values(undefined) ->
    undefined;
parse_values(List) ->
    lists:map(fun(X) ->
		      parse_value(X)
	      end,List).
parse_messages(Msgs) ->
    lists:map(fun(X) ->
		      parse_message(X)
	      end,Msgs).

parse_field({group,[],Name,Req,Fields}) ->
    #field{name = to_atom(Name), is_required = to_bool(Req), is_group = true, fields = parse_fields(Fields)};
parse_field({group,[],Name,Tag,Req,Type,Fields}) -> 
    #field{name = to_atom(Name), tag = to_binary(Tag), is_required = to_bool(Req), is_group = true, fields = parse_fields(Fields), type = Type};
parse_field({field,[],Name,Tag,"Y",Type,Fields}) ->
    #field{name = to_atom(Name), tag = to_binary(Tag), is_required = true, is_group = false, fields = parse_fields(Fields),type = Type};
parse_field({field,[],Name,Tag,"N",Type,Fields}) ->
    #field{name = to_atom(Name), tag = to_binary(Tag), is_required = false, is_group = false, fields = parse_fields(Fields), type = Type};
parse_field({field,[],Name,Tag,Req,Type,Values}) ->
    #field{name = to_atom(Name), tag = to_binary(Tag), is_required = to_bool(Req), is_group = false, values = parse_values(Values), type = Type}.

parse_value({value,[],Name, Tag}) ->
    #value{name = to_binary(string:to_lower(Name)), value = to_binary(Tag)}.
parse_message({message,[],IsAdminMessage,Tag,Name,Fields}) ->
    #message{name = to_atom(Name), tag = to_binary(Tag), is_admin = is_admin(IsAdminMessage), fields = parse_fields(Fields)}.
    
to_bool("Y") ->
    true;
to_bool(undefined) ->
    undefined;
to_bool(_) ->
    false.

is_admin("admin") ->
    true;
is_admin(_) ->
    false.

to_binary(undefined) ->
    undefined;
to_binary(Tag) when is_list(Tag) ->
    list_to_binary(Tag);
to_binary(Tag) when is_integer(Tag) ->
    list_to_binary(integer_to_list(Tag));
to_binary(Tag) when is_binary(Tag) ->
    Tag.

to_atom(Name) when is_list(Name) ->
    list_to_atom(string:to_lower(Name));
to_atom(Name) when is_binary(Name) ->
    to_atom(binary_to_list(Name));
to_atom(Name) when is_atom(Name) ->
    Name.

append_defined(ID, Rest) ->
   lists:foldl(fun(Tup, Acc) ->
		       case Tup of
			   "" -> Acc;
			   undefined -> Acc;
			   D1 -> Acc ++"/"++D1
			end
		  end,ID, Rest).

%%%===================================================================================
%%% Internal Functions for parsing FIX messages
%%%===================================================================================

%% print_spec(PF,PM,HF,TF) ->
%%     	    io:format("===============================================================~n=====================HEADER====================================~n===============================================================~n~p~n",[HF]),
%% 	    io:format("===============================================================~n===================MESSAGES====================================~n===============================================================~n~p~n",[PM]),
%% 	    io:format("===============================================================~n====================TRAILER====================================~n===============================================================~n~p~n",[TF]),
%% 	    io:format("===============================================================~n=====================FIELDS====================================~n===============================================================~n~p~n",[PF]).

test4() ->
    parse_dict({"/home/nisbus/code/erlang/quickfix_erl/FIX42.xml","/home/nisbus/code/erlang/quickfix_erl/FIX42.xsd"},"SESSION").

test() ->
    FIX = <<"8=FIX.4.29=026835=834=114152=20110428-10:07:2249=INORD50=S56=Y4857=Y48016=0.011=KODCS75UO3X5314=015=ISK17=GPCFD1-2246118=N20=037=2240738=10039=040=244=51.1000528=A48=IS000000038854=155=548259=060=20110428-10:07:22150=0151=1009140=Y5815=23109=Y4876=BOOK10=125">>,
    case catch parse_fix_msg(FIX, #session_settings{session_id = undefined, data_dictionary = undefined}) of
	{error, Reason, Value} ->
	    io:format("~p ~p~n",[Reason,Value]);
	X ->
	    X
    end.

test2() ->
    File = "/home/nisbus/code/erlang/quickfix_erl/fixlog.txt",
     {_Major,_Minor,Header,Messages,Trailer,_Comp,_Fields}  = parse_dict("/home/nisbus/code/erlang/quickfix_erl/FIX42.xml","/home/nisbus/code/erlang/quickfix_erl/FIX42.xsd"),
    
    F = fun(X,_) -> 
		Split = binary:split(list_to_binary(X),<<10>>),
		Msg = hd(Split),
		M = get_msg_type(Msg,Messages),
		T = get_messages(M,Header,Trailer),
		parse_fix_msg(Msg,T)
	end,
    for_each_line_in_file(File, F,[read], 0).

for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> NewAccum = Proc(Line, Accum),
                    for_each_line(Device, Proc, NewAccum)
    end.
