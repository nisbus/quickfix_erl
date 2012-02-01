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
-export([compose_session_id/1,get_sessions/0,parse_dict/2, parse_fix_msg/2,to_list/1,get_msg_type/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

compose_session_id(#session_settings{begin_string = B, sender_comp_id = SC, sender_sub_id = SS, sender_location_id = SL, target_comp_id = TC, target_sub_id = TS, target_location_id = TL, session_qualifier = SQ} = _SessionSettings) ->
    
    ID = to_list(B)++":"++to_list(SC),
    ID0 = append_defined(ID, [to_list(SS),to_list(SL)]) ++ "->"++to_list(TC),
    ID1 = append_defined(ID0, [to_list(TS),to_list(TL)]),
    case SQ of
	"" -> ID1;
	_ -> ID1++":"++to_list(SQ)
    end.

to_list(Item) when is_binary(Item) ->
    binary_to_list(Item);
to_list(undefined) ->
    "";
to_list(Item) when is_list(Item) ->
    Item.

%%@Doc Read the session settings from the app.conf
get_sessions() ->
    {ok,Sessions} = application:get_env(quickfix_erl,sessions),
    lists:map(fun(Session) ->
		      S = #session_settings{
			begin_string = proplists:get_value(begin_string,Session),
			sender_comp_id = proplists:get_value(sender_comp_id,Session),
			sender_sub_id = proplists:get_value(sender_sub_id,Session),
			sender_location_id = proplists:get_value(sender_location_id,Session),
			target_comp_id = proplists:get_value(target_comp_id,Session),
			target_sub_id = proplists:get_value(target_sub_id,Session),
			target_location_id = proplists:get_value(target_location_id,Session),
			session_qualifier = proplists:get_value(session_qualifier,Session),
			default_appver_id = proplists:get_value(default_appver_id,Session),
			connection_type = proplists:get_value(connection_type,Session),
			use_data_dictionary = proplists:get_value(use_data_dictionary,Session,false),
			use_local_time = proplists:get_value(use_local_time,Session,false),
			days = proplists:get_value(days,Session),
			start_time = proplists:get_value(start_time,Session),
			end_time = proplists:get_value(end_time,Session),
			max_latency = proplists:get_value(max_latency,Session),
			heartbeat_interval = proplists:get_value(heartbeat_interval,Session,60),
			socket_connect_host = proplists:get_value(socket_connect_host,Session),
			socket_connect_port = proplists:get_value(socket_connect_port,Session),
			socket_failover_host = proplists:get_value(socket_failover_host,Session),
			socket_failover_port = proplists:get_value(socket_failover_port,Session),
			socket_disaster_host = proplists:get_value(socket_disaster_host,Session),
			socket_disaster_port = proplists:get_value(socket_disaster_port,Session),
			reconnect_interval = proplists:get_value(reconnect_interval,Session,60),
			refresh_on_logon = proplists:get_value(refresh_on_logon,Session, false),
			reset_on_logon = proplists:get_value(reset_on_logon,Session, false),
			reset_on_logout = proplists:get_value(reset_on_logout,Session, false),
			reset_on_disconnect = proplists:get_value(reset_on_disconnect,Session, false),
			data_dictionary = proplists:get_value(data_dictionary,Session),
			logon_timeout = proplists:get_value(logon_timeout,Session, 60),
			logout_timeout = proplists:get_value(logout_timeout,Session, 60),
			send_redundant_resend_requests = proplists:get_value(send_redundant_resend_requests,Session, false),
			milliseconds_in_timestamp = proplists:get_value(milliseconds_in_timestamp,Session, false)
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
	    HF0 = fix_fields(HF,PF),
	    TF0 = fix_fields(TF,PF),

	    PM0 = fix_message_fields(PM,PF,HF0,TF0),
	    {Maj,Min,PM0};
	Other ->
	    lager:error("Invalid spec ~p~n",[Other]),
	    {0,0,[],[],[],[],[]}
    end.

parse_fix_msg(FIX, Message) ->
    <<"8=FIX.",_Maj:1/binary,".",_Min:1/binary,?SOH,Rest/binary>> = FIX,
    KV = binary:split(Rest,?SOHB,[global]),
    KeyVals = [X || X <- KV, X =/= <<>>],
    lists:map(fun(B) ->		      
		      [Key,Value] = binary:split(B,<<"=">>),  
		      case get_field_from_tag(Key,Message#message.fields) of
			  {ok, M} ->
			      case M#field.is_group of
				  true ->
				      lager:debug("Found group field ~p~n",[M#field.name]);
				  false ->
				      void
			      end,
			      case M#field.values of
				  undefined -> 
				      T = M#field.type,
				      {M#field.name, translate_value(Value,T)};
				  Values ->
				      case get_value_from_val(Value,Values) of
					  {ok, X} ->
					      {M#field.name,X#value.name};
					  {error,_Reason} ->
					      %throw({error, Reason, Value})
					      {M#field.name, Value}
				      end
			      end;
			  {error, _R} -> 
			      {Key, Value}
			      %throw({error, R, Key})
		      end
	      end,KeyVals).

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

%%%===================================================================================
%%% Internal Functions for generating lookups
%%%===================================================================================
translate_value(Value, "INTEGER") ->
    list_to_integer(binary_to_list(Value));
translate_value(Value, "FLOAT") ->
    list_to_float(binary_to_list(Value));
translate_value(Value, "STRING") ->
    Value;
translate_value(Value, "QTY") ->
    translate_value(Value,"INTEGER");
translate_value(Value, "INT") ->
    translate_value(Value,"INTEGER");
translate_value(Value,"UTCTIMESTAMP") ->
    timestamp_to_string(Value);
translate_value(Value,_) ->    
    Value.

timestamp_to_string(<<Y:4/binary,MM:2/binary,DD:2/binary,"-",Time/binary>>) ->
    list_to_binary(binary_to_list(Y)++"-"++binary_to_list(MM)++"-"++binary_to_list(DD)++" "++binary_to_list(Time)).

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
	    
fix_message_fields(Messages,Fields,Header,Trailer) ->
    lists:map(fun(M) ->
		      F = M#message.fields,
		      M#message{fields = lists:flatten([[fix_fields(F,Fields)|Header]|Trailer])}
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

%% print_spec(PF,PM,HF,TF) ->
%%     	    io:format("===============================================================~n=====================HEADER====================================~n===============================================================~n~p~n",[HF]),
%% 	    io:format("===============================================================~n===================MESSAGES====================================~n===============================================================~n~p~n",[PM]),
%% 	    io:format("===============================================================~n====================TRAILER====================================~n===============================================================~n~p~n",[TF]),
%% 	    io:format("===============================================================~n=====================FIELDS====================================~n===============================================================~n~p~n",[PF]).

%%%===================================================================================
%%% Internal Functions for tests
%%%===================================================================================
-ifdef(TEST).

parse_file_test() ->
    {ok,TestPath} = file:get_cwd(),
    XML = TestPath++"/../FIX42.xml",
    XSD = TestPath++"/../priv/spec.xsd",
    File = TestPath++"/../fixlog.txt",
     {_Major,_Minor,Messages}  = parse_dict(XML,XSD),
    F = fun(X,_) -> 
		Split = binary:split(list_to_binary(X),<<10>>),
		Msg = hd(Split),
		T = get_msg_type(Msg,Messages),

		parse_fix_msg(Msg,T)
	end,
    for_each_line_in_file(File, F,[read], 0).

parse_dict_test() ->
    {ok,TestPath} = file:get_cwd(),
    XML = TestPath++"/../FIX42.xml",
    XSD = TestPath++"/../priv/spec.xsd",
    X = case parse_dict(XML,XSD) of
    	{_Major,_Minor,_Messages} ->
    	    true;
    	_ -> false
    end,
    ?assert(true == X).

for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> NewAccum = Proc(Line, Accum),
                    for_each_line(Device, Proc, NewAccum)
    end.
-endif.
