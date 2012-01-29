-define(SOH, 16#01).
-define(SOHB, <<00000001>>).

-define(KEYTOTAG(Key,Prop),proplists:get_value(Prop,Key)).
-define(TAGTOKEY(Tag,Prop), proplists:get_key(Prop,Tag)). 

%%@doc 
%% field definition is composed from a standard QuickFix xml file and is composed of:
%%
%%Tag, Value, isGroupHeader, isGroupItem, GroupParent, isRequired
%%@end
%-type(field_definition,{string(), string(),boolean(),boolean(),string(),boolean()}).

-record(session_settings,
	{
	  session_id         :: string(),
	  begin_string       :: string()|undefined,
	  sender_comp_id     :: string() | undefined,
	  sender_sub_id      :: string() | undefined,
	  sender_location_id :: string() | undefined,
	  target_comp_id     :: string() | undefined,
	  target_sub_id      :: string() | undefined,
	  target_location_id :: string() | undefined,
	  session_qualifier  :: string() | undefined,
	  default_appver_id  :: string() | undefined,
	  connection_type    :: string() | undefined,
	  use_data_dictionary:: boolean() | undefined,
	  use_local_time     :: boolean() | undefined,
	  days               :: string() | undefined,
	  start_time         :: tuple() | undefined,
	  end_time           :: tuple() | undefined,
	  max_latency        :: integer() | undefined,
	  heartbeat_interval :: integer() | undefined,
	  socket_connect_host:: string() | undefined,
	  socket_connect_port:: integer() | undefined,
	  socket_failover_host:: string() | undefined,
	  socket_failover_port:: integer() | undefined,
	  socket_disaster_host:: string() | undefined,
	  socket_disaster_port:: integer() | undefined,
	  reconnect_interval :: integer() | undefined,
	  refresh_on_logon   :: boolean() | undefined,
	  reset_on_logon     :: boolean() | undefined,
	  reset_on_logout    :: boolean() | undefined,
	  reset_on_disconnect:: boolean() | undefined,
	  data_dictionary    :: string() | undefined,
	  logon_timeout      :: integer() | undefined,
	  logout_timeout     :: integer() | undefined,
	  send_redundant_resend_requests::boolean()|undefined,
	  milliseconds_in_timestamp::boolean|undefined,
	  data_dictonary     :: string()
	}).

-record(value,
	{
	  name            :: atom(),
	  value           :: binary()
	}).

-record(field,
	{
	  name            :: atom(),
	  tag             :: binary(),
	  is_required     :: boolean(),
	  is_group        :: boolean(),
	  is_group_member :: boolean(),
	  fields          :: [#field{}],
	  type            :: any(),
	  values          :: [#value{}] | undefined
	}).

-record(message,
	{
	  name     :: atom,
	  tag      :: binary(),
	  is_admin :: boolean(),
	  fields   :: [#field{}]
	}).
