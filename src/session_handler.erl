-module(session_handler).
-export([behaviour_info/1]).

%% @private
-spec behaviour_info(_)
	-> undefined | [ {handle_app_msg, 2} | {handle_admin_msg,2} | {init, 3} | {terminate, 2}, ...].
behaviour_info(callbacks) ->
	[{init, 3}, {handle_app_msg, 2},{handle_admin_msg,2}, {terminate, 2}];
behaviour_info(_Other) ->
	undefined.
