
-module(quickfix_erl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-include("../include/session_records.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Sessions = session_utils:get_sessions(),
    Sups = lists:map(fun(S) ->
			     Days = S#session_settings.days,
			     Start = S#session_settings.start_time,
			     End = S#session_settings.end_time,
			     {timed_supervisor,{timed_supervisor, start_link,
						[timed_supervisor,{fix_session,start_link,[S]},
						 [{schedule, [{Days,
							       [{Start,End}]}
							     ]
						  }
,
						  {shutdown, brutal_kill}
						 ]
						]
					       },permanent, 5000, supervisor, [timed_supervisor]}
		     end,Sessions),
    {ok, { {one_for_one, 5, 10}, Sups} }.
