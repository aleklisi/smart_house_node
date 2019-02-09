%%%-------------------------------------------------------------------
%% @doc smart_house_node top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(smart_house_node_sup).

-behaviour(supervisor).

-include("hrl/sensor_params.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_all, 0, 1}, sensors()}}.

%%====================================================================
%% Internal functions
%%====================================================================

sensors() ->
	[sensor()].

sensor() ->
	#{id => test_sensor,
	  start => {sensor, start_link, [
          #{
            ?MEASUREMENT_NAME => test_sensor,
            ?REPEAT_AFTER => 3000,
            ?MEASUREMENT_FUN => fun(Max) -> rand:uniform(Max) end,
            ?MEASUREMENT_FUN_ARGS => [70]
          }
      ]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker}.
