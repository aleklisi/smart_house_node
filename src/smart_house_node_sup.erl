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
	[test_sensor:config(#{}),
    cpu_temperature:config(#{?REPEAT_AFTER => 1000})].
