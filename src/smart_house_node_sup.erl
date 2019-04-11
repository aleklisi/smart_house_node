%%%-------------------------------------------------------------------
%% @doc smart_house_node top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(smart_house_node_sup).

-behaviour(supervisor).

-include("hrl/sensor_params.hrl").

-export([start_link/0]).

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

init([]) ->
    {ok, {#{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    }, sensors()}}.

%%====================================================================
%% Internal functions
%%====================================================================

sensors() ->
	[
        test_sensor:config(#{}),
        cpu_temperature:config(#{?REPEAT_AFTER => 1000}),
        serial_sup:config()
    ].
