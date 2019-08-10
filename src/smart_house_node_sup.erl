-module(smart_house_node_sup).

-author('alek.lisiecki@gmail.com').

-behavior(supervisor).

-include("hrl/sensor_params.hrl").

-export([start_link/1]).
-export([init/1]).

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

init(Config) ->
    Children = lists:map(fun make_child/1, Config),
    {ok, {#{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    }, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

make_child(Config = {serial, _, _, _}) ->
    serial_sup:config(Config);
make_child(Config = {active_sensors, _}) ->
    active_sensors_sup:config(Config).
