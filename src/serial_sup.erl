-module(serial_sup).

-behaviour(supervisor).

-include("hrl/sensor_params.hrl").

%% API
-export([start_link/0, config/0]).

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

config() ->
 #{id => serial_sup,
	  start => {serial_sup, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor}.

init([]) ->
    {ok, {{one_for_all, 0, 1}, process_sensors()}}.

%%====================================================================
%% Internal functions
%%====================================================================

process_sensors() ->
	[
        serial_reader:config(#{open => "/dev/ttyACM0", speed => 9600}),
        pasive_sensor:config(#{?MEASUREMENT_NAME => humidity}),
        pasive_sensor:config(#{?MEASUREMENT_NAME => temperature})
    ].
