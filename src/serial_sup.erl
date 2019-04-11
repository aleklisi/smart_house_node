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
    {ok, {#{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    }, process_sensors()}}.

%%====================================================================
%% Internal functions
%%====================================================================

process_sensors() ->
	[
        serial_reader:config(#{open => "/dev/ttyACM0", speed => 9600}),
        pasive_sensor:config(#{?MEASUREMENT_NAME => pm10}),
        pasive_sensor:config(#{?MEASUREMENT_NAME => pm25}),
        pasive_sensor:config(#{?MEASUREMENT_NAME => pm25only})
    ].
