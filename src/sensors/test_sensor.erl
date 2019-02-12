-module(test_sensor).

-include("hrl/sensor_params.hrl").

-export([config/1]).

config(#{}) ->
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