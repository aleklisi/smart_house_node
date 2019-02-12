-module(cpu_temperature).

-include("hrl/sensor_params.hrl").

-export([config/1]).

config(#{?REPEAT_AFTER := Time}) ->
    #{id => cpu_temperature,
	  start => {sensor, start_link, [
          #{
            ?MEASUREMENT_NAME => cpu_temperature,
            ?REPEAT_AFTER => Time,
            ?MEASUREMENT_FUN => fun rpi_os_wrapper:get_cpu_temperature/0,
            ?MEASUREMENT_FUN_ARGS => []
          }
      ]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker}.