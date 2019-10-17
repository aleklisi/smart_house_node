-module(cpu_temperature).

-behavior(sensor_behaviour).

-export([
    child_spec/1,
    init/1,
    take_measurements/1]).

child_spec(Config) ->
    #{name := Name} = Config,
    #{
        id => Name,
        start => 
            {sensor, start_link, [Config]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE, sensor]
    }.

init(_) ->
    ok.

take_measurements(_) ->
    [{cpu_temperature, get_cpu_temperature()}].

get_cpu_temperature() ->	
    CmdResult = os:cmd("/opt/vc/bin/vcgencmd measure_temp"),	
    CmdResultTrimmed = lists:subtract(CmdResult, "temp='C\n"),	
    {Temperature, []} = string:to_float(CmdResultTrimmed),	
    Temperature.
