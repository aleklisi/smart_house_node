-module(rpi_os_wrapper).

-export([
    get_cpu_temperature/0
]).

get_cpu_temperature() ->
    CmdResult = os:cmd("/opt/vc/bin/vcgencmd measure_temp"),
    CmdResultTrimed = lists:subtract(CmdResult, "temp='C\n"),
    {Temperature, []} = string:to_float(CmdResultTrimed),
    Temperature.
