-module(rpi_os_wrapper).

-author('alek.lisiecki@gmail.com').

-export([
    get_cpu_temperature/0
]).

get_cpu_temperature() ->
    CmdResult = os:cmd("/opt/vc/bin/vcgencmd measure_temp"),
    CmdResultTrimmed = lists:subtract(CmdResult, "temp='C\n"),
    {Temperature, []} = string:to_float(CmdResultTrimmed),
    Temperature.
