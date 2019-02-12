-module(rpi_os_wrapper).

-export([
    get_cpu_temperature/0,
    get_timestamp/0
]).

get_cpu_temperature() ->
    CmdResult = os:cmd("/opt/vc/bin/vcgencmd measure_temp"),
    CmdResultTrimed = lists:subtract(CmdResult, "temp='C\n"),
    {Temperature, []} = string:to_float(CmdResultTrimed),
    Temperature.

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).
