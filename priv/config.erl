-module(config).

-author('alek.lisiecki@gmail.com').

-include("hrl/passive_component.hrl").

-export([config/0]).

config() ->
[
    {
        serial,
        serial_reader,
        #{open => "/dev/ttyACM0", speed => 9600, ?PROCESS_GROUP_NAME => serial_based_sensors},
        [
            {exometer_reporter, {humidity, serial_based_sensors}},
            {exometer_reporter, {temperature, serial_based_sensors}},
            {exometer_reporter, {mq3, serial_based_sensors}},
            {exometer_reporter, {mq5, serial_based_sensors}},
            {exometer_reporter, {mq135, serial_based_sensors}}
        ]
    },
    {
        active_sensors,
        [
            {
                cpu_temperature, #{repeat_after => 1000}
            }
        ]
    }
].