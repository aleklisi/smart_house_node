-module(config).

-export([config/0]).

config() ->
[
    {
        serial,
        serial_reader,
        #{open => #{open => "/dev/ttyACM0", speed => 9600}},
        [
            {exometer_reporter, pm10},
            {exometer_reporter, pm25},
            {exometer_reporter, pm25only}
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