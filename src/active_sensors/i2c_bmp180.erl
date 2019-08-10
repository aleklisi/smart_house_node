-module(i2c_bmp180).
% based on https://cdn-shop.adafruit.com/datasheets/BST-BMP180-DS000-09.pdf

-include("hrl/sensor_params.hrl").

-define(SENSOR_REGISTERED_NAME, i2c_bmp180_sensor_io_expander).

-export([config/1]).

config(#{?REPEAT_AFTER := Time}) ->
    #{
        id => i2c_bmp180,
	    start => {sensor, start_link, [
            #{
                ?SENSOR_NAME => bmp180,
                ?INIT_SENSOR_FUN => fun init/0,
                ?MEASUREMENTS_NAMES =>
                    [
                        bmp180_temperature
                    ],
                ?REPEAT_AFTER => Time,
                ?MEASUREMENT_FUN =>
                    fun() ->
                        [
                            get_temperature()
                        ]
                    end
            }
      ]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker}.

init() ->
    {ok, IoExpander} = i2c:start_link("i2c-1", 16#77),
    register(?SENSOR_REGISTERED_NAME, IoExpander),
    link(IoExpander).

get_temperature() ->
    ok = i2c:write(?SENSOR_REGISTERED_NAME, <<16#f4, 16#2e>>),
    timer:sleep(30),
    MSB = read(msb, char, unsigned),
    LSB = read(lsb, char, unsigned),
    UT = MSB bsl 8 + LSB,
    X1 = (UT - read(ac6, short, unsigned)) * read(ac5, short, unsigned) / math:pow(2, 15),
    X2 = read(mc, short, signed) * math:pow(2, 11) / (X1 + read(md, short, signed)),
    B5 = X1 + X2,
    T = (B5 + 8) / math:pow(2, 4) / 4, % I do not know why but the result is 4 times too big
    T.

read(Register, Size, Signed) ->
    RegisterAddress = get_register(Register),
    i2c:write(?SENSOR_REGISTERED_NAME, <<RegisterAddress>>),
    Binary = i2c:read(?SENSOR_REGISTERED_NAME, bytes_size(Size)),
    Result = parse(Binary, Size, Signed),
    Result.

parse(Binary, short, signed) ->
    <<Sign:1, UnsignedResult:15>> = Binary,
    sign(Sign) * UnsignedResult;
parse(Binary, short, unsigned) ->
    <<Result:16>> = Binary,
    Result;
parse(<<Result:8>>, char, unsigned) ->
    Result.

get_register(msb) -> 16#F6;
get_register(lsb) -> 16#F7;
get_register(ac5) -> 16#B2;
get_register(ac6) -> 16#B4;
get_register(mc) -> 16#BC;
get_register(md) -> 16#BE.

sign(0) -> 1;
sign(1) -> -1.

bytes_size(short) -> 2;
bytes_size(char) -> 1.
