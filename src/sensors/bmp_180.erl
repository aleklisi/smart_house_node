-module(bmp_180).

-export([
    child_spec/1,
    init/1,
    take_measurements/1]).

-ignore_xref([child_spec/1,
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

init(#{device_name := DeviceName, sensor_name := SensorName}) ->	
    {ok, IoExpander} = i2c:start_link(DeviceName, 16#77),	
    register(SensorName, IoExpander),	
    link(IoExpander).	

take_measurements(#{sensor_name := SensorName}) ->
    [{SensorName, get_temperature(SensorName)}].

get_temperature(SensorName) ->	
    ok = i2c:write(SensorName, <<16#f4, 16#2e>>),	
    timer:sleep(30),	
    MSB = read(msb, char, unsigned, SensorName),	
    LSB = read(lsb, char, unsigned, SensorName),	
    UT = MSB bsl 8 + LSB,	
    X1 = 
        (UT - read(ac6, short, unsigned, SensorName))
        * read(ac5, short, unsigned, SensorName)
        / math:pow(2, 15),
    X2 = 
        read(mc, short, signed, SensorName)
        * math:pow(2, 11)
        / (X1 + read(md, short, signed, SensorName)),
    B5 = X1 + X2,	
    T = (B5 + 8) / math:pow(2, 4) / 4, % I do not know why but the result is 4 times too big	
    T.	

read(Register, Size, Signed, SensorName) ->	
    RegisterAddress = get_register(Register),	
    i2c:write(SensorName, <<RegisterAddress>>),	
    Binary = i2c:read(SensorName, bytes_size(Size)),	
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