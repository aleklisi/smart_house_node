-module(ds1820_temperature).

-include_lib("eunit/include/eunit.hrl").

-behavior(sensor_behaviour).

-export([child_spec/1, init/1, take_measurements/1]).

child_spec(Config) ->
    #{name := Name} = Config,
    #{id => Name, start => {sensor, start_link, [Config]},
      restart => permanent, shutdown => brutal_kill,
      type => worker, modules => [?MODULE, sensor]}.

init([]) ->
    os:cmd("modprobe w1-gpio"),
    os:cmd("modprobe w1-therm"),
    Dev = get_device_id(),
    Dev.

take_measurements(#{init_result := DeviceId}) ->
    [{ds1820_temperature, get_temperature(DeviceId)}].

get_device_id() ->
    LsResult = os:cmd("ls /sys/bus/w1/devices/"),
    Folders = string:split(LsResult, "\n", all),
    [DevId] = lists:filter(fun starts_with_28/1, Folders),
    DevId.

starts_with_28([$2, $8, $- | _]) -> true;
starts_with_28(_) -> false.

get_temperature(DeviceId) ->
    CatResult = os:cmd("cat /sys/bus/w1/devices/" ++
			 DeviceId ++ "/w1_slave"),
    {ok, MP} = re:compile("t=(.*)", [ucp]),
    case re:run(CatResult, MP,
		[global, {capture, all, list}])
	of
      {match, [[_, StringTemp]]} ->
	  list_to_integer(StringTemp) / 1000;
      nomatch -> []
    end.

%% EUNIT TESTs

get_device_id_test() ->
    Str = "28-031097791404\nw1_bus_master1\n",
    Folders = string:split(Str, "\n", all),
    [DevId] = lists:filter(fun starts_with_28/1, Folders),
    ?assertEqual("28-031097791404", DevId).

get_temperature_test() ->
    {ok, MP} = re:compile("t=(.*)", [ucp]),
    CatResult = "7f 01 55 05 7f a5 a5 66 3d : crc=3d "
		"YES\n7f 01 55 05 7f a5 a5 66 3d t=23937",
    Temperature = case re:run(CatResult, MP,
			      [global, {capture, all, list}])
		      of
		    {match, [[_, StringTemp]]} ->
			list_to_integer(StringTemp);
		    nomatch -> []
		  end,
    ?assertEqual(23937, Temperature).
