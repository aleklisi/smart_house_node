-module(dht_11).

-behavior(sensor_behaviour).

-export([child_spec/1, init/1, take_measurements/1]).

child_spec(Config) ->
    #{
        name := Name,
        init_args := #{ gpio := _ }
    } = Config,
    #{id => Name, start => {sensor, start_link, [Config]},
      restart => permanent, shutdown => brutal_kill,
      type => worker, modules => [?MODULE, sensor]}.

init(#{gpio := GPIO}) -> GPIO.

take_measurements(#{init_result := GPIO}) ->
    #{
        humidity := Humidity,
        temperature := Temperature
    } = measure(GPIO),
    [{dht_11_temperature, Temperature},
     {dht_11_humidity, Humidity}].

measure(GPIO) ->
    Command =
	io_lib:format("python3 priv/AdafruitDHT.py 11 ~B",
              [GPIO]),
    CommandResult = os:cmd(Command),
    #{
        humidity => get_value_from_raw_command(CommandResult, "Humidity=(.*)%"),
      temperature => get_value_from_raw_command(CommandResult, "Temp=(.*)\\*")
    }.

get_value_from_raw_command(CommandResult, Regex) ->
    {ok, MP} = re:compile(Regex, [ucp]),
    X = re:run(CommandResult, MP, [global, {capture, all, list}]),
    {match, [[_, StringMeasurement]]} = X,
    list_to_float(StringMeasurement).
