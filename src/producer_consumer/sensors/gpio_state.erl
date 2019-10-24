-module(gpio_state).

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

init([Pin]) ->
    StrName = "gpio" ++ integer_to_list(Pin),
    Name = list_to_atom(StrName),
    logger:info("GPIO monitoring pin ~p started", [Pin]),
    {ok, Gpio} = gpio:start_link(Pin, input),
    {Name, Gpio}.

take_measurements(#{init_result := {Name, Gpio}}) ->
    Value = gpio:read(Gpio),
    logger:debug("GPIO monitoring pin ~p measured ~p value", [Name, Value]),
    [
        {Name, Value}
    ].
