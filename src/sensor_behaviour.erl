-module(sensor_behaviour).

-ignore_xref([behaviour_info/1]).

-callback child_spec(any()) -> supervisor:child_spec().

-callback init(InitSensorArgs :: any()) -> InitResult :: any().

-callback take_measurements(GenServerState :: any()) ->
    [{MeasumentName :: atom, Result :: float() | integer()}].
