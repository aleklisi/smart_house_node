-module(reporter_behaviour).

-ignore_xref([behaviour_info/1]).

-callback child_spec(any()) -> supervisor:child_spec().

-callback init(InitSensorArgs :: any()) -> InitResult :: any().

-callback handle_info(Info :: any(), State :: map()) ->
    NewState :: map().
