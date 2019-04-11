-module(exometer_reporter).

-include("hrl/passive_component.hrl").
-export([config/1]).

config(MeasurementName) ->
    State = #{
        ?ID => MeasurementName,
        ?INIT_FUN => fun init/2,
        ?INIT_FUN_ARGS => [],
        ?ACTION_FUN => fun action/3,
        ?ACTION_FUN_ARGS => []
    },
    passive_component:config(State).

init(State, []) ->
    MeasurementName = maps:get(?ID, State),
    lager:warning("Pasive sensor ~p starting", [MeasurementName]),
    exometer_wrapper:exometer_init_metric(MeasurementName),
    State.

action(State, Message, []) ->
    MeasurementName = maps:get(?ID, State),
    lager:info("Message is", [Message]),
    JsonMap = jsx:decode(Message, [return_maps]),
    lager:info("Decoded json is ~p", [JsonMap]),
    Value = maps:get(atom_to_binary(MeasurementName, utf8), JsonMap),
    MetricName = exometer_wrapper:exometer_make_name(MeasurementName),
    exometer_wrapper:exometer_write(MetricName, Value),
    State.

