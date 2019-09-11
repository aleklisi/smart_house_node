-module(exometer_reporter).

-author('alek.lisiecki@gmail.com').

-include("hrl/passive_component.hrl").
-export([config/1]).

config({MeasurementName, ProcessGroupName}) ->
    State = #{
        ?ID => MeasurementName,
        ?INIT_FUN => fun init/1,
        ?INIT_FUN_ARGS => [],
        ?ACTION_FUN => fun action/2,
        ?ACTION_FUN_ARGS => [],
        ?PROCESS_GROUP_NAME => ProcessGroupName
    },
    passive_component:config(State).

init(State) ->
    MeasurementName = maps:get(?ID, State),
    lager:warning("Passive sensor ~p starting", [MeasurementName]),
    exometer_wrapper:exometer_init_metric(MeasurementName),
    State.

action(State, Message) ->
    MeasurementName = maps:get(?ID, State),
    lager:debug("Message is ~p", [Message]),
    JsonMap = jsx:decode(Message, [return_maps]),
    lager:debug("Decoded json is ~p", [JsonMap]),
    Value = maps:get(atom_to_binary(MeasurementName, utf8), JsonMap),
    MetricName = exometer_wrapper:exometer_make_name(MeasurementName),
    exometer_wrapper:exometer_write(MetricName, Value),
    State.

