-module(sensor).

-author('alek.lisiecki@gmail.com').

-include("hrl/sensor_params.hrl").

-behavior(gen_server).

-ignore_xref([start_link/1]).

-export([start_link/1]).
-export([handle_call/3,
         handle_cast/2,
         handle_info/2, init/1,
	     terminate/2]).

%% defaults

default_init_fun() -> ok.

default_terminate_fun() -> ok.

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    SensorsName = maps:get(?SENSOR_NAME, Args),
    logger:warning("Sensor ~p starting", [SensorsName]),
    gen_server:start_link({local, SensorsName}, ?MODULE, Args, []).

%%====================================================================
%% Gen_server callbacks
%%====================================================================
init(Args) ->
    ArgsWithDefaults = maps:merge(default_state(), Args),
    #{
        ?SENSOR_NAME := SensorName,
        ?INIT_SENSOR_FUN := InitSensorFun,
        ?INIT_SENSOR_FUN_ARGS := InitSensorFunArgs,
        ?MEASUREMENTS_NAMES := MeasurementsNames,
        ?REPEAT_AFTER := RepeatAfter
    } = ArgsWithDefaults,
    apply(InitSensorFun, InitSensorFunArgs),
    lists:foreach(
        fun exometer_wrapper:exometer_init_metric/1,
        MeasurementsNames),
    erlang:send_after(RepeatAfter, self(), ?SAVE_MEASUREMENT_MESSAGE),
    logger:info("Sensor ~p started", [SensorName]),
    {ok, ArgsWithDefaults}.

handle_info(?SAVE_MEASUREMENT_MESSAGE, State) ->
    #{
        ?REPEAT_AFTER := RepeatAfter,
        ?MEASUREMENTS_NAMES := MeasurementsNames,
        ?MEASUREMENT_FUN := MeasurementFun,
        ?MEASUREMENT_FUN_ARGS := MeasurementFunArgs
    } = State,
    erlang:send_after(RepeatAfter, self(), ?SAVE_MEASUREMENT_MESSAGE),
    Values = apply(MeasurementFun, MeasurementFunArgs),
    ValuesWithNames = lists:zip(Values, MeasurementsNames),
    lists:foreach(
        fun({Value, MeasurementName}) ->
            MetricName = exometer_wrapper:exometer_make_name(MeasurementName),
            exometer_wrapper:exometer_write(MetricName, Value)
        end,
        ValuesWithNames),
    {noreply, State}.

handle_call(_Args, _From, _State) ->
    erlang:error("not_implemented").

handle_cast(_Args, _State) ->
    erlang:error("not_implemented").

terminate(Reason, State) ->
    #{
        ?SENSOR_NAME := SensorName,
        ?TERMINATE_SENSOR_FUN := TerminateSensorFun,
        ?TERMINATE_SENSOR_FUN_ARGS := TerminateSensorFunArgs
    } = State,
    logger:error("Sensor ~p terminated with reason ~p", [SensorName, Reason]),
    apply(TerminateSensorFun, TerminateSensorFunArgs),
    ok.

%%====================================================================
%% Private function
%%====================================================================

default_state() ->
    #{
        ?SENSOR_NAME => default_sensor_name,
        ?INIT_SENSOR_FUN => fun default_init_fun/0,
        ?INIT_SENSOR_FUN_ARGS => [],
        ?MEASUREMENT_FUN_ARGS => [],
        ?REPEAT_AFTER => ?DEFAULT_REPEAT_TIME,
        ?TERMINATE_SENSOR_FUN => fun default_terminate_fun/0,
        ?TERMINATE_SENSOR_FUN_ARGS => []
    }.
