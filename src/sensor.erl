-module(sensor).

-include("hrl/sensor_params.hrl").

-behaviour(gen_server).

-ignore_xref([start_link/1]).

-export([start_link/1]).
-export([handle_call/3, handle_cast/2,
         handle_info/2, init/1,
	     terminate/2]).


%% defaults

default_init_fun() -> ok.

default_terminate_fun() -> ok.

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    MeasurementName = maps:get(?MEASUREMENT_NAME, Args),
    lager:warning("Sensor ~p starting", [MeasurementName]),
    gen_server:start_link({local, MeasurementName}, ?MODULE,
			  Args, []).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init(Args) ->
    SensorName = maps:get(?MEASUREMENT_NAME, Args),
    lager:info("Sensor ~p started", [SensorName]),
    InitSensorFun = maps:get(?INIT_SENSOR_FUN, Args,
			     fun default_init_fun/0),
    InitSensorFunArgs = maps:get(?INIT_SENSOR_FUN_ARGS,
				 Args, []),
    apply(InitSensorFun, InitSensorFunArgs),
    MeasurementName = maps:get(?MEASUREMENT_NAME, Args),
    RepeatAfterMiliseconds = maps:get(?REPEAT_AFTER, Args,
				      ?DEFAULT_REPEATE_TIME),
    exometer_wrapper:exometer_init_metric(MeasurementName),
    erlang:send_after(RepeatAfterMiliseconds, self(),
		      ?SAVE_MEASUREMENT_MESSAGE),
    {ok, Args#{?REPEAT_AFTER => RepeatAfterMiliseconds}}.

handle_info(?SAVE_MEASUREMENT_MESSAGE, State) ->
    #{?REPEAT_AFTER := RepeatAfterMiliseconds,
      ?MEASUREMENT_NAME := MeasurementName,
      ?MEASUREMENT_FUN := MeasuremntFun,
      ?MEASUREMENT_FUN_ARGS := MeasuremntFunArgs} =
	State,
    erlang:send_after(RepeatAfterMiliseconds, self(),
		      ?SAVE_MEASUREMENT_MESSAGE),
    Value = apply(MeasuremntFun, MeasuremntFunArgs),
    MetricName = exometer_wrapper:exometer_make_name(MeasurementName),
    exometer_wrapper:exometer_write(MetricName, Value),
    {noreply, State}.

handle_call(_Args, _From, State) -> {reply, ok, State}.

handle_cast(_Args, State) -> {noreply, State}.

terminate(Reason, State) ->
    SensorName = maps:get(?MEASUREMENT_NAME, State),
    lager:warning("Sensor ~p terminated with reason ~p",
		   [SensorName, Reason]),
    TermonateSensorFun = maps:get(?TERMONATE_SENSOR_FUN,
				  State, fun default_terminate_fun/0),
    TermonateSensorFunArgs =
	maps:get(?TERMONATE_SENSOR_FUN_ARGS, State, []),
    apply(TermonateSensorFun, TermonateSensorFunArgs),
    ok.

%%====================================================================
%% Private function
%%====================================================================
