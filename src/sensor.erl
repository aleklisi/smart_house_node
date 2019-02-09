-module(sensor).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2]).

-define(SAVE_MEASUREMENT_MESSAGE, save_measurement).
-define(DEFAULT_REPEATE_TIME, 5000).

%state elements
-define(MEASUREMENT_NAME, m_name).
-define(REPEAT_AFTER, repeat_after).
-define(UNIT, unit).
-define(MEASUREMENT_FUN, measurement_function).
-define(MEASUREMENT_FUN_ARGS, measurement_function_arguments).
%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    MeasurementName = maps:get(?MEASUREMENT_NAME, Args),
    gen_server:start_link({local, MeasurementName}, ?MODULE, Args, []).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init(Args) ->
    MeasurementName = maps:get(?MEASUREMENT_NAME, Args),
    RepeatAfterMiliseconds = maps:get(?REPEAT_AFTER, Args, ?DEFAULT_REPEATE_TIME),
    State = Args,
    exometer_init_metric(MeasurementName),
    erlang:send_after(RepeatAfterMiliseconds, self(), ?SAVE_MEASUREMENT_MESSAGE),
    {ok, State}.

handle_info(?SAVE_MEASUREMENT_MESSAGE, State) ->
    RepeatAfterMiliseconds = maps:get(?REPEAT_AFTER, State, ?DEFAULT_REPEATE_TIME),
    erlang:send_after(RepeatAfterMiliseconds, self(), ?SAVE_MEASUREMENT_MESSAGE),
    MeasurementName = maps:get(?MEASUREMENT_NAME, State),
    MeasuremntFun = maps:get(?MEASUREMENT_FUN, State),
    MeasuremntFunArgs = maps:get(?MEASUREMENT_FUN_ARGS, State, []),
    Value = apply(MeasuremntFun, MeasuremntFunArgs),
    MetricName = exometer_make_name(MeasurementName),
    exometer_write(MetricName, Value),
    {noreply, State}.

handle_call(_Args, _From, State) ->
    {reply, ok, State}.

handle_cast(_Args, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Private function
%%====================================================================

%TODO export to seprarate module

exometer_init_metric(MeasurementName) ->
    MetricName = exometer_make_name(MeasurementName),
    R = exometer:new(MetricName, histogram),
    exometer_report:subscribe(exometer_report_graphite, MetricName, [mean, min, max, median], 10000),
    lager:info("Exometer init ~p\tMetricName = ~p\n", [R, MetricName]),
    R.

exometer_write(MetricName, Value) ->
    R = exometer:update(MetricName, Value),
    lager:info("Exometer write ~p\tMetricName = ~p\n", [R, MetricName]),
    R.

exometer_make_name(MeasurementName) ->
    [node(), MeasurementName].
