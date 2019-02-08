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
    Measurement = maps:get(?MEASUREMENT_NAME, Args),
    gen_server:start_link({local, Measurement}, ?MODULE, Args, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Args) ->
    Measurement = maps:get(?MEASUREMENT_NAME, Args),
    RepeatAfterMiliseconds = maps:get(?REPEAT_AFTER, Args, ?DEFAULT_REPEATE_TIME),
    State = Args,
    mnesia:create_table(Measurement, [
        {disc_only_copies, [node()]},
        {attributes, [time, node_name, value, unit]}]),
    erlang:send_after(RepeatAfterMiliseconds, self(), ?SAVE_MEASUREMENT_MESSAGE),
    {ok, State}.

handle_info(?SAVE_MEASUREMENT_MESSAGE, State) ->
    RepeatAfterMiliseconds = maps:get(?REPEAT_AFTER, State, ?DEFAULT_REPEATE_TIME),
    erlang:send_after(RepeatAfterMiliseconds, self(), ?SAVE_MEASUREMENT_MESSAGE),
    MeasurementName = maps:get(?MEASUREMENT_NAME, State),
    MeasuremntFun = maps:get(?MEASUREMENT_FUN, State),
    MeasuremntFunArgs = maps:get(?MEASUREMENT_FUN_ARGS, State, []),
    Unit = maps:get(?UNIT, State),
    Time = os_wrapper:get_timestamp(),
    Value = apply(MeasuremntFun, MeasuremntFunArgs),
    Record = {MeasurementName, Time, node(), Value, Unit},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Record) end),
    {noreply, State}.

handle_call(_Args, _From, State) ->
    MeasurementName = maps:get(?MEASUREMENT_NAME, State),
    Result = mnesia:dirty_match_object({MeasurementName, '_', '_', '_', '_'}),
    {reply, Result, State}.

handle_cast(_Args, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok.
