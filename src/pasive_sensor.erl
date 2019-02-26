-module(pasive_sensor).

-behaviour(gen_server).

-include("hrl/sensor_params.hrl").
-include("hrl/pasive_sensor.hrl").

-export([start_link/1, config/1]).

-export([handle_call/3, handle_cast/2, 
         handle_info/2, init/1, terminate/2]).

config(State = #{?MEASUREMENT_NAME := MeasurementName}) ->
	#{id => MeasurementName,
	  start => {pasive_sensor, start_link, [State]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker}.

start_link(Args = #{?MEASUREMENT_NAME := MeasurementName}) ->
    gen_server:start_link({local, MeasurementName}, ?MODULE, Args, []).

init(Args) ->
    MeasurementName = maps:get(?MEASUREMENT_NAME, Args),
    lager:warning("Pasive sensor ~p starting", [MeasurementName]),
    exometer_wrapper:exometer_init_metric(MeasurementName),
    pg2:create(?PROCESS_GROUP_NAME),
    pg2:join(?PROCESS_GROUP_NAME, self()),
    {ok, Args}.

handle_call(_Args, _From, State) -> {reply, ok, State}.

handle_cast(_Args, State) -> {noreply, State}.

terminate(Reason, _State) ->
    lager:warning("Sensor serial terminated with reason ~p", [Reason]),
    ok.

handle_info({?JSON_MESSAGE, Json}, State) ->
    MeasurementName = maps:get(?MEASUREMENT_NAME, State),
    lager:info("Json map is ~p", [Json]),
    JsonMap = jsx:decode(Json, [return_maps]),
    lager:info("Json map is ~p", [JsonMap]),
    Value = maps:get(atom_to_binary(MeasurementName, utf8), JsonMap),
    MetricName = exometer_wrapper:exometer_make_name(MeasurementName),
    exometer_wrapper:exometer_write(MetricName, Value),
    {noreply, State}.
