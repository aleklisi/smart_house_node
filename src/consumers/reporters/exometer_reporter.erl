-module(exometer_reporter).

-behavior(reporter_behaviour).

-export([
   child_spec/1,
    init/1,
    handle_info/2]).

child_spec(Config) ->
    #{
        name := Name
    } = Config,
    #{
        id => Name,
        start =>
            {reporter, start_link, [Config]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE, reporter]
    }.

init([]) ->
    {false, histogram};
init([MetricType]) ->
    {false, MetricType}.

handle_info(Msg = {measurements, Info}, State = #{init_result := {false, MetricType}}) ->
    {MetricsNames, _} = lists:unzip(Info),
    lists:foreach(
        fun(MetricName) ->
            exometer_init_metric(MetricName, MetricType)
        end, MetricsNames),
     self() ! Msg,
    State#{init_result => true};
handle_info({measurements, Info}, State = #{init_result := true}) ->
    lists:foreach(
        fun({MeasurementName, Value}) ->
            exometer_write([MeasurementName], Value)
        end, Info),
    State.

exometer_init_metric(MeasurementName, MetricType) ->
    MetricName = [MeasurementName],
    R = exometer:new(MetricName, MetricType),
    exometer_report:subscribe(exometer_report_graphite,
			      MetricName, [mean, min, max, median], 10000),
    R.	

exometer_write(MetricName, Value) ->
    exometer:update(MetricName, Value).	
