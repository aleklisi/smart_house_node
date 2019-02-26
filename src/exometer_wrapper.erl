-module(exometer_wrapper).

-export(
    [
        exometer_init_metric/1,
        exometer_write/2,
        exometer_make_name/1
    ]).

exometer_init_metric(MeasurementName) ->
    MetricName = exometer_make_name(MeasurementName),
    R = exometer:new(MetricName, histogram),
    exometer_report:subscribe(exometer_report_graphite,
			      MetricName, [mean, min, max, median], 10000),
    lager:info("Exometer init ~p\tMetricName = ~p\n",
	       [R, MetricName]),
    R.

exometer_write(MetricName, Value) ->
    R = exometer:update(MetricName, Value),
    lager:info("Exometer write ~p\tMetricName = ~p\n",
	       [R, MetricName]),
    R.

exometer_make_name(MeasurementName) ->
    [MeasurementName].
