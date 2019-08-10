
-author('alek.lisiecki@gmail.com').

%% @doc Name of a sensor used to name process,
%% useful for debug, trace or view supervision tree.
%% Obligatory.
%% @spec atom()
%% @end
-define(SENSOR_NAME, sensor_name).

%% @doc A list of names (atoms) of measurements for each measurement,
%% for most of sensors it will be just 1 element list.
%% Obligatory.
%% Default default_sensor_name.
%% @spec [atom()]
%% @end
-define(MEASUREMENTS_NAMES, measurement_names).

%% @doc Set custom repeat loop time.
%% Optional.
%% Default is ?DEFAULT_REPEAT_TIME.
%% @spec pos_integer()
%% @end
-define(REPEAT_AFTER, repeat_after).

%% @doc A function that does actual measurements.
%% Obligatory.
%% @spec fun([any]) -> [number()].
%% @end
-define(MEASUREMENT_FUN, measurement_function).

%% @doc A measurement function constance extra arguments passed in config.
%% Optional.
%% Default is [].
%% @spec [any()].
%% @end
-define(MEASUREMENT_FUN_ARGS,
	measurement_function_arguments).

%% @doc A function that is executed on sensor terminate.
%% Optional.
%% Default fun() -> ok..
%% @spec fun([any]) -> any().
%% @end
-define(TERMINATE_SENSOR_FUN, terminate_fun).

%% @doc A function that is executed on sensor init.
%% Optional.
%% Default fun() -> ok..
%% @spec fun([any]) -> any().
%% @end
-define(INIT_SENSOR_FUN, init_fun).

%% @doc A terminate function constance extra arguments passed in config.
%% Optional.
%% Default is [].
%% @spec [any()].
%% @end
-define(TERMINATE_SENSOR_FUN_ARGS, terminate_fun_args).

%% @doc An init function constance extra arguments passed in config.
%% Optional.
%% Default is [].
%% @spec [any()].
%% @end
-define(INIT_SENSOR_FUN_ARGS, init_fun_args).

%%====================================================================
%% Internal params
%%====================================================================

%% @doc Internal message for active sensors to work in a loop.
%% @end
-define(SAVE_MEASUREMENT_MESSAGE, save_measurement).

%% @doc Default repeated time is set to 5 seconds.
%% @end
-define(DEFAULT_REPEAT_TIME, 5000).
