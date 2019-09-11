-module(beam_memory).

-include("hrl/sensor_params.hrl").

-export([config/1]).

config(#{?REPEAT_AFTER := Time}) ->
    #{
        id => beam_memory,
	    start => {sensor, start_link, [
            #{
                ?SENSOR_NAME => beam_memory,
                ?MEASUREMENTS_NAMES => 
                [
                    beam_total_memory,
                    beam_atoms,
                    beam_atoms_used,
                    % beam_atom_count,
                    beam_binaries
                ],
                ?REPEAT_AFTER => Time,
                ?MEASUREMENT_FUN =>
                fun() ->
                    [
                        erlang:memory(total),
                        erlang:memory(atom),
                        erlang:memory(atom_used),
                        % introduced in erlang OTP 20 
                        % erlang:system_info(atom_count),
                        erlang:memory(binary)
                    ]
                end
            }
      ]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker}.
