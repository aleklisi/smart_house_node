-module(beam_memory).

-behavior(sensor_behaviour).

-export([
    child_spec/1,
    init/1,
    take_measurements/1]).

child_spec(Config) ->
    #{name := Name} = Config,
    #{
        id => Name,
        start =>
            {sensor, start_link, [Config]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE, sensor]
    }.

init(_Args) ->
    ok.

take_measurements(_State) ->
    [
        {total_beam_memory, erlang:memory(total)},
        {atom_memory, erlang:memory(atom)},
        {atom_used_memory, erlang:memory(atom_used)},
        {atoms_count, erlang:system_info(atom_count)},
        {binary_memory, erlang:memory(binary)}
    ].
