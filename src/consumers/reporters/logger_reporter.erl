-module(logger_reporter).

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

init(_) ->
    logger:set_module_level(?MODULE, all).

handle_info(Info, State) ->
    logger:error("~p", [Info]),
    State.
