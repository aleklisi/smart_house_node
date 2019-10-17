-module(pipe_sup).

-author('alek.lisiecki@gmail.com').

-behavior(supervisor).

-ignore_xref([
    child_spec/1,
    start_link/1]).

-export([
    start_link/1,
    init/1,
    child_spec/1]).

child_spec({Name, Children}) ->
    #{
        id => Name,
        start =>
            {?MODULE, start_link, [{Name, Children}]},
        modules => [?MODULE]
    }.

start_link({Name, RawChildren}) ->
    supervisor:start_link({local, Name}, ?MODULE, RawChildren).

init(Config) ->
    Children = lists:map(fun make_child/1, Config),
    {ok, {#{
        strategy => rest_for_one,
        intensity => 0,
        period => 1
    }, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

make_child({Module, Opts}) ->
    Module:child_spec(Opts).
