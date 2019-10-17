-module(smart_house_node_sup).

-author('alek.lisiecki@gmail.com').

-behavior(supervisor).

-ignore_xref([start_link/1]).

-export([
    start_link/1,
    init/1]).

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

init(Config) ->
    Children = lists:map(fun make_child/1, Config),
    {ok, {#{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    }, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

make_child({Module,Opts}) ->
    Module:child_spec(Opts).
