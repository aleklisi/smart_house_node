-module(smart_house_node_app).

-author('alek.lisiecki@gmail.com').

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Config} = application:get_env(smart_house_node, config),
    smart_house_node_sup:start_link(Config).

stop(_State) ->
    ok.
