-module(smart_house_node_app).

-author('alek.lisiecki@gmail.com').

-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    smart_house_node_sup:start_link(config:config()).

stop(_State) ->
    ok.
