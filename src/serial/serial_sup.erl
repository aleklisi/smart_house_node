-module(serial_sup).

-author('alek.lisiecki@gmail.com').

-behavior(supervisor).

-include("hrl/sensor_params.hrl").

-ignore_xref([start_link/1]).
-export([start_link/1,
         config/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Children) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Children).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

config(Config) ->
    Children = make_children(Config),
    #{id => ?MODULE,
	  start => {?MODULE, start_link, [Children]},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor}.

init(Children) ->
    {ok, {#{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    }, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

make_children({serial, ReaderModule, ReaderConfig, Children}) ->
    PassiveComponents = lists:map(fun make_passive_child/1, Children),
    [ ReaderModule:config(ReaderConfig) | PassiveComponents].

make_passive_child({Module, Args}) ->
    Module:config(Args).