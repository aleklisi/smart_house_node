-module(passive_component).

-behaviour(gen_server).

-include("hrl/passive_component.hrl").

-ignore_xref([start_link/1]).

-export([start_link/1, config/1]).
-export([handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

config(State = #{?ID := Id}) ->
	#{id => Id,
	  start => {passive_component, start_link, [State]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker}.

start_link(Args = #{?ID := Id}) ->
    gen_server:start_link({local, Id}, ?MODULE, Args, []).

init(State) ->
    #{
        ?ID := Id,
        ?INIT_FUN := InitFun,
        ?INIT_FUN_ARGS := InitFunArgs,
        ?PROCESS_GROUP_NAME := ProcessGroupName
    } = State,
    InitState = erlang:apply(InitFun, [State | InitFunArgs]),
    lager:warning("Pasive component ~p starting", [Id]),
    pg2:create(ProcessGroupName),
    pg2:join(ProcessGroupName, self()),
    lager:info("Pasive component ~p joining group ~p", [Id, ProcessGroupName]),
    {ok, InitState}.

handle_info({?MESSAGE, Message}, State) ->
    #{
        ?ACTION_FUN := ActionFun,
        ?ACTION_FUN_ARGS := ActionFunArgs
    } = State,
    NewState = erlang:apply(ActionFun, [State, Message | ActionFunArgs]),
    {noreply, NewState};
handle_info(Message, _State) ->
    lager:error("Unexpected message ~p", [Message]),
    erlang:error("Not implemented").

handle_call(_Args, _From, _State) ->
    erlang:error("Not implemented").

handle_cast(_Args, _State) ->
    erlang:error("Not implemented").

terminate(Reason, _State) ->
    lager:warning("Passive component terminated with reason ~p", [Reason]),
    ok.
