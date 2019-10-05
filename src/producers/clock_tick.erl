-module(clock_tick).

-behaviour(gen_server).

-ignore_xref([
    child_spec/1,
    start_link/1]).

%% API
-export([
    child_spec/1,
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

child_spec([Name, SendAfter, Msg, ProcGroupName]) ->
    #{
        id => Name,
        start => 
            {?MODULE, start_link, [[Name, SendAfter, Msg, ProcGroupName]]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    }.

start_link([Name | T]) ->
   gen_server:start_link({local, Name}, ?MODULE, T, []).

init(Args = [SendAfter, Msg, ProcGroupName]) ->
    pg2:create(ProcGroupName),
    erlang:send_after(SendAfter, self(), Msg),
    {ok, Args}.

handle_info(Msg, State = [SendAfter, Msg, ProcGroupName]) ->
    erlang:send_after(SendAfter, self(), Msg),
    Consumers = pg2:get_local_members(ProcGroupName),
    lists:foreach(fun(Pid) -> Pid ! Msg end, Consumers),
   {noreply, State}.

%%%%%%%%%%%
% Not used 
%%%%%%%%%%%

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
