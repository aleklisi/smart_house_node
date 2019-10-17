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

child_spec([Name, SendAfter, ProcGroupName]) ->
    #{
        id => Name,
        start =>
            {?MODULE, start_link, [[Name, SendAfter, ProcGroupName]]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    }.

start_link([Name | T]) ->
   gen_server:start_link({local, Name}, ?MODULE, T, []).

init(Args = [SendAfter, ProcGroupName]) ->
    pg2:create(ProcGroupName),
    erlang:send_after(SendAfter, self(), tick),
    {ok, Args}.

handle_info(tick, State = [SendAfter, ProcGroupName]) ->
    erlang:send_after(SendAfter, self(), tick),
    Consumers = pg2:get_local_members(ProcGroupName),
    lists:foreach(fun(Pid) -> Pid ! tick end, Consumers),
   {noreply, State};
handle_info(Info, _State) ->
    erlang:error({"Unexpected message", Info}).

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
