-module(console_writer).

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

child_spec([Name, MessageId, ConsumerGroupName]) ->
    #{
        id => Name,
        start => 
            {?MODULE, start_link, [[Name, MessageId, ConsumerGroupName]]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    }.

start_link(Args = [Name, _MessageId, _ConsumerGroupName]) ->
   gen_server:start_link({local, Name}, ?MODULE, Args, []).

init(Args = [Name, MessageId, ConsumerGroupName]) ->
   pg2:join(ConsumerGroupName, self()),
   logger:warning("~p ~p starting with ~p\n", [?MODULE, Name, MessageId]),
   {ok, Args}.

handle_info(tick, State) ->
   logger:info("~p\n", [tick]),
   {noreply, State};
handle_info({MessageId, Content}, State = [_Name, MessageId]) ->
   logger:info("~p ~p\n", [MessageId, Content]),
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
