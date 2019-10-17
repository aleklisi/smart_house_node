-module(reporter).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ignore_xref([
    start_link/1]).

start_link(Config = #{name := Name}) ->
   gen_server:start_link({local, Name}, ?MODULE, [Config], []).

init([Config]) ->
    #{
        reporter_module := ReporterModule,
        reporter_init_args := ReporterInitArgs,
        consumer_group := ConsumerGroup
    } = Config,
    pg2:create(ConsumerGroup),
    pg2:join(ConsumerGroup, self()),
    InitResult = ReporterModule:init(ReporterInitArgs),
   {ok, Config#{init_result => InitResult}}.

handle_info(Info, State = #{reporter_module := ReporterModule}) ->
    NewState = ReporterModule:handle_info(Info, State),
    {noreply, NewState}.

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
