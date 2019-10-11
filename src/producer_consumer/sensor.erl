-module(sensor).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-ignore_xref([
    start_link/1]).

start_link(Config = #{name := Name}) ->
   gen_server:start_link({local, Name}, ?MODULE, [Config], []).

init([Config]) ->
    #{
        init_module := InitSensorMod,
        init_args := InitSensorArgs,
        consumer_groups := ConsumersGroups,
        producer_groups := ProducerGroups
    } = Config,
    lists:map(fun pg2:create/1, ConsumersGroups ++ ProducerGroups),
    lists:map(fun(PGName) -> pg2:join(PGName, self()) end, ConsumersGroups),
    InitResult = InitSensorMod:init(InitSensorArgs),
   {ok, Config#{init_result => InitResult}}.

handle_info(tick, State) ->
    #{
        producer_groups := ProducerGroups,
        measurement_module := MeasurementModule
    } = State,
    Measurements = MeasurementModule:take_measurements(State),
    ProcessesGroups = lists:map(fun pg2:get_local_members/1, ProducerGroups),
    MsgReceivers = lists:flatten(ProcessesGroups),
    lists:map(fun(Rcvr) -> Rcvr ! {measurements, Measurements} end, MsgReceivers),
   {noreply, State};
handle_info(Info, _State) ->
   erlang:error({unexpected_message, Info}).

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





