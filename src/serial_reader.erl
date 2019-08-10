-module(serial_reader).

-author('alek.lisiecki@gmail.com').

-behavior(gen_server).

-include("hrl/passive_component.hrl").

-ignore_xref([start_link/1]).

-export([start_link/1, config/1]).
-export([handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

-define(SERIAL, serial_port).
-define(JSONLIST, json_list).

config(Args) ->
	#{id => serial_reader,
	  start => {serial_reader, start_link, [Args]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker}.

start_link(Args) ->
    gen_server:start_link({local, serial_reader}, ?MODULE, Args, []).

init(Args) ->
    #{
        open := DevicePath,
        speed := Speed
    } = Args,
    SerialPort = serial:start([{open, DevicePath}, {speed, Speed}]),
    lager:warning("Sensor serial starting", []),
    {ok, Args#{?SERIAL => SerialPort, ?JSONLIST => []}}.

handle_info({data, Bytes}, State = #{ ?PROCESS_GROUP_NAME := ProcessGroupName}) ->
    MaybeJsonEnd = binary:bin_to_list(Bytes),
    NewJson = maps:get(?JSONLIST, State) ++ MaybeJsonEnd,
    NewState = case lists:last(MaybeJsonEnd) of
        $\n ->
            NewBinaryJson = list_to_binary(NewJson),
            send_json(NewBinaryJson, ProcessGroupName),
            State#{ ?JSONLIST => [] };
        _ ->
            State#{ ?JSONLIST => NewJson }
    end,
    {noreply, NewState}.

send_json(Json, ProcessGroupName) ->
    pg2:create(ProcessGroupName),
    [ PID ! {?MESSAGE, Json} || PID <- pg2:get_local_members(ProcessGroupName) ].

handle_call(_Args, _From, _State) ->
    erlang:error("Not implemented").

handle_cast(_Args, _State) ->
    erlang:error("Not implemented").

terminate(Reason, _State) ->
    lager:warning("Sensor serial terminated with reason ~p", [Reason]),
    ok.
