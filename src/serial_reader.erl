-module(serial_reader).

-behaviour(gen_server).

-include("hrl/pasive_sensor.hrl").

-define(SERIAL, serial_port).
-define(JSONLIST, json_list).

-export([start_link/1, config/1]).

-export([handle_call/3, handle_cast/2, 
         handle_info/2, init/1, terminate/2]).

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

handle_info({data, Bytes}, State) ->
    MaybeJsonEnd = binary:bin_to_list(Bytes),
    NewJson = maps:get(?JSONLIST, State) ++ MaybeJsonEnd,
    NewState = case lists:last(MaybeJsonEnd) of
        $\n -> 
            NewBinaryJson = list_to_binary(NewJson),
            send_json(NewBinaryJson),
            State#{ ?JSONLIST => [] };
        _ -> 
            State#{ ?JSONLIST => NewJson }
    end,
    {noreply, NewState}.

send_json(Json) -> 
    pg2:create(?PROCESS_GROUP_NAME),
    [ PID ! {?JSON_MESSAGE, Json} || PID <- pg2:get_local_members(?PROCESS_GROUP_NAME) ].

handle_call(_Args, _From, State) -> {reply, ok, State}.

handle_cast(_Args, State) -> {noreply, State}.

terminate(Reason, _State) ->
    lager:warning("Sensor serial terminated with reason ~p", [Reason]),
    ok.
