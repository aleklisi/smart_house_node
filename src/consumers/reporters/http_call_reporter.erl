-module(http_call_reporter).

-behavior(reporter_behaviour).

-export([
   child_spec/1,
    init/1,
    handle_info/2]).

child_spec(Config) ->
    #{
        name := Name
    } = Config,
    #{
        id => Name,
        start =>
            {reporter, start_link, [Config]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE, reporter]
    }.

init([Url]) ->
    inets:start(),
    Url.

handle_info({measurements, Info}, State = State = #{init_result := Url}) ->
    MapInfo = maps:from_list(Info),
    Body = jiffy:encode(MapInfo),
    Headers = [],
    ContentType = "application/json",
    Request = {Url, Headers, ContentType, Body},
    httpc:request(post, Request, [], []),
    State.
