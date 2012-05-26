-module(wsdemo_client).

-behaviour(websocket_client).

-export([start/1, start/3, start/4]).

-export([ws_onopen/1, ws_onmessage/2, ws_info/2, ws_onclose/1]).

start(StatsPid) ->
    start("localhost", 8000, StatsPid).

start(Host, Port, StatsPid) ->
    start(Host, Port, "/", StatsPid).

start(Host, Port, Path, StatsPid) ->
    websocket_client:start_link(Host, Port, Path, ?MODULE, StatsPid).

ws_onopen(StatsPid) ->
    StatsPid ! {websocket, self(), onopen},
    StatsPid.

ws_onmessage(Msg, StatsPid) ->
    StatsPid ! {websocket, self(), onmessage, Msg},
    StatsPid.

ws_info(_Msg, StatsPid) ->
    StatsPid ! {websocket, self(), info},
    StatsPid.

ws_onclose(StatsPid) ->
    StatsPid ! {websocket, self(), onclose},
    StatsPid.
