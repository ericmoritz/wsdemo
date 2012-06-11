-module(wsdemo_client).

-behaviour(websocket_client).

-export([start_link/0, start_link/2, start_link/3]).

-export([ws_init/0, ws_onopen/2, ws_onmessage/3, ws_info/3, ws_onclose/2]).

-record(state, {start_time}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
    start_link("localhost", 8000).

start_link(Host, Port) ->
    start_link(Host, Port, "/").

start_link(Host, Port, Path) ->
    websocket_client:start_link(?MODULE, Host, Port, Path).

ws_init() ->
    #state{start_time=erlang:now()}.

ws_onopen(_Client, #state{start_time=TS} = State) ->
    Elapsed = timer:now_diff(erlang:now(), TS),
    folsom_metrics:notify({connection_time, Elapsed}),
    folsom_metrics:notify({connections, {inc, 1}}),
    folsom_metrics:notify({active, {inc, 1}}),
    erlang:start_timer(0, self(), send_ping),
    State.

ws_onmessage(Client, Msg, State) ->
    folsom_metrics:notify({messages, {inc, 1}}),
    handle_msg(Client, Msg, State).

handle_msg(Client, {text, <<"ping:",_/bits>> = Msg}, State) ->
    % rewrite the text message as a binary message
    handle_msg(Client, {binary, Msg}, State);
handle_msg(_Client, {binary, <<"ping:",NowBytes/bits>>}, State) ->
    End = erlang:now(),
    Start = decode_now(NowBytes),
    Elapsed = timer:now_diff(End, Start),
    folsom_metrics:notify({latency, Elapsed}),
    State;
handle_msg(_Client, Msg, State) ->
    error_logger:warning_msg("Unmatched msg: ~p~n", [Msg]),
    State.

ws_info(Client, {timeout, _Ref, send_ping}, State) ->
    TS = encode_now(erlang:now()),
    Data = <<"ping:", TS/binary>>,
    websocket_client:write_sync(Client, {binary, Data}),
    % queue up the next send_ping message
    erlang:start_timer(1000, self(), send_ping),
    State.

ws_onclose(Client, State) ->
    error_logger:info_msg("~p~n", [Client]),
    folsom_metrics:notify({active, {dec, 1}}),
    folsom_metrics:notify({disconnections, {inc, 1}}),
    State.

encode_now({MegaSecs, Secs, MicroSecs}) ->
    <<MegaSecs:32, Secs:32, MicroSecs:32>>.

decode_now(<<MegaSecs:32, Secs:32, MicroSecs:32>>) ->
    {MegaSecs, Secs, MicroSecs}.

-ifdef(TEST).

decode_now_test_() ->
    Now = {1338,474888,611762},
    [
     ?_assertEqual(Now, decode_now(encode_now(Now)))
     ].

-endif.
