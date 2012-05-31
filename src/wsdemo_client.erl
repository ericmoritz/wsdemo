-module(wsdemo_client).

-behaviour(websocket_client).

-export([start_link/0, start_link/2, start_link/3]).

-export([ws_init/0, ws_onopen/2, ws_onmessage/3, ws_info/3, ws_onclose/2]).

-record(state, {start_time}).

start_link() ->
    start_link("localhost", 8000).

start_link(Host, Port) ->
    start_link(Host, Port, "/").

start_link(Host, Port, Path) ->
    websocket_client:start_link(?MODULE, Host, Port, Path).

ws_init() ->
    #state{start_time=get_timestamp()}.

ws_onopen(_Client, #state{start_time=TS} = State) ->
    Elapsed = get_timestamp() - TS,
    folsom_metrics:notify({connection_time, Elapsed}),
    folsom_metrics:notify({connections, {inc, 1}}),
    folsom_metrics:notify({active, {inc, 1}}),
    erlang:start_timer(0, self(), send_ping),
    State.

ws_onmessage(Client, Msg, State) ->
    folsom_metrics:notify({messages, {inc, 1}}),
    handle_msg(Client, Msg, State).

handle_msg(_Client, {text,<<"tick">>}, State) ->
    folsom_metrics:notify({ticks, {inc, 1}}),
    folsom_metrics:notify({tick_rate, 1}),
    State;
handle_msg(Client, {binary, <<"ping:",TS:64/integer>>}, State) ->
    Elapsed = get_timestamp() - TS,
    folsom_metrics:notify({latency, Elapsed}),
    State;
handle_msg(_Client, Msg, State) ->
    error_logger:warning_msg("Unmatched msg: ~p~n", [Msg]),
    State.

ws_info(Client, {timeout, _Ref, send_ping}, State) ->
    TS = get_timestamp(),
    Data = <<"ping:", TS:64/integer>>,
    websocket_client:write_sync(Client, {binary,Data}),
    % queue up the next send_ping message
    erlang:start_timer(0, self(), send_ping),
    State.

ws_onclose(Client, State) ->
    error_logger:info_msg("~p~n", [Client]),
    folsom_metrics:notify({active, {dec, 1}}),
    folsom_metrics:notify({disconnections, {inc, 1}}),
    State.

get_timestamp() ->
    folsom_utils:now_epoch_micro().
