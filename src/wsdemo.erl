-module(wsdemo).

-export([start/1, start/0]).

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3,
         websocket_terminate/3]).

start() ->
    start(8000).

start(Port) ->
    application:start(cowboy),

    Dispatch = [
                {'_', [{'_', ?MODULE, []}]}
               ],

    cowboy:start_listener(?MODULE, 1000,
                          cowboy_tcp_transport, [{port, Port}, {max_connections, infinity}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]).
    
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_handle(Msg, Req, State) ->
    {reply, Msg, Req, State, hibernate}.

websocket_info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
