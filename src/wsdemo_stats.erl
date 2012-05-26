-module(wsdemo_stats).
-export([start/3, start/1, timer/2, recv/1]).

start(Clients) ->
    start("localhost", 8000, Clients).

start(Hostname, Port, Clients) -> 
    spawn(?MODULE, timer, [1000, self()]),

    This = self(),

    spawn(fun()->
                  start_clients(This, Hostname, Port, Clients)
          end),

    recv({0,0,0}).

start_clients(_StatsPid, _, _, 0) ->
    timer:sleep(infinity);
start_clients(StatsPid, Hostname, Port, Clients) ->
    wsdemo_client:start(Hostname, Port, StatsPid),
    start_clients(StatsPid, Hostname, Port, Clients-1).

recv(Stats) ->
    {Active, Closed, Messages} = Stats,
    receive
        {stats} -> io:format("~p: Stats: ~w\n",[self(), Stats])
        after 0 -> noop
    end,
    receive
        {websocket,_Pid,onopen}    -> recv({Active+1, Closed,  Messages});
        {websocket,_Pid,onmessage, _} -> recv({Active,   Closed,  Messages+1});
        {websocket,_Pid,onclose}   -> recv({Active-1, Closed+1,Messages})
    after 0 ->
            recv({Active, Closed, Messages})
    end.

timer(T, Who) ->
    receive
    after T ->
        Who ! {stats}
    end,
    timer(T, Who).

