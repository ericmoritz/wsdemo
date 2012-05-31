-module(wsdemo_stats).

-behaviour(gen_server).

-export([start_link/1, start_link/3, start_clients/3, stop/0, stats/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start_link(Clients) ->
    start_link("localhost", 8000, Clients).

start_link(Hostname, Port, Clients) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [Hostname, Port, Clients], []).

stop() ->
    gen_server:call(?MODULE, stop).


stats() ->
    KeyFunFun = fun(Fun) ->
                        fun(Item) ->
                                {Item, Fun(Item)}
                        end
                end,
    Counters = lists:map(KeyFunFun(fun folsom_metrics:get_metric_value/1),
                         [connections, disconnections, messages, ticks,
                          crashes]),
    Histograms = lists:map(KeyFunFun(fun folsom_metrics:get_histogram_statistics/1),
                           [connection_time, latency]),
    Histograms ++ Counters.


start_client(Hostname, Port) ->
    gen_server:cast(?MODULE, {start_client, Hostname, Port}).

start_clients(_, _, 0) ->
    receive 
        stop -> ok
    end;
start_clients(Hostname, Port, Clients) ->
    start_client(Hostname, Port),
    start_clients(Hostname, Port, Clients-1).

init([Hostname, Port, Clients]) ->
    process_flag(trap_exit, true),

    folsom_metrics:new_histogram(connection_time),    
    folsom_metrics:new_histogram(latency),
    folsom_metrics:new_counter(connections),
    folsom_metrics:new_counter(active),
    folsom_metrics:new_counter(disconnections),
    folsom_metrics:new_counter(messages),
    folsom_metrics:new_counter(crashes),
    folsom_metrics:new_counter(ticks),

    spawn_link(fun() -> start_clients(Hostname, Port, Clients) end),
    {ok, no_state}.

handle_call(stop, _From, State) ->                       
    {stop, normal, ok, State}.

handle_cast({start_client, Hostname, Port}, State) ->
    wsdemo_client:start_link(Hostname, Port),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, Reason},State) ->
    folsom_metrics:notify({crashes,1}),
    error_logger:error_msg("~s: ~w~n", ["Crash", Reason]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

    
