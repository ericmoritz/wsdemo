-module(wsdemo_runner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/5]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(LogFile, Server, Port, Clients, Seconds) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [LogFile, Server, Port, Clients, Seconds], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([LogFile, Server, Port, Clients, Seconds]) ->
    {ok, _} = wsdemo_logger:start_link(LogFile),
    {ok, _} = wsdemo_stats:start_link(Server, Port, Clients),

    erlang:send_after(timer:seconds(Seconds), self(), done),
    {ok, [LogFile, Server, Port, Clients, Seconds]}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(done, State) ->
    io:format("Stopping~n", []),
    {stop, normal, State};
handle_info(timeout, State) ->
    {noreply, State};
handle_info(Msg, State) ->
    io:format("Unknows msg: ~w~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    wsdemo_logger:close(),
    wsdemo_stats:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

