-module(wsdemo_runner_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).
-record(state, {server_name, db, host, port, clients, seconds}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/6, cancel/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

% event handlers
-export([running/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ServerName, DB, Host, Port, Clients, Seconds) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE,
                       [ServerName, DB, Host, Port, Clients, Seconds], []).

cancel() ->
    gen_fsm:sync_send_event(?SERVER, cancel).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([ServerName, DB, Host, Port, Clients, Seconds]) ->
    State = #state{server_name=ServerName,
                   db=DB,
                   host=Host,
                   port=Port,
                   clients=Clients,
                   seconds=Seconds},
    State2 = start_test(State),
    {ok, running, State2}.

running(cancel, _, State) ->
    State2 = stop_test(State),
    {stop, ok, cancel, State2}.

handle_event(Event, StateName, _State) ->
    % crash an unknown event
    exit({error, {invalid_event, {Event, StateName}}}).

handle_sync_event(Event, From, StateName, _State) ->
    % crash an unknown sync event
    exit({error, {invalid_sync_event, {Event, From, StateName}}}).

handle_info(timer_done, running, State) ->
    stop_test(State),
    {stop, normal, #state{}}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_test(State) ->
    error_logger:info_msg("Testing ~p~n", [State]),
    {ok, _} = wsdemo_logger:start_link(State#state.db),
    {ok, _} = wsdemo_server_logger:start_link(State#state.host,
                                              State#state.port,
                                              State#state.server_name),
    {ok, _} = wsdemo_stats:start_link(State#state.host,
                                      State#state.port,
                                      State#state.clients),

    erlang:send_after(timer:seconds(State#state.seconds),
                                    self(),
                                    timer_done),
    % TODO: Run the test
    State.

stop_test(State) ->
    ok = wsdemo_stats:stop(),
    ok = wsdemo_server_logger:stop(),
    ok = wsdemo_logger:close(),
    % TODO: Stop the test
    State.
