-module(wsdemo_master_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).
-record(state, {config, current, servers, callback, runner}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, run_suite/2, run_suite/3, run_suite/6]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% state handlers
-export([idle/2, warmup/2, fulltest/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

run_suite(Servers, Clients, Seconds) ->
    run_suite(Servers, "/tmp", "127.0.0.1", 8000, Clients, Seconds).

run_suite(Servers, DataRoot, Host, Port, Clients, Seconds) ->
    Callback = fun(R) ->
                       error_logger:info_msg("~w~n", [R])
               end,
    run_suite(Callback, {Servers, DataRoot, Host, Port, Clients, Seconds}).

run_suite(Callback, {_Servers, _DataRoot, _Host, _Port, _Clients, _Seconds} = Config) ->
    gen_fsm:send_event(?SERVER, {run_suite, Callback, Config}).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    process_flag(trap_exit, true),

    {ok, idle, #state{}}.

idle({run_suite, Callback, Config}, State) ->
    Servers = element(1, Config),
    State2 = State#state{callback=Callback,
                         servers=Servers,
                         config=Config},
    State3 = do_warmup(State2),
    {next_state, warmup, State3}.

warmup(run_fulltest, State) ->
    State2 = do_fulltest(State),
    {next_state, fulltest, State2}.

fulltest(next_server, State) ->
    handle_event(next_server, fulltest, State).

handle_event(cooldown, _StateName, State) ->
    % stop the current server
    wsdemo_server_manager:stop_server(),

    % schedule the end of the cooldown phase
    erlang:send_after(timer:seconds(15), self(), next_server),

    error_logger:info_msg("Stopping ~s and cooling down for 15s~n", [State#state.current]),
    {next_state, cooldown, State};
handle_event(next_server, _StateName, #state{callback=CB, servers=[_|Rest]} = State) ->
    case Rest of
        [] ->
            CB(done),
            % we are out of servers to test, return to the idle state
            {next_state, idle, #state{}};
        Rest ->
            % pop off the current server and move to the warm up phase
            % for the next server
            State2 = do_warmup(State#state{servers=Rest}),
            {next_state, warmup, State2}
    end.

handle_sync_event(Event, From, StateName, _State) ->
    % crash an unknown event
    exit({error, {invalid_event, {Event, From, StateName}}}).


% In 'warmup', a normal exit of the runner proc moves us to 'fulltest'
handle_info({'EXIT', Pid, normal}, warmup, #state{runner=Pid} = State) ->
    warmup(run_fulltest, State);
% In 'fulltest', a normal exit of the runner proc moves us to 'cooldown'
handle_info({'EXIT', Pid, normal}, fulltest, #state{runner=Pid} = State) ->
    handle_event(cooldown, fulltest, State);
% In any phase, an abnormal exit of the runner proc moves us to 'cooldown'
handle_info({'EXIT', Pid, _Reason}, StateName, #state{runner=Pid} = State) ->
    handle_event(cooldown, StateName, State);
% A next_server message is sent when the cooldown is over
handle_info(next_server, cooldown, State) ->
    % skip the test if the runner crashes
    handle_event(next_server, cooldown, State).

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
start_server(ServerName) ->
    error_logger:info_msg("Starting ~s and waiting 15s for server init~n", [ServerName]),
    {message, <<"started">>} = wsdemo_server_manager:start_server(ServerName),

    % wait for the server to come up... TODO: Send a synchronous ping to the server
    timer:sleep(timer:seconds(15)).

do_atest(DBName, Modifier, State) ->
    [Server|_] = State#state.servers,
    {_, DBRoot, Host, Port, Clients, Seconds} = State#state.config,
    DB = filename:join(DBRoot, DBName),

    Clients2 = trunc(Clients * Modifier),
    Seconds2 = trunc(Seconds * Modifier),
                  
    error_logger:info_msg("Testing ~p~n", [[{server, Server},
                                            {db, DB},
                                            {host, Host},
                                            {port, Port},
                                            {clients, Clients2},
                                            {seconds, Seconds2}]]),

    {ok, Pid} = wsdemo_runner_fsm:start_link(DB, Host, Port, Clients, Seconds2),
    State#state{current=Server, runner=Pid}.
    
do_warmup(State) ->
    [Server|_] = State#state.servers,
    start_server(Server),
    do_atest(Server ++ "-warmup", 0.1, State).
    

do_fulltest(State) ->
    [Server|_] = State#state.servers,
    do_atest(Server, 1, State).
