-module(wsdemo_master_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).
-record(state, {config, current, servers, callback}).

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
    ok = do_warmup(State2),
    {next_state, warmup, State2}.

warmup(run_fulltest, State) ->
    do_fulltest(State),
    {next_state, fulltest, State}.

fulltest(next_server, #state{callback=CB, servers=[Previous|Rest]} = State) ->
    % stop the previous server
    stop_server(Previous),

    case Rest of
        [] ->
            CB(done),
            % we are out of servers to test, return to the idle state
            {next_state, idle, #state{}};
        Rest ->
            % pop off the current server and move to the warm up phase
            % for the next server
            State2 = State#state{servers=Rest},
            do_warmup(State2),
            {next_state, warmup, State2}
    end.

handle_event(Event, StateName, _State) ->
    % crash an unknown event
    exit({error, {invalid_event, {Event, StateName}}}).

handle_sync_event(Event, From, StateName, _State) ->
    % crash an unknown event
    exit({error, {invalid_event, {Event, From, StateName}}}).

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
start_server(ServerName) ->
    error_logger:info_msg("Starting ~s~n", [ServerName]),
    {message, "started"} = wsdemo_server_manager:start_server(ServerName),

    % wait for the server to come up... TODO: Send a synchronous ping to the server
    timer:sleep(5000).

stop_server(ServerName) ->
    error_logger:info_msg("Stopping Server ~s~n", [ServerName]),
    wsdemo_server_manager:stop_server().
    
next_server() ->
    gen_fsm:send_event(?SERVER, next_server).

%% switch from warmup to the full test
run_fulltest() ->
    gen_fsm:send_event(?SERVER, run_fulltest).

do_atest(Callback, DBName, TimeModifier, State) ->
    [Server|_] = State#state.servers,
    {_, DBRoot, Host, Port, Clients, Seconds} = State#state.config,
    DB = filename:join(DBRoot, DBName),

    Seconds2 = trunc(Seconds * TimeModifier),
                  
    error_logger:info_msg("Testing ~p~n", [[{server, Server},
                                            {db, DB},
                                            {host, Host},
                                            {port, Port},
                                            {clients, Clients},
                                            {seconds, Seconds2}]]),
    ok = wsdemo_runner_fsm:run(Callback,
                               DB, Host, Port, Clients, Seconds2).
    
do_warmup(State) ->

    WarmupCallback = fun(done) ->
                             % send the full test event
                             run_fulltest();
                        (cancel) ->
                             pass
                 end,

    [Server|_] = State#state.servers,
    start_server(Server),
    do_atest(WarmupCallback, Server ++ "-warmup", 0.1, State).
    

do_fulltest(State) ->
    FulltestCB = fun(done) ->
                         % send the next_server event
                         next_server();
                    (cancel) ->
                         pass
                 end,
    [Server|_] = State#state.servers,
    do_atest(FulltestCB, Server, 1, State).
