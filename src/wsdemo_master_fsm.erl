-module(wsdemo_master_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).
-record(state, {config, servers, callback, runner}).

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
    error_logger:info_msg("Running suite ~p~n", [config_to_propslist(Config)]),
    Servers = element(1, Config),
    State2 = State#state{callback=Callback,
                         servers=Servers,
                         config=Config},

    handle_event(run_warmup, idle, State2).

warmup(run_fulltest, State) ->
    handle_event(run_fulltest, warmup, State).

fulltest(next_server, State) ->
    handle_event(next_server, fulltest, State).

handle_event(run_warmup, StateName, State) ->
    case do_warmup(State) of
        {ok, State2} ->
            {next_state, warmup, State2};
        _Error ->
            % could not start the server, move on
            handle_event(cooldown, StateName, State)
    end;
handle_event(run_fulltest, _StateName, State) ->
    State2 = do_fulltest(State),
    {next_state, fulltest, State2};    
handle_event(cooldown, _StateName, State) ->
    % stop the current server
    wsdemo_server_manager:stop_server(current_server(State)),

    % schedule the end of the cooldown phase
    erlang:send_after(timer:seconds(15), self(), next_server),

    error_logger:info_msg("Stopping ~s and cooling down for 15s~n", [current_server(State)]),
    {next_state, cooldown, State};
handle_event(next_server, StateName, #state{callback=CB, servers=[_|Rest]} = State) ->
    case Rest of
        [] ->
            CB(done),
            % we are out of servers to test, return to the idle state
            {next_state, idle, #state{}};
        Rest ->
            % pop off the current server and move to the warm up phase
            % for the next server
            handle_event(run_warmup, StateName, State#state{servers=Rest})
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
current_server(#state{servers=[Current|_]}) ->
    Current.

config_to_propslist({Servers, DataRoot, Host, Port, Clients, Seconds}) ->
    [{servers, Servers},
     {db_root, DataRoot},
     {host, Host},
     {port, Port},
     {clients, Clients},
     {seconds, Seconds}].

do_atest(DBName, Modifier, State) ->
    Server = current_server(State),
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

    {ok, Pid} = wsdemo_runner_fsm:start_link(Server, DB, Host, Port, Clients, Seconds2),
    State#state{runner=Pid}.
    
do_warmup(State) ->
    Server = current_server(State),

    error_logger:info_msg("Starting ~s and waiting 15s for server init~n", [Server]),
    case wsdemo_server_manager:start_server(Server) of
        ok ->
            timer:sleep(timer:seconds(15)),
            {ok, do_atest("warmup", 0.1, State)};
        Error ->
            error_logger:error_msg("Cound not start server ~s, moving onto next server~nReason: ~p~n", [Error]),
            Error
    end.
    

do_fulltest(State) ->
    [Server|_] = State#state.servers,
    do_atest(Server, 1, State).
