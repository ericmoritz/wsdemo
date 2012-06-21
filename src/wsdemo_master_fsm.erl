-module(wsdemo_master_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).
-record(state, {config, current, servers, callback}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, run_suite/2, run_suite/3, run_suite/6, next_server/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% state handlers
-export([idle/2, running/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

run_suite(Servers, Clients, Seconds) ->
    run_suite(Servers, "/tmp", "localhost", 8000, Clients, Seconds).

run_suite(Servers, DataRoot, Host, Port, Clients, Seconds) ->
    Callback = fun(R) ->
                       error_logger:info_msg("~w~n", [R])
               end,
    run_suite(Callback, {Servers, DataRoot, Host, Port, Clients, Seconds}).

run_suite(Callback, {_Servers, _DataRoot, _Host, _Port, _Clients, _Seconds} = Config) ->
    gen_fsm:send_event(?SERVER, {run_suite, Callback, Config}).

next_server() ->
    gen_fsm:send_event(?SERVER, next_server).

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

    State3 = start_next_test(State2),

    {next_state, running, State3}.

running(next_server, #state{callback=CB, servers=[]}) ->
    CB(done),
    {next_state, idle, #state{}};
running(next_server, State) ->
    State2 = start_next_test(State),
    {next_state, running, State2}.

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
start_next_test(State) ->
    [Server|Rest] = State#state.servers,
    {_, DBRoot, Host, Port, Clients, Seconds} = State#state.config,
    DB = filename:join(DBRoot, Server),

    RunnerCB = fun(done) ->
                       wsdemo_master_fsm:next_server();
                  (cancel) ->
                       pass
               end,
                  
    error_logger:info_msg("Testing ~p~n", [[{server, Server},
                                            {db, DB},
                                            {host, Host},
                                            {port, Port},
                                            {clients, Clients},
                                            {seconds, Seconds}]]),
    ok = wsdemo_runner_fsm:run(RunnerCB,
                               DB, Host, Port, Clients, Seconds),
    State#state{current=Server, servers=Rest}.
