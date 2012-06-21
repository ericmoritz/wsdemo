-module(wsdemo_bench_master).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {config, current, servers}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    init(application:get_all_env());
init(Config) ->
    process_flag(trap_exit, true),
    Servers = proplists:get_value(servers, Config),
    io:format("Starting ~p~n", [Servers]),
    {ok, #state{config=Config, servers=Servers}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{servers=[ServerName|Rest]}=State) ->
    % Fire up the first server
    start_server_and_runner(ServerName, State),
    {noreply, State#state{servers=Rest, current=ServerName}};
handle_info({'EXIT', _Pid, normal}, #state{servers=[]}=State) ->
    % No more servers, we're done
    io:format("Stopping ~s~n", [State#state.current]),
    wsdemo_server_manager:stop_server(),
    io:format("Done.", []),
    {stop, normal, []};
handle_info({'EXIT', _Pid, normal}, #state{servers=[ServerName|Rest]}=State) ->
    % Stop the existing server
    io:format("Stopping ~s~n", [State#state.current]),
    wsdemo_server_manager:stop_server(),
    
    % fire up the next server
    start_server_and_runner(ServerName, State),
    {norely, State#state{servers=Rest, current=ServerName}};
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, {error, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_server_and_runner(ServerName, #state{config=Config}) ->
    DataRoot = proplists:get_value(data_root, Config),
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    Clients = proplists:get_value(clients, Config),
    Seconds = proplists:get_value(seconds, Config),
    LogFile = filename:join(DataRoot, ServerName),

    io:format("Testing ~s~n", [ServerName]),

    % start the server
    {message, "started"} = wsdemo_server_manager:start_server(ServerName),

    % wait for the server to stop
    timer:sleep(5000),

    % start the runner
    {ok, _Pid} = wsdemo_runner:start_link(LogFile,
                                          Host, Port, Clients, Seconds),
    ok.

