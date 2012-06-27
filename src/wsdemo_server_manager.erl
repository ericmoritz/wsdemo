-module(wsdemo_server_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {host, port}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_link/1, start_server/1, stop_server/1,
         list_servers/0, memusage/1, connections/1, status/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-type error() :: {error, any()}.
-type status() :: stopped | starting | running | backoff | stopping | exited | fatal | unknown.
-type proplists() :: [{Key1 :: any(), Value1 :: any()}].

-spec start_link() -> {ok, pid()} | error().
start_link() ->
    {ok, Supervisord} = application:get_env(wsdemo_bench, supervisord),
    start_link(Supervisord).

-spec start_link({Host :: string(), Port :: integer()}) -> {ok, pid()} | error().
start_link(Supervisord) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Supervisord], []).

-spec start_server(string()) -> ok | error().
start_server(ServerName) ->
    gen_server:call(?SERVER, {start_server, ServerName}).

-spec stop_server(string()) -> ok | error().
stop_server(ServerName) ->
    gen_server:call(?SERVER, {stop_server, ServerName}).

-spec status(string()) -> {ok, status()} | error().
status(ServerName) ->
    gen_server:call(?SERVER, {status, ServerName}).

-spec memusage(string()) -> {ok, integer()} | error().
memusage(ServerName) ->
    gen_server:call(?SERVER, {memusage, ServerName}).    

-spec connections(HostAndPort :: iolist()) -> {ok, integer()} | error().
connections(HostAndPort) ->
    gen_server:call(?SERVER, {connections, HostAndPort}).

-spec list_servers() -> {ok, [{ServerName :: string(), proplists()}]} | error().
list_servers() ->
    gen_server:call(?SERVER, list_servers).
    
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{Host, Port}]) ->
    {ok, #state{host=Host, port=Port}}.

handle_call(list_servers, _From, State) ->
    {reply, rpc_list_servers(State), State};
handle_call({start_server, ServerName}, _From, State) ->
    {reply, rpc_start_server(ServerName, State), State};
handle_call({stop_server, ServerName}, _From, State) ->
    {reply, rpc_stop_server(ServerName, State), State};
handle_call({status, ServerName}, _From, State) ->
    {reply, rpc_status(ServerName, State), State};
handle_call({memusage, ServerName}, _From, State) ->
    {reply, rpc_call('wsdemo_monitor.memusage', [ServerName], State), State};
handle_call({connections, HostAndPort}, _From, State) ->
    {reply, rpc_call('wsdemo_monitor.connections', [HostAndPort], State), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
supervisord_status(Status) ->
%https://github.com/Supervisor/supervisor/blob/master/supervisor/states.py#L4
%class ProcessStates:
%    STOPPED = 0
%    STARTING = 10
%    RUNNING = 20
%    BACKOFF = 30
%    STOPPING = 40
%    EXITED = 100
%    FATAL = 200
%    UNKNOWN = 1000
    case Status of
        0 -> stopped;
        10 -> starting;
        20 -> running;
        30 -> backoff;
        40 -> stopping;
        100 -> exited;
        200 -> fatal;
        1000 -> unknown
    end.

rpc_status(ServerName, State) ->
    case rpc_call('supervisor.getProcessInfo', [ServerName], State) of
        {ok, {struct, Props}} ->
            supervisord_status(proplists:get_value(state, Props));
        Error ->
            Error
    end.

procinfo_to_server_list({array, ProcInfoList}) ->
    procinfo_to_server_list(ProcInfoList, []).

procinfo_to_server_list([{struct, ProcInfo}|Rest], Accum) ->
    ServerName = proplists:get_value(name, ProcInfo),
    Status = supervisord_status(proplists:get_value(state, ProcInfo)),
    procinfo_to_server_list(Rest, [{ServerName, Status}|Accum]);
procinfo_to_server_list([], Accum) ->
    lists:reverse(Accum).

rpc_list_servers(State) ->
    case rpc_call('supervisor.getAllProcessInfo', [], State) of
        {ok, ProcessInfo} ->
            {ok, procinfo_to_server_list(ProcessInfo)};
        Error ->
            Error
    end.
rpc_start_server(ServerName, State) -> 
    case rpc_call('supervisor.startProcess', [ServerName], State) of
        {ok, true} ->
            ok;
        Error ->
            Error
    end.

rpc_stop_server(ServerName, State) ->
    case rpc_call('supervisor.stopProcess', [ServerName], State) of
        {ok, true} ->
            ok;
        Error ->
            Error
    end.
       
rpc_call(Method, Args, #state{host=Host, port=Port}) ->
    case xmlrpc:call(Host, Port, "/RPC2", {call, Method, Args}) of
        {ok,{response,[Return]}} ->
            {ok, Return};
        {ok,{response,{fault,Code,Reason}}} ->
            {error, {fault, Code, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

