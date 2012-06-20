-module(wsdemo_server_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {port}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, start_link/0, start_server/1, stop_server/0, status/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start() ->
    start_link().

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

start_server(ServerName) ->
    gen_server:call({global, ?SERVER}, {start_server, ServerName}, infinity).

stop_server() ->
    gen_server:call({global, ?SERVER}, stop_server, infinity).

status() ->
    gen_server:call({global, ?SERVER}, status, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    Port = open_port({spawn, "python priv/server_manager.py"},
                     [{line, 255}, stream]),
    {ok, #state{port=Port}}.

handle_call({start_server, ServerName}, From, State) ->
    Reply = call_python(From, State#state.port, ["start ", ServerName]),
    {reply, Reply, State};
handle_call(stop_server, From, State) ->
    Reply = call_python(From, State#state.port, "stop"),
    {reply, Reply, State};
handle_call(status, From, State) ->
    {reply, call_python(From, State#state.port, "status"), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
call_python(Dest, Port, Msg) ->
    call_python(Dest, Port, Msg, 1000).

call_python(Dest, Port, Msg, Timeout) ->
    true = port_command(Port, [Msg, "\n"]),
    collect_response(Port, Timeout).

collect_response(Port, Timeout) ->
    collect_response(Port, Timeout, [], []).

collect_response(Port, Timeout, RespAcc, LineAcc) ->
    receive
        {Port, {data, {eol, "__message__:" ++ Msg}}} ->
            {message, Msg};
        {Port, {data, {eol, "__error__:" ++ Error}}} ->        
            {error, Error};
        {Port, {data, {eol, Line}}} ->        
            {error, {unknown_line, Line}}
    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after Timeout -> 
            timeout
    end.

