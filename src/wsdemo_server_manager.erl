-module(wsdemo_server_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {port}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, start_link/0, start_server/1, stop_server/0,
         memusage/0, connections/1, status/0]).

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

memusage() ->
    gen_server:call({global, ?SERVER}, memusage, infinity).    

-spec connections(HostAndPort :: iolist()) -> integer().
connections(HostAndPort) ->
    gen_server:call({global, ?SERVER}, {connections, HostAndPort},
                    infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    Port = open_port({spawn, "python priv/server_manager.py"},
                     [{line, 255}, stream, binary]),
    {ok, #state{port=Port}}.

handle_call({start_server, ServerName}, _From, State) ->
    Reply = call_python(State#state.port, ["start:", ServerName]),
    {reply, Reply, State};
handle_call(stop_server, _From, State) ->
    Reply = call_python(State#state.port, "stop"),
    {reply, Reply, State};
handle_call(status, _From, State) ->
    {reply, call_python(State#state.port, "status"), State};
handle_call(memusage, _From, State) ->
    {reply, call_python_int(State#state.port, "memusage"), State};
handle_call({connections, HostAndPort}, _From, State) ->
    {reply,
     call_python_int(State#state.port,
                     ["connections:", HostAndPort]),
     State};
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
message_key() ->
    list_to_binary(erlang:ref_to_list(make_ref())).

call_python_int(Port, Msg) ->
    case call_python(Port, Msg) of
        {message, IntString} ->
            {ok, list_to_integer(binary_to_list(IntString))};
        Other ->
            Other
    end.

call_python(Port, Msg) ->
    call_python(Port, Msg, 1000).

call_python(Port, Msg, Timeout) ->
    MessageKey = message_key(),
    true = port_command(Port, [MessageKey, ":", Msg, "\n"]),
    collect_response(Port, MessageKey, Timeout).

collect_response(Port, MessageKey, Timeout) ->
    %% TODO: Add message refs
    KeyLen = size(MessageKey),
    
    receive
        {Port, {data, {eol, <<MessageKey:KeyLen/binary, ":__message__:", Msg/binary>>}}} ->
            {message, Msg};
        {Port, {data, {eol, <<MessageKey:KeyLen/binary, ":__error__:", Error/binary>>}}} ->        
            {error, Error};
        Other ->
            exit({MessageKey, KeyLen, Other})
    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after Timeout -> 
            timeout
    end.

