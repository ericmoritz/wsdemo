-module(wsdemo_logger).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {fh}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, event/1, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Filename) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Filename], []).

event(Term) ->
    gen_server:cast(?SERVER, {event, {erlang:now(), Term}}).

%% Finish up all the pending messages and then stop the server
stop() ->
    gen_server:call(?SERVER, stop, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Filename]) ->
    {ok, FH} = file:open(Filename, [write,binary]),
    {ok, #state{fh=FH}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({event, Data}, State) ->
    FH = State#state.fh,
    Bin = term_to_binary(Data),
    BinSize = size(Bin),
    file:write(FH, <<BinSize:16, Bin/binary>>),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{fh=FH}) ->
    file:close(FH),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

