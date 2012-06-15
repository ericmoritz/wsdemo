-module(wsdemo_logger).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, event/1, close/0, foldl/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [Filename], []).

event(Event) ->
    Now = erlang:now(),
    gen_server:cast(?MODULE, {event, 
                              {
                                % Key
                                {Now, make_ref(), self()},
                                % Val
                                {Now, Event}
                               }}).

close() ->
    gen_server:call(?MODULE, close).

foldl(Fun, Acc0, LogFile) ->
    {ok, Ref} = eleveldb:open(LogFile, []),

    Reducer = fun({_, VBin}, A) ->
                      V = binary_to_term(VBin),
                      Fun(V, A)
              end,
    eleveldb:fold(Ref, Reducer, Acc0, []).

%% Internal
init([Filename]) ->                             
    ok = eleveldb:destroy(Filename, []),
    {ok, Ref} = eleveldb:open(Filename,
                              [{create_if_missing, true}]),
    {ok, Ref}.

handle_call(close, _From, Ref) ->
    {stop, normal, ok, Ref};
handle_call(_Msg, _From, Ref) ->
    {noreply, Ref}.

handle_cast({event, {Key, Event}}, Ref) ->
    KeyBin = term_to_binary(Key),
    EventBin = term_to_binary(Event),
    ok = eleveldb:put(Ref, KeyBin, EventBin, []),
    {noreply, Ref}.

handle_info(_Msg, Ref) ->    
    {noreply, Ref}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
    
