-module(wsdemo_logger).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/1, event/1, close/0, foldl/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new(Filename) ->
    case file:delete(Filename) of
        {error, enoent} ->
            pass;
        ok ->
            pass;
        {error, Reason} ->
            exit({error, Reason})
    end,

    % We deleted any existing log so anything other than
    % {ok, Log} is something to crash over.
    {ok, _Log} = disk_log:open([{name, ?MODULE},
                                {file, Filename},
                                {size, infinity}]),
    ok.

event(Term) ->
    disk_log:alog(?MODULE, {erlang:now(), Term}).

close() ->
    % Stop clients from send new message
    disk_log:block(?MODULE), 
    % Close the log
    disk_log:close(?MODULE).


foldl(Fun, Acc, Filename) ->
    {ok, DL} = disk_log:open([{file, Filename},
                              {name, {?MODULE, make_ref()}},
                              {mode, read_only}]),
    foldl(Fun, Acc, DL, start).

%% Internal
foldl(Fun, Acc, DL, Cont) ->
    case disk_log:chunk(DL, Cont) of 
        eof ->
            Acc;
        {Cont2, Terms} ->
            Acc0 = lists:foldl(Fun, Acc, Terms),
            foldl(Fun, Acc0, DL, Cont2)
    end.
            

    
                             
