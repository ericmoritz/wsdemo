-module(wsdemo_bench).

-export([start/0, run_sync/0, build_cluster/0, build_cluster/1]).

start() ->
    application:start(wsdemo_bench).


build_cluster() ->
    case application:get_env(nodes) of
        {ok, Nodes} ->
            build_cluster(Nodes),
            global:sync(),
            ok;
        undefined ->
            undefined
    end.

build_cluster(undefined) ->
    ok;
build_cluster([]) ->
    ok;
build_cluster([Node|Rest]) ->
    pong = net_adm:ping(Node),
    build_cluster(Rest).

run_sync() ->
    Self = self(),
    Ref = make_ref(),

    CB = fun(Reason) ->
                 Self ! {Ref, Reason}
         end,

    [Servers, DBRoot, Host, Port, Clients, Seconds] = 
        get_keys_or_not([servers, db_root, host, port, clients, seconds]),

    wsdemo_master_fsm:run_suite(CB, {Servers, DBRoot,
                                     Host, Port, Clients, Seconds}),
    % wait for the suite to finish.
    receive
        {Ref, Reason} ->
            Reason
    end.

get_keys_or_not(Keys) ->
    get_keys_or_not([], Keys).

get_keys_or_not(Acc,[]) ->
    lists:reverse(Acc);
get_keys_or_not(Acc,[Key|Rest]) ->
    case application:get_env(wsdemo_bench, Key) of
        {ok, Val} ->
            get_keys_or_not([Val|Acc], Rest);
        undefined ->
            {error, {application_env, Key, not_found}}
    end.

            

    
    
