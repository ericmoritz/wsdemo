-module(wsdemo_bench_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % complete the cluster if the app var exists
    wsdemo_bench:build_cluster(),
    wsdemo_bench_sup:start_link().

stop(_State) ->
    ok.

