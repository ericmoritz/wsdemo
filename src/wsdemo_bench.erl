-module(wsdemo_bench).

-export([start/0]).


start() ->
    application:start(wsdemo_bench).
