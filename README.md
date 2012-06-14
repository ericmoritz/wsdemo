# wsdemo

A Cowboy Websocket demo

This demo is intended to follow along with
the [Million-user Comet Application](http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1)

## Demo

Compile:

    ./rebar get-deps compile



Follow the "Tuning the Linux Kernel for many tcp connections" instructions in Part One of the 
[Million-user Comet Application](http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1)
to increase your available connection counts.

Start the server:

      sudo bash
      ulimit -n 999999
      erl -pa ebin deps/*/ebin -s wsdemo

Start floodtest:

      sudo bash
      ulimit -n 999999
      ./runtest results/erlang.bin 300 10000

Compile the results:

     ./compile_stats results/erlang.bin

I am planning on rerunning these benchmarks with the new stats collecting code using 64bit m1.large instances.
The [previous results](https://github.com/ericmoritz/wsdemo/blob/results-v1/results.md) are still available
