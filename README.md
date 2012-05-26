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
      erl -pa ebin deps/*/ebin
      1> wsdemo:start(8000).

Start floodtest:

      sudo bash
      ulimit -n 999999
      erl -pa ebin 
      1> wsdemo_stats:start("localhost", 8000, 10000).

You will see the stats printed out as with the part one of C1M Comet demo.

