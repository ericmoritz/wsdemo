# wsdemo

A Cowboy Websocket demo

This demo is intended to follow along with
the [Million-user Comet Application](http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1)

## Demo

Compile:

    ./rebar get-deps compile

Start Server:
      
      erl -pa ebin deps/*/ebin
      1> wsdemo:start(8000).

Follow the instructions in Part One of the Million-user Comet Application
to increase your available connection counts.

Start floodtest:

      erl -pa ebin 
      1> wsdemo_stats:start("localhost", 8000, 10000).

That will run forever.

