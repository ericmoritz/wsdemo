# wsdemo

A Websocket competition.  Will your favorite platform come out on top?
If you think your platform of choice was poorly represented, "submit a
pull request™"

## How do you include a contestant?

Just create the "Hello, World!" of socket servers, an echo server that
speaks WebSocket version 13 (13 is the only version my client supports).

First write your implementation in place it in the `competition/`
directory.

Added any instructions needed to install dependencies in
`configure_ubuntu-12.04.sh`.  Add any build instructions to
`competition/Makefile` if your project needs to be compiled

Include a bash script in `competition/` that will start your server in
the most ideal way, with any optimization flags needed, etc. The name
of the script should take the following format:

    wsdemo-start-{language}-{platform}.sh

For instance if you wrote an echo server using `bash` and `nc` the
filename would be `competition/wsdemo-start-bash-nc.sh`.  

Finally add an entry into `./bin/run_all_tests.sh` for instance, for
our fictitious bash/nc server, the addition would be:

    do_test "bash-nc"

## Running the benchmark

It is best to run the benchmark client on a separate machine than the
servers.  

Using two brand new Ubuntu 12.04 servers, run the
`configure_ubuntu-12.04.sh` script on each. This script will destructively
alter each server so use caution and review the script before running
it.

If you want to use a different OS, use `configure_ubuntu-12.04.sh` as a
template for configuring your system of choice.

Next compile the code on both machines:

      make

To run a single benchmark on a server do the following:

    ./runtest db_path duration hostname port client_count

For instance for a benchmark of 192.168.1.5:8000 of 1000 clients lasting 60
seconds with with the data stored at `data/myserver` the command would
be:

    ./runtest data/myserver 60 192.168.1.5 8000 1000

To do the full benchmark of all the servers, on the client machine run:

    sudo bash
    ulimit -n 999999
    ./bin/run_all_tests.sh

Then follow the instructions.  The entire benchmark takes over 60
minutes of babysitting.

You may see client crashes while the tests are running. Crashes due to
`connection_timeout` are due to a server unable to accept the incoming
TCP connection.  This is not cause for alarm. If you see anything
crashing because of a `/{error, .+}/`, this is an unexpected crash of
the client and should be investigated.

## Exporting the data

The resulting `leveldb` databases will be placed in `data/`.  The
events are stored as binary Erlang terms in the database so you will
need to export the events to use them.

There are two scripts to do that. `./bin/compile_all_stats.sh` and
`./bin/convert_all_stats.sh`

`./bin/compile_all_stats.sh` creates three `.csv` tables:

   * counts.csv - the sums of all the events
   * handshake_times.csv - timestamp, elapsed usecs for each handshake
   * message_latencies.csv - timestamp, elapsed usecs for each message

`./bin/convert_all_stats.sh` dumps the raw events as a .csv tables in
the `results/` directory..

The events data has the following fields:

    timestamp, type, client_id, event_id, event

   * timestamp - The timestamp of the event
   * type - the server type
   * client_id - A string that identifies the client
   * event_id - A string that links start and end events together
   * event - A string that identifies the event

The `event_id` is used to pair `ws_init` and `ws_onopen` events to
calculate the elapsed handshake time and the `event_id` is used to
pair `send_message` events to `recv_message` events to calculate the
elapsed message time.  The `event_id` for `'EXIT'` events take the
format of `{pid(), Reason :: any()}` which is still unique for every
`'EXIT'` event but serves a double purpose of allowing you to know the
cause of the crash.  Here are the expected reasons:

    connection_timeout - The client could not establish a connection
                         with the server in under 2 seconds
    normal - The server closed the connection.  This should never
             happen for this benchmark
    {error, Reason} - Server or Client protocol error. It is most
                      likely a client error as my WS client is a very
                      bare bones version 13 client.

Any reason other than this is an unexpected error and should be
[reported](https://github.com/ericmoritz/wsdemo/issues) as an issue.
If you see a `{error, Reason}` you should probably file a bug report
as well unless you can determine it is a server issue.

