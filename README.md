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

wsdemo_bench utilizes [supervisord](http://supervisord.org/) to start
and stop the servers while running the tests.  In order to include
your server in the test you need to create a supervisord ini file in
`competition/servers`.  

    [program:%(server_name)s]
    command=%(command)s
    autostart=false
    autorestart=false
    startretries=0
    stopasgroup=true
    killasgroup=true
    
The server name must follow the following format:

    {language}-{platform}

For instance if you wrote an echo server using `bash` and `nc` the
srver name would be `bash-nc`.  You can add additional demarcations if
needed.  For instance the Python tornado example has a single threaded
and a multiprocessor version which go by the name `python-tornado-1`
and `python-tornado-N`.

Finally you need to add your server to the server list in
`wsdemo_bench.app.src` configuration file.


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


## Running the entire suite on your own servers

There are two components in wsdemo_bench. The first component is
`supervisord`

wsdemo_bench communications with Supervisord to start and stop each
server before each benchmark.

On the machine that you are running the servers on do the following:

    sudo bash
    ulimit -n 999999
    cd competition
    supervisord

You can monitor supervisord by using it's `supervisorctl` command:

    competition/ $ supervisorctl


Next, on the client machine create a `config/demo.config` file:

    [
        {sasl, [
            {sasl_error_logger, {file, LogFile}}]},

        {wsdemo_bench, [
                    {host, Ip},
                    {port, 8000},
                    {db_root, DbRoot},
                    {clients, 100},
                    {seconds, 10},
                    {supervisord, {ServerHost, 9001}}]}].
    
This will configure the test using 100 clients for
10 seconds each test. 

Replace `Ip`, `DbRoot` and  `ServerHost` with the correct values.

To do the full benchmark of all the servers, on the client machine run:

    sudo bash
    ulimit -n 999999
    ./bin/run_all_tests.sh -config config/demo.config

If all goes well, you should see the suite running in front of your eyes.

You may see client crashes while the tests are running. Crashes due to
`connection_timeout` are due to a server unable to accept the incoming
TCP connection.  This is not cause for alarm. If you see anything
crashing because of a `/{error, .+}/`, this is an unexpected crash of
the client and should be investigated.

## Suite config

Here is the full spec for the wsdemo_bench config:

     [
        {host, Host :: string()},
        {port, Port :: integer()},
        {db_root, DbRoot :: directory()},
        {supervisord, {Host, 9001}},
        {clients, number()}, % defaults to 10000
        {seconds, number()}, % defaults to 600
        {servers, [ServerName1::string(), ..., ServerNameN::string()]} % defaults to full suite
     ]        


## Exporting the data

The resulting `leveldb` databases will be placed in you configured
`db_root`.  The events are stored as binary Erlang terms in the
database so you will need to export the events to use them.

There are two scripts to do that. `./bin/compile_all_stats.sh` and
`./bin/convert_all_stats.sh`

`./bin/compile_all_stats.sh` creates three `.csv` tables:

   * counts.csv - the sums of all the events
   * handshake_times.csv - timestamp, elapsed usecs for each handshake
   * message_latencies.csv - timestamp, elapsed usecs for each message

`./bin/convert_all_stats.sh` dumps the raw events as a .csv tables in
the `results/` directory.

The events data has the following fields:

    timestamp, type, client_id, event_key, event_data

   * timestamp - The timestamp of the event
   * type - the server type
   * client_id - A string that identifies the client
   * event_key - The event type
   * event_data - event specific data

The `event_data` is used to pair `ws_init` and `ws_onopen` events to
calculate the elapsed handshake time and the `event_data` is used to
pair `send_message` events to `recv_message` events to calculate the
elapsed message time.  The `event_data` for `'EXIT'` events take the
format of `{pid(), Reason :: any()}`. These are some of the expected
reasons for a client crash:

    connection_timeout - The client could not establish a connection
                         with the server in under 2 seconds
    normal - The server closed the connection.  This should never
             happen for this benchmark
    {error, Reason} - Server or Client protocol error. It is most
                      likely a client error as my WS client is a very
                      bare bones version 13 client.

Any reason other than these are an unexpected error and should be
[reported](https://github.com/ericmoritz/wsdemo/issues) as an issue.
If you see a `{error, Reason}` you should probably file a bug report
as well unless you can determine it is a server issue.

