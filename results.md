# C10K Websocket Test
*NOTE* I am planning on rerunning these benchmarks with the new stats collecting code using 64bit m1.large instances.
This document has been truncated until the new benchmarks can be reran. The [previous results](https://github.com/ericmoritz/wsdemo/blob/results-v1/results.md) are still available

## Methodology

This benchmark starts a new client every 1ms, each client sends a
timestamp message to the server every second and the server echos that
message back to the client. 


I will start two 64 bit m1.large AWS instances running Ubuntu 12.04.
The following command will be ran on both base systems:

    curl https://github.com/ericmoritz/wsdemo/blob/master/configure_ubuntu.sh | bash

Both EC2 instances are configured with the following `etc/sysctl.conf`
file:

    # General gigabit tuning:
    net.core.rmem_max = 16777216
    net.core.wmem_max = 16777216
    net.ipv4.tcp_rmem = 4096 87380 16777216
    net.ipv4.tcp_wmem = 4096 65536 16777216
    net.ipv4.tcp_syncookies = 1
    # this gives the kernel more memory for tcp
    # which you need with many (100k+) open socket connections
    net.ipv4.tcp_mem = 50576   64768   98152
    net.core.netdev_max_backlog = 2500

On one instance I will start each of the servers as follows:

    sudo bash
    ulimit -n 999999
    $SERVER_CMD

and the other instance I will run the `./runtest` command as:

    sudo bash
    ulimit -n 999999
    ./runtest results/${SERVER_TYPE}.dat 300 $SERVER_HOSTNAME 8000 10000

The clients communicate to the server via the server's public IP.


## Statistic data

The data generated is an length encoded Binary Erlang term.  The term is in the following formats

    {timestamp(), {ws_init, pid()}}
    {timestamp(), {ws_onopen, pid()}}
    {timestamp(), {message_send, pid(), ref()}}
    {timestamp(), {message_recv, pid(), ref()}}
    {timestamp(), {'EXIT', pid(), connection_timeout}}
    {timestamp(), {'EXIT', pid(), normal}}
    {timestamp(), {'EXIT', pid(), Reason :: any()}}

To dump the messages to STDOUT run the following command:

    ./cat_stats $FILENAME

To generate aggregate statistics:

   ./compile_stats $FILENAME

## Aggregate Statistic Definitions

### clients

Total number of clients started

### handshakes

Total number of clients that successfully completed the Websocket handshake

### handshake_time

Elapsed time between starting the client and when the websocket handshake finished.

### messages_sent

Total number of messages sent by the client

### messages_recv

Total number of messages received by the client

### message_latency

Elapsed time for a message sent from the client to echo
back to the client by the server.

### connection_timeouts

Total number of connection timeouts.  The timeout is currently hard
coded as 2 seconds.  This means that any TCP connection that takes
longer than 2 seconds to be accepted, this number will be incremented
and the client crashes

### crashes

Number of crashed clients from a reason other than timeout

## Summary

*TODO*  The statistic gathering code has been refactored and the tests need to be reran.  The 
[previous results](https://github.com/ericmoritz/wsdemo/blob/results-v1/results.md) are still available.

