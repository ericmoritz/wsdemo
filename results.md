I started two m1.medium AWS instances.  One one side I started one of the 
servers and the other side I ran the following code:

    wsdemo_stats:start_link("ec2-50-17-58-219.compute-1.amazonaws.com",
                            8000, 10000),
    spawn(fun() -> timer:sleep(5 * 60 * 1000),
                   error_logger:info_msg("Result: ~p~n", 
                   [wsdemo_stats:stats()]) end).

After 5 minutes of running, the following stats are outputed.

## stat definitions

### connection_time

Elapsed time in microseconds between the opening of the client TCP socket
is opened and the handshake is finished.

### latency

Elapsed time in microseconds for a message send from the client to echo
back to the client by the serve.

### connections

Number of connections

### disconnections

Number of server disconnects

### messages

Total number of messages received by the clients

### ticks

Number of ticks received by the clients.  ticks are sent every second per
connection.

## crashes

Number of crashed clients

