# C10K Websocket Test

## Methodology

This benchmark starts a new client every 1ms, each client sends a
timestamp message to the server every second and the server echos that
message back to the client. 

I started two m1.medium AWS instances.  On one instance I started one of the 
servers and the other instance I ran the `./runtest` command as:

    sudo bash
    ulimit -n 999999
    ./runtest 300 $SERVER_HOSTNAME 8000 10000

The clients communicated to the server via the server's public IP.

After 5 minutes of running, the statistics are dumped to a log file in
`results/`.

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


## stat definitions

### connection_time

Elapsed time between the opening of the client TCP socket
and the websocket handshake is finished.

### latency

Elapsed time for a message sent from the client to echo
back to the client by the server.

### connections

Number of connections

### disconnections

Number of server disconnects

### messages

Total number of messages received by the clients

### connection_timeouts

Total number of connection timeouts.  The timeout is currently hard
coded as 2 seconds.  This means that any TCP connection that takes
longer than 2 seconds to be accepted, this number will be incremented
and the client crashes

### crashes

Number of crashed clients from a reason other than timeout

## Summary

<table>
  <tr>
    <th>Implementation</th>
    <th>Connection Time (mean)</th>
    <th>Latency (mean)</th>
    <th>Messages</th>
    <th>Timeouts</th>
  </tr>
  <tr>
  <td>Erlang</td>
  <td>865ms</td>
  <td>017ms</td>
  <td>2849294</td>
  <td>0</td>
  </tr>
  <tr>
  <td>Java (Webbit)</td>
  <td>567ms</td>
  <td>835ms</td>
  <td>1028390</td>
  <td>157</td>
  </tr>
  <tr>
  <td>Go</td>
  <td>284ms</td>
  <td>18503ms</td>
  <td>2398180</td>
  <td>225</td>
  </tr>
  <tr>
  <td>Node.js</td>
  <td>768ms</td>
  <td>42580ms</td>
  <td>1170847</td>
  <td>4299</td>
  </tr>
  <tr>
  <td>Python (ws4py)</td>
  <td>1561ms</td>
  <td>34889ms</td>
  <td>1052996</td>
  <td>5208</td>
  </tr>
</table>

Both the Python gevent/ws4py implementation and the Node.js websocket
implementation failed hard with around half of the connections hitting
the 2 second TCP connection timeout.

I expected Go to kick Erlang's ass in the performance department but
the message latency was much higher than Erlang's latency and we had
225 unhappy customers. Go only reached C9.775k; close but no cigar.

I did not know what to expect with the Java Webbit implementation.  I
expected it to perform somewhere close to the Go implementation.  The
Webbit implementation did do much better than the Go implementation
but it still had 157 connection timeouts which is unacceptable.

I was very surprised that node.js fell down after 5001 connections and
that gevent fell down at 4792 connections.  Both platforms were
specifically built to for the C10k problem and both platforms could
barely handle C5k. 

## Raw Data

*NOTE:* All times in the raw data section are microseconds (μs).  

### Erlang

Command:

    erl -pa deps/*/ebin ebin +K true -s wsdemo

Results:

    Running test with 10000 clients for 300 seconds
    Result: [{connection_time,
                 [{min,1211},
                  {max,7095662},
                  {arithmetic_mean,865234.6877431907},
                  {geometric_mean,54697.83406753208},
                  {harmonic_mean,7120.650345343649},
                  {median,30830},
                  {variance,2735986827114.5996},
                  {standard_deviation,1654081.8683229072},
                  {skewness,1.9914668412550036},
                  {kurtosis,2.909594628744607},
                  {percentile,
                      [{75,742384},{95,4987533},{99,6799786},{999,7095626}]},
                  {histogram,
                      [{601211,754},
                       {1201211,43},
                       {1801211,37},
                       {2301211,24},
                       {2901211,29},
                       {4001211,47},
                       {5001211,44},
                       {6001211,34},
                       {7001211,13},
                       {8001211,3}]}]},
             {latency,
                 [{min,339},
                  {max,133716},
                  {arithmetic_mean,17183.052529182878},
                  {geometric_mean,10783.698396530715},
                  {harmonic_mean,5192.896345572704},
                  {median,13152},
                  {variance,268282946.31856245},
                  {standard_deviation,16379.345112627747},
                  {skewness,2.1492019675946312},
                  {kurtosis,6.822353838233374},
                  {percentile,[{75,21791},{95,51176},{99,72926},{999,120010}]},
                  {histogram,
                      [{6339,269},
                       {12339,215},
                       {18339,200},
                       {23339,116},
                       {29339,64},
                       {40339,70},
                       {50339,39},
                       {60339,25},
                       {70339,18},
                       {80339,2},
                       {90339,5},
                       {100339,3},
                       {110339,0},
                       {120339,1},
                       {130339,0},
                       {140339,1}]}]},
             {connections,10000},
             {disconnections,0},
             {messages,2849294},
             {connection_timeouts,0},
             {crashes,0}]

### Java (Webbit)

Command:

    cd competition
    make
    ./wsdemo-java.sh

Results:

    Running test with 10000 clients for 300 seconds
    Result: [{connection_time,
                 [{min,3338},
                  {max,2623179},
                  {arithmetic_mean,568653.1643968872},
                  {geometric_mean,239776.61439946695},
                  {harmonic_mean,44919.74165588423},
                  {median,451451},
                  {variance,306300228181.46747},
                  {standard_deviation,553443.9702277615},
                  {skewness,1.2305092991530981},
                  {kurtosis,1.2062614948219368},
                  {percentile,
                      [{75,770172},{95,1664444},{99,2386111},{999,2614860}]},
                  {histogram,
                      [{203338,329},
                       {403338,114},
                       {603338,218},
                       {803338,121},
                       {1003338,52},
                       {1203338,32},
                       {1403338,52},
                       {1603338,47},
                       {1803338,33},
                       {2003338,9},
                       {2203338,4},
                       {2403338,7},
                       {2503338,2},
                       {2703338,8},
                       {2903338,0}]}]},
             {latency,
                 [{min,2404},
                  {max,3125659},
                  {arithmetic_mean,835454.7616731517},
                  {geometric_mean,415058.36966433167},
                  {harmonic_mean,127535.15422317719},
                  {median,1120680},
                  {variance,464519954989.0205},
                  {standard_deviation,681557.0078790332},
                  {skewness,0.2642559196998899},
                  {kurtosis,-0.6775121016726264},
                  {percentile,
                      [{75,1395539},{95,1587208},{99,2847752},{999,3028506}]},
                  {histogram,
                      [{242404,413},
                       {502404,25},
                       {802404,33},
                       {1002404,17},
                       {1202404,46},
                       {1502404,385},
                       {1702404,83},
                       {1902404,6},
                       {2202404,0},
                       {2402404,0},
                       {2602404,2},
                       {2902404,11},
                       {3102404,6},
                       {4002404,1}]}]},
             {connections,4637},
             {disconnections,0},
             {messages,1028390},
             {connection_timeouts,157},
             {crashes,0}]
    
### Go

Command:

    go run competition/wsdemo.go

Results:

    Running test with 10000 clients for 300 seconds
    Result: [{connection_time,
                 [{min,1160},
                  {max,4976523},
                  {arithmetic_mean,284385.766536965},
                  {geometric_mean,25658.973510831038},
                  {harmonic_mean,4682.473567580233},
                  {median,17083},
                  {variance,321545391374.91504},
                  {standard_deviation,567049.7256633805},
                  {skewness,2.8689925005503345},
                  {kurtosis,11.032450263460195},
                  {percentile,
                      [{75,315753},{95,1638363},{99,2280604},{999,4077353}]},
                  {histogram,
                      [{201160,752},
                       {401160,33},
                       {601160,49},
                       {801160,70},
                       {1001160,24},
                       {1201160,10},
                       {1401160,13},
                       {1601160,21},
                       {1801160,26},
                       {2001160,14},
                       {2201160,1},
                       {2401160,8},
                       {2601160,1},
                       {2801160,0},
                       {3001160,0},
                       {3201160,3},
                       {4001160,1},
                       {5001160,2},
                       {6001160,0}]}]},
             {latency,
                 [{min,396},
                  {max,49339496},
                  {arithmetic_mean,18502964.8463035},
                  {geometric_mean,9622411.318087175},
                  {harmonic_mean,95396.41025637122},
                  {median,16967669},
                  {variance,172290802618704.38},
                  {standard_deviation,13125959.112335538},
                  {skewness,0.25510601755314605},
                  {kurtosis,-1.1384153840403934},
                  {percentile,
                      [{75,29263313},{95,40281761},{99,43519694},{999,48055962}]},
                  {histogram,
                      [{5000396,215},
                       {10000396,121},
                       {14000396,95},
                       {19000396,125},
                       {23000396,95},
                       {28000396,87},
                       {40000396,231},
                       {50000396,59},
                       {60000396,0}]}]},
             {connections,9775},
             {disconnections,0},
             {messages,2398180},
             {connection_timeouts,225},
             {crashes,0}]

### Node.js

Command:

    node competition/wsdemo.js

Result:

    Running test with 10000 clients for 300 seconds
    Result: [{connection_time,
                 [{min,1365},
                  {max,15279774},
                  {arithmetic_mean,767702.8326848249},
                  {geometric_mean,116937.90265102463},
                  {harmonic_mean,8679.280802291636},
                  {median,180911},
                  {variance,1353540644549.0554},
                  {standard_deviation,1163417.6569697813},
                  {skewness,3.66537131211259},
                  {kurtosis,30.079010617480094},
                  {percentile,
                      [{75,1246355},{95,2627839},{99,4837405},{999,11155277}]},
                  {histogram,
                      [{501365,633},
                       {901365,45},
                       {1301365,103},
                       {1701365,64},
                       {2101365,50},
                       {2501365,59},
                       {2901365,33},
                       {4001365,25},
                       {5001365,13},
                       {6001365,1},
                       {7001365,0},
                       {8001365,0},
                       {9001365,0},
                       {10001365,0},
                       {11001365,0},
                       {12001365,1},
                       {13001365,0},
                       {14001365,0},
                       {15001365,0},
                       {16001365,1}]}]},
             {latency,
                 [{min,611},
                  {max,97026003},
                  {arithmetic_mean,42579710.93871595},
                  {geometric_mean,29278195.25822909},
                  {harmonic_mean,179784.76163030352},
                  {median,40669676},
                  {variance,608302541426507.6},
                  {standard_deviation,24663790.086410232},
                  {skewness,0.11640391568235428},
                  {kurtosis,-1.0294319744170162},
                  {percentile,
                      [{75,63882416},{95,82661184},{99,91485418},{999,95397096}]},
                  {histogram,
                      [{9000611,99},
                       {18000611,106},
                       {26000611,95},
                       {40000611,203},
                       {50000611,122},
                       {60000611,107},
                       {70000611,123},
                       {80000611,103},
                       {90000611,56},
                       {100000611,14},
                       {110000611,0}]}]},
             {connections,5701},
             {disconnections,0},
             {messages,1170847},
             {connection_timeouts,4299},
             {crashes,0}]

### Python (ws4py)

Command:

    python competition/wsdemo.py

Result:

    Running test with 10000 clients for 300 seconds
    Result: [{connection_time,
                 [{min,1385},
                  {max,31066360},
                  {arithmetic_mean,1560786.1439688716},
                  {geometric_mean,242841.69030740944},
                  {harmonic_mean,13336.537657190269},
                  {median,466631},
                  {variance,8683385612161.933},
                  {standard_deviation,2946758.492337289},
                  {skewness,5.552260242760376},
                  {kurtosis,45.00935160637418},
                  {percentile,
                      [{75,2137563},{95,5306448},{99,11598748},{999,31064785}]},
                  {histogram,
                      [{1101385,678},
                       {2101385,87},
                       {3101385,79},
                       {5001385,118},
                       {6001385,31},
                       {7001385,16},
                       {8001385,0},
                       {9001385,0},
                       {10001385,1},
                       {11001385,7},
                       {12001385,1},
                       {13001385,0},
                       {14001385,0},
                       {15001385,0},
                       {16001385,0},
                       {17001385,5},
                       {18001385,0},
                       {19001385,0},
                       {20001385,0},
                       {21001385,0},
                       {22001385,0},
                       {23001385,0},
                       {24001385,0},
                       {25001385,0},
                       {26001385,0},
                       {27001385,0},
                       {28001385,0},
                       {29001385,0},
                       {30001385,2},
                       {31001385,1},
                       {40001385,2}]}]},
             {latency,
                 [{min,1454},
                  {max,82132060},
                  {arithmetic_mean,34889348.67509728},
                  {geometric_mean,22808521.363542553},
                  {harmonic_mean,668574.9036028518},
                  {median,34752889},
                  {variance,499999310132036.25},
                  {standard_deviation,22360664.34907595},
                  {skewness,0.0915931544450531},
                  {kurtosis,-1.236873451442312},
                  {percentile,
                      [{75,54840581},{95,69809695},{99,77043401},{999,81923621}]},
                  {histogram,
                      [{8001454,158},
                       {16001454,131},
                       {24001454,86},
                       {32001454,111},
                       {40001454,98},
                       {50001454,125},
                       {60001454,149},
                       {70001454,121},
                       {80001454,45},
                       {90001454,4},
                       {100001454,0}]}]},
             {connections,4792},
             {disconnections,0},
             {messages,1052996},
             {connection_timeouts,5208},
             {crashes,0}]
