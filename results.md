I started two m1.medium AWS instances.  On one instance I started one of the 
servers and the other instance I ran the `./runtest` command as:

    sudo bash
    ulimit -n 999999
    ./runtest 300 $SERVER_HOSTNAME 8000 10000


After 5 minutes of running, the following stats are outputed.

## stat definitions

### connection_time

Elapsed time in microseconds between the opening of the client TCP socket
and the websocket handshake is finished.

### latency

Elapsed time in microseconds for a message sent from the client to echo
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

## Results

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
  <td>00.864ms</td>
  <td>00.017ms</td>
  <td>2849294</td>
  <td>0</td>
  </tr>
  <tr>
  <td>Go</td>
  <td>00.284ms</td>
  <td>18.502ms</td>
  <td>2398180</td>
  <td>225</td>
  </tr>
  <tr>
  <td>Node.js</td>
  <td>00.767ms</td>
  <td>42.580ms</td>
  <td>1170847</td>
  <td>4299</td>
  </tr>
  <tr>
  <td>Python (ws4py)</td>
  <td>01.561ms</td>
  <td>34.889ms</td>
  <td>1052996</td>
  <td>5208</td>
  </tr>
</table>

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
