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

Each client sends a message with a timestamp every second and waits for the server to echo that message.  When the
message is received, the client records the elapsed microseconds. This elapsed time is collected in the latency stat.

### connections

Number of connections

### disconnections

Number of server disconnects

### messages

Total number of messages received by the clients

### ticks

Number of ticks received by the clients.  ticks are sent every second by the server for each connection.

## crashes

Number of crashed clients

## wsdemo.erl

    Result: [{connection_time,
             [{min,1509},
              {max,27677940},
              {arithmetic_mean,3687536.9134241245},
              {geometric_mean,235730.97870297634},
              {harmonic_mean,13262.630789054754},
              {median,371297},
              {variance,38761208260496.52},
              {standard_deviation,6225850.003051513},
              {skewness,2.0761222266293933},
              {kurtosis,3.6181694800355704},
              {percentile,
                  [{75,4079936},{95,18575148},{99,26217543},{999,27491497}]},
              {histogram,
                  [{2201509,642},
                   {5001509,159},
                   {7001509,48},
                   {9001509,29},
                   {11001509,17},
                   {13001509,19},
                   {16001509,40},
                   {18001509,18},
                   {20001509,17},
                   {22001509,4},
                   {24001509,10},
                   {26001509,14},
                   {29001509,11},
                   {31001509,0}]}]},
         {latency,
             [{min,396},
              {max,2809740},
              {arithmetic_mean,683493.5301556421},
              {geometric_mean,77449.85569490225},
              {harmonic_mean,4680.871643237114},
              {median,220661},
              {variance,682805426909.6475},
              {standard_deviation,826320.4141915214},
              {skewness,0.8942087916023738},
              {kurtosis,-0.5703740560613628},
              {percentile,
                  [{75,1359082},{95,2393150},{99,2618056},{999,2764555}]},
              {histogram,
                  [{290396,535},
                   {600396,67},
                   {900396,65},
                   {1200396,74},
                   {1500396,66},
                   {1800396,62},
                   {2100396,77},
                   {2300396,24},
                   {2600396,46},
                   {2900396,12},
                   {3200396,0}]}]},
         {connections,10000},
         {disconnections,0},
         {messages,928795},
         {ticks,918795},
         {crashes,0}]

## wsdemo.js

    Result: [{connection_time,
             [{min,1602},
              {max,7344983},
              {arithmetic_mean,918440.3210116732},
              {geometric_mean,123712.10408908203},
              {harmonic_mean,11043.19798735485},
              {median,225955},
              {variance,1801351944117.3472},
              {standard_deviation,1342144.5317540683},
              {skewness,1.75368668051449},
              {kurtosis,2.76568006209472},
              {percentile,
                  [{75,1370933},{95,3794557},{99,5351069},{999,7262180}]},
              {histogram,
                  [{501602,611},
                   {1001602,114},
                   {1401602,49},
                   {1901602,55},
                   {2401602,30},
                   {2801602,38},
                   {4001602,91},
                   {5001602,22},
                   {6001602,11},
                   {7001602,4},
                   {8001602,3}]}]},
         {latency,
             [{min,527},
              {max,116187692},
              {arithmetic_mean,46139930.151750974},
              {geometric_mean,21311125.511323277},
              {harmonic_mean,96601.45452787343},
              {median,42303452},
              {variance,1244711499402870.3},
              {standard_deviation,35280469.09272707},
              {skewness,0.2877854387607965},
              {kurtosis,-1.1896246923042872},
              {percentile,
                  [{75,74572187},
                   {95,106390955},
                   {99,112828780},
                   {999,115476601}]},
              {histogram,
                  [{13000527,272},
                   {25000527,92},
                   {40000527,132},
                   {50000527,83},
                   {70000527,146},
                   {80000527,85},
                   {90000527,63},
                   {100000527,61},
                   {120000527,94},
                   {130000527,0}]}]},
         {connections,5035},
         {disconnections,0},
         {messages,1162270},
         {ticks,429049},
         {crashes,0}]

## wsdemo.py

    Result: [{connection_time,
             [{min,1625},
              {max,8949829},
              {arithmetic_mean,1955258.1605058366},
              {geometric_mean,198086.59606449556},
              {harmonic_mean,11496.576074940796},
              {median,447483},
              {variance,5702507225222.317},
              {standard_deviation,2387992.300076011},
              {skewness,0.8949096532774136},
              {kurtosis,-0.5539879497701503},
              {percentile,
                  [{75,3776923},{95,6681685},{99,7749191},{999,8853781}]},
              {histogram,
                  [{901625,562},
                   {1701625,41},
                   {2501625,53},
                   {4001625,131},
                   {5001625,73},
                   {6001625,90},
                   {7001625,43},
                   {8001625,29},
                   {9001625,6},
                   {10001625,0}]}]},
         {latency,
             [{min,1074},
              {max,153197060},
              {arithmetic_mean,55918287.59143969},
              {geometric_mean,23474733.739317104},
              {harmonic_mean,231085.73125452633},
              {median,48342671},
              {variance,2122481285136451.8},
              {standard_deviation,46070394.88800212},
              {skewness,0.39782490291508016},
              {kurtosis,-1.1613402018348096},
              {percentile,
                  [{75,96350013},
                   {95,135040839},
                   {99,149188847},
                   {999,153072918}]},
              {histogram,
                  [{16001074,304},
                   {40001074,161},
                   {50001074,61},
                   {70001074,113},
                   {80001074,53},
                   {100001074,106},
                   {120001074,111},
                   {130001074,42},
                   {150001074,70},
                   {160001074,7},
                   {180001074,0}]}]},
         {connections,5459},
         {disconnections,0},
         {messages,857810},
         {ticks,238121},
         {crashes,0}]
