HOSTNAME=$1
PORT=$2

do_test() {
   SERVERNAME=$1
   echo "Start $SERVERNAME then press enter"
   read

   echo "Warming up server"
   ./runtest data/warmup 60 $HOSTNAME $PORT 1000

   echo "Start ./bin/meminfo on the server then press enter"
   read
   echo "Running test for $SERVERNAME"
   ./runtest data/$SERVERNAME 600 $HOSTNAME $PORT 10000
}

do_test "erlang-cowboy"
do_test "go-websockets"
do_test "haskell-snap"
do_test "java-webbit"
do_test "node-websocket"
do_test "node-ws-cluster"
do_test "perl-ev"
do_test "python-tornado"
do_test "python-ws4py"
do_test "ruby-em-websockets"

