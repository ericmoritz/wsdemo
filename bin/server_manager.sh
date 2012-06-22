if [ "x$1" == "x" ]; then
    echo "usage: $0 nodename"
    exit
fi
erl -pa ebin deps/*/ebin -s wsdemo_server_manager -name "$1"

