if [ "x$1" == "x" -o "x$2" == "x" ]; then 
    echo "Usage: $0 Name Config"
    exit
fi

erl -boot start_sasl -pa ebin deps/*/ebin -s wsdemo_bench -name "$1" -config "$2" -eval "wsdemo_bench:run_sync(), init:stop()."

