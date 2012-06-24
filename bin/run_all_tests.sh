if [ "x$1" == "x" ]; then 
    echo "Usage: $0 Config"
    exit
fi

erl -boot start_sasl -pa ebin deps/*/ebin -s wsdemo_bench -config "$1" -eval "wsdemo_bench:run_sync(), init:stop()."

