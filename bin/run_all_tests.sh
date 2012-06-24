erl -boot start_sasl -pa ebin deps/*/ebin -s wsdemo_bench $@ -eval "wsdemo_bench:run_sync(), init:stop()."


