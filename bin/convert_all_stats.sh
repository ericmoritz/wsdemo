
for db_path in data/*; do
    server_type=`basename $db_path`
    echo "Converting $server_type to CSV"
    ./bin/events_to_csv $server_type $db_path | tee results/$server_type.events.csv
done
