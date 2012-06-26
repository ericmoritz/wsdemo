
for db_path in "$1"/*; do
    server_type=`basename $db_path`
    echo "Converting $server_type to CSV"
    ./bin/events_to_csv $server_type $db_path > "$db_path"/events.csv
done
