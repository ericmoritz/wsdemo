\c wsdemo

-- Copy all handshake data into the PostgreSQL database
--   Handshake times
\COPY handshakes FROM '../data/erlang-cowboy/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/go-websockets/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/haskell-snap/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/java-webbit/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/node-websocket/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/node-ws/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/perl-ev/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/python-tornado/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/python-ws4py/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/ruby-em-websockets/handshake_times.csv' WITH CSV HEADER

--   Latencies
\COPY latencies FROM '../data/erlang-cowboy/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/go-websockets/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/haskell-snap/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/java-webbit/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/node-websocket/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/node-ws/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/perl-ev/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/python-tornado/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/python-ws4py/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/ruby-em-websockets/message_latencies.csv' WITH CSV HEADER
