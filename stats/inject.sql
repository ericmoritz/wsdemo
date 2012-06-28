\c wsdemo

-- Copy all data into pg

--   Events
\COPY events FROM '../data/vagrant-c1k/clojure-aleph/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/erlang-cowboy/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/go-gonet/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/haskell-snap/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/java-webbit/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/node-ws/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/node-ws-cluster/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/perl-ev/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/pypy-tornado-1/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/pypy-tornado-N/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/pypy-twisted-1/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/pypy-twisted-N/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/python-tornado-1/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/python-tornado-N/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/python-twisted-1/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/python-twisted-N/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/python-gevent-websocket-1/events.csv' WITH CSV HEADER
\COPY events FROM '../data/vagrant-c1k/python-gevent-websocket-N/events.csv' WITH CSV HEADER

--   Latencies
\COPY latencies FROM '../data/vagrant-c1k/clojure-aleph/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/erlang-cowboy/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/go-gonet/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/haskell-snap/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/java-webbit/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/node-ws/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/node-ws-cluster/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/perl-ev/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/pypy-tornado-1/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/pypy-tornado-N/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/pypy-twisted-1/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/pypy-twisted-N/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/python-tornado-1/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/python-tornado-N/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/python-twisted-1/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/python-twisted-N/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/python-gevent-websocket-1/message_latencies.csv' WITH CSV HEADER
\COPY latencies FROM '../data/vagrant-c1k/python-gevent-websocket-N/message_latencies.csv' WITH CSV HEADER

--   Handshakes
\COPY handshakes FROM '../data/vagrant-c1k/clojure-aleph/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/erlang-cowboy/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/go-gonet/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/haskell-snap/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/java-webbit/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/node-ws/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/node-ws-cluster/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/perl-ev/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/pypy-tornado-1/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/pypy-tornado-N/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/pypy-twisted-1/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/pypy-twisted-N/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/python-tornado-1/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/python-tornado-N/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/python-twisted-1/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/python-twisted-N/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/python-gevent-websocket-1/handshake_times.csv' WITH CSV HEADER
\COPY handshakes FROM '../data/vagrant-c1k/python-gevent-websocket-N/handshake_times.csv' WITH CSV HEADER


-- Hand the tables a couple of indexes
--  This speeds up queries considerably since there is stuff to hook onto
--  for the query engine.
CREATE INDEX handshakes_ts_idx ON handshakes(timestamp);
CREATE INDEX handshakes_elapsed_idx ON handshakes(elapsed);

CREATE INDEX latencies_ts_idx ON latencies(timestamp);
CREATE INDEX latencies_elapsed_idx ON latencies(elapsed);
CREATE INDEX latencies_fwrk ON latencies(framework);

CREATE INDEX events_ts_idx ON events(timestamp);
