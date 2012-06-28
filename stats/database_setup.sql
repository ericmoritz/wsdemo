-- Database setupfor for a PostgreSQL database
\c wsdemo

-- Handshake data
CREATE TABLE handshakes (
    framework   VARCHAR(30),
    timestamp   BIGINT,
    elapsed     BIGINT
);

CREATE VIEW handshakes_min AS
  SELECT framework, min(timestamp) AS min_timestamp
  FROM handshakes
  GROUP BY framework;

CREATE OR REPLACE VIEW handshakes_skew AS
  SELECT l.framework as framework
       , l.timestamp - lm.min_timestamp as timestamp
       , l.elapsed as elapsed
  FROM handshakes l INNER JOIN handshakes_min lm ON (l.framework = lm.framework);

CREATE TABLE latencies (
    framework   VARCHAR(30),
    timestamp   BIGINT,
    elapsed     BIGINT
);

CREATE VIEW latencies_min AS
  SELECT framework, min(timestamp) as min_timestamp
  FROM latencies
  GROUP BY framework;

CREATE OR REPLACE VIEW latencies_skew AS
  SELECT l.framework as framework
       , l.timestamp - lm.min_timestamp as timestamp
       , l.elapsed as elapsed
  FROM latencies l INNER JOIN latencies_min lm ON (l.framework = lm.framework);

CREATE VIEW latencies_sample AS
  SELECT *
  FROM latencies
  WHERE random() < 0.01;

CREATE TABLE latencies_small (
    framework  VARCHAR(30),
    timestamp  BIGINT,
    elapsed    BIGINT
);

CREATE TABLE events (
    timestamp  BIGINT,
    framework  VARCHAR(30),
    client_id  VARCHAR(20),
    event_key  VARCHAR(20),
    event_data TEXT
);

