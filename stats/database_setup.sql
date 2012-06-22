-- Database setupfor for a PostgreSQL database
\c wsdemo

-- Handshake data
CREATE TABLE handshakes (
    framework   VARCHAR(20),
    timestamp   BIGINT,
    elapsed     BIGINT
);

CREATE VIEW handshakes_simple AS
    SELECT framework, timestamp, elapsed / 1000 AS elapsed_ms
    FROM handshakes;

CREATE TABLE handshakes_freq (
    framework   VARCHAR(20),
    elapsed_ms  INTEGER,
    n           INTEGER
);

CREATE VIEW handshake_start_time AS
  SELECT framework, min(timestamp) AS begin
  FROM handshakes
  GROUP by framework;

-- Essentially a materialized view
CREATE TABLE handshake_start_times (
    framework   VARCHAR(20),
    begin       BIGINT
);

CREATE TABLE latencies (
    framework   VARCHAR(20),
    timestamp   BIGINT,
    elapsed     BIGINT
);

CREATE VIEW latencies_simple AS
    SELECT framework, timestamp, elapsed / 1000 AS elapsed_ms
    FROM latencies;

CREATE VIEW latencies_start_time AS
  SELECT framework, min(timestamp) AS begin
  FROM latencies
  GROUP by framework;

CREATE TABLE latencies_start_times (
    framework   VARCHAR(20),
    begin       BIGINT
);

CREATE TABLE latencies_freq (
    framework   VARCHAR(20),
    elapsed_ms  INTEGER,
    n           INTEGER
);

