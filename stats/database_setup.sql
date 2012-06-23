-- Database setupfor for a PostgreSQL database
\c wsdemo

-- Handshake data
CREATE TABLE handshakes (
    framework   VARCHAR(20),
    timestamp   BIGINT,
    elapsed     BIGINT
);

CREATE TABLE latencies (
    framework   VARCHAR(20),
    timestamp   BIGINT,
    elapsed     BIGINT
);

CREATE VIEW latencies_sample AS
  SELECT *
  FROM latencies
  WHERE random() < 0.1;

CREATE TABLE latencies_small (
    framework  VARCHAR(20),
    timestamp  BIGINT,
    elapsed    BIGINT
);

