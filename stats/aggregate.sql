\c wsdemo

-- INSERT INTO handshake_start_times (framework, begin)
--   SELECT framework, begin FROM handshake_start_time;

-- INSERT INTO latencies_start_times (framework, begin)
--   SELECT framework, begin FROM latencies_start_time;

INSERT INTO handshakes_freq (framework, elapsed_ms, n)
  SELECT framework, elapsed_ms, count(elapsed_ms)
  FROM handshakes_simple
  GROUP BY framework, elapsed_ms;

INSERT INTO latencies_freq (framework, elapsed_ms, n)
  SELECT framework, elapsed_ms, count(elapsed_ms)
  FROM latencies_simple
  GROUP BY framework, elapsed_ms;

