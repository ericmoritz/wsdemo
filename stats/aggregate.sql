\c wsdemo

INSERT INTO latencies_small
  SELECT * FROM latencies_skew
  WHERE random() < 0.01;
