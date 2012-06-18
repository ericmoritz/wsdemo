.PHONY = all stats dirs

all: stats

stats: dirs counts.csv latencies.csv handshake.csv
	R CMD BATCH stats.r

dirs:
	mkdir -p stat_results

clean:
	rm -f counts.csv latencies.csv handshake.csv
	rm -f *.Rout
	rm -f stat_results/*.pdf

counts.csv:
	echo "framework, clients, handshakes, connection_timeouts, messages_sent, messages_recv, crashes" > counts.csv
	for k in ../data/*/counts.csv; do tail -n 1 "$$k" >> counts.csv ; done

latencies.csv:
	echo "framework, timestamp, elapsed" > latencies.csv
	for k in ../data/*/message_latencies.csv; do sed -e '1d' "$$k" >> latencies.csv; done

handshake.csv:
	echo "framework, timestamp, elapsed" > handshake.csv
	for k in ../data/*/handshake_times.csv; do sed -e '1d' "$$k" >> handshake.csv; done
