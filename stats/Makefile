.PHONY = all stats dirs

all: stats

stats: dirs counts.csv
	R CMD BATCH stats.r

dirs:
	mkdir -p stat_results

clean:
	rm -f counts.csv
	rm -f *.Rout
	rm -f stat_results/*.pdf

counts.csv:
	echo "type, clients, handshakes, connection_timeouts, messages_sent, messages_recv, crashes" > counts.csv
	for k in ../data/*/counts.csv; do tail -n 1 "$$k" >> counts.csv ; done

