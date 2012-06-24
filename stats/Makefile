.PHONY = all stats dirs

all: stats

inject-data:
	@echo "---- Creating Database"
	createdb wsdemo
	@echo "---- Installing the DB Schema"
	psql < database_setup.sql
	@echo "---- Injecting raw data (expect this to take a lot of time"
	psql < inject.sql
	@echo "---- Aggregating data"
	psql < aggregate.sql

drop-data:
	dropdb wsdemo

stats: dirs counts.csv
	R CMD BATCH stats.r

dirs:
	mkdir -p stat_results

clean:
	rm -f counts.csv
	rm -f *.Rout
	rm -f stat_results/*.pdf

counts.csv:
	echo "framework, clients, handshakes, connection_timeouts, messages_sent, messages_recv, crashes" > counts.csv
	for k in ../data/*/counts.csv; do tail -n 1 "$$k" >> counts.csv ; done
