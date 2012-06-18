.PHONY: build report stats

build:
	./rebar get-deps compile

report:
	./bin/compile_all_stats.sh

stats:
	$(MAKE) -C stats stats
