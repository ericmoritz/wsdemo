.PHONY: build report stats

build:
	./rebar get-deps compile
	$(MAKE) -C competition build

report:
	./bin/compile_all_stats.sh

stats:
	$(MAKE) -C stats stats
