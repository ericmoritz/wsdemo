.PHONY: 
	echo "make server | client"

server: server-deps config
	$(MAKE) -C competition server

config:
	sudo cp etc/sysctl.conf /etc/
	sudo sysctl -p

server-deps:
	sudo apt-get update
	sudo apt-get install -y \
	    curl emacs unzip python-dev python-setuptools build-essential erlang-nox erlang-dev \
	    libevent-dev git golang mercurial openjdk-7-jdk ruby rubygems haskell-platform  libadns1-dev
	echo -e "y\ny\no conf prerequisites_policy follow\no conf commit" | sudo cpan
	sudo cpan Protocol::WebSocket
	sudo cpan YAML
	sudo cpan EV
	sudo cpan EV::ADNS
	yes | sudo cpan IO::Stream
	sudo easy_install pip ws4py gevent gevent-websocket tornado twisted txws supervisor priv/wsdemo_monitor
	sudo go get code.google.com/p/go.net/websocket
	sudo gem install em-websocket
	sudo cabal update
	sudo cabal install snap-server snap-core websockets websockets-snap


client:
	./rebar get-deps compile

report:
	./bin/compile_all_stats.sh

stats:
	$(MAKE) -C stats stats
