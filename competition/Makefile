server: play pypy node build

build:
	$(MAKE) -C wsdemo-playframework-2
	erlc wsdemo.erl
	javac -cp webbit-0.4.7-full.jar EchoWsServer.java
	ghc -rtsopts --make -O2 -threaded wsdemo-snap.hs
	go build -o wsdemo-go-gonet wsdemo.go

clean:
	$(MAKE) -C wsdemo-playframework-2 clean
	rm *.class *snap.hi *snap.o wsdemo-snap *.beam wsdemo-go-gonet
	rm -rf pypy-1.9 node-v0.8.21

play:
	unzip -u play-2.0.1.zip
	$(MAKE) -C wsdemo-playframework-2

pypy:
	tar xjf pypy-1.9-linux64.tar.bz2	
	./pypy-1.9/bin/pypy distribute_setup.py
	./pypy-1.9/bin/easy_install pip
	./pypy-1.9/bin/pip install tornado ws4py twisted txws	

node:
	tar xzf node-v0.8.21.tar.gz
	cd node-v0.8.21 && ./configure && make && sudo make install
	npm install websocket ws
