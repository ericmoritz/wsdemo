build:
	$(MAKE) -C wsdemo-playframework-2
	erlc wsdemo.erl
	javac -cp webbit-0.4.7-full.jar EchoWsServer.java
	ghc -rtsopts --make -O2 -threaded wsdemo-snap.hs
	go build -o wsdemo-go-gonet wsdemo.go

clean:
	$(MAKE) -C wsdemo-playframework-2 clean
	rm *.class *snap.hi *snap.o wsdemo-snap *.beam wsdemo-go-gonet

