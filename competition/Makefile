build:
	erlc wsdemo.erl
	javac -cp webbit-0.4.7-full.jar EchoWsServer.java
	ghc -rtsopts --make -O2 -threaded wsdemo-snap.hs
	go build -o wsdemo-go-gonet wsdemo.go

clean:
	rm *.class
	rm *snap.hi
	rm *snap.o
	rm wsdemo-snap
	rm *.beam
	rm wsdemo-go-gonet
