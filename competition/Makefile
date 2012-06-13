build:
	javac -cp webbit-0.4.7-full.jar EchoWsServer.java
	ghc --make -O2 -threaded wsdemo-snap.hs

clean:
	rm *.class
	rm *snap.hi
	rm *snap.o
	rm wsdemo-snap
