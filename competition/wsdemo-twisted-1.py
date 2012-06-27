from twisted.internet.reactor  import listenTCP, run
from twisted.internet.protocol import Protocol, Factory
from txws                      import WebSocketFactory

class Echo( Protocol ):
    def dataReceived( self, data ):
        self.transport.write( data )

class EchoFactory( Factory ):
    def buildProtocol( self, addr ):
        return Echo()

if __name__ == "__main__":
    listenTCP(8000, WebSocketFactory( EchoFactory() ), backlog=768)

    print "Twisted + txWS (1 worker)"

    run()
