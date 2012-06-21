from twisted.internet.reactor  import listenTCP, run
from twisted.internet.protocol import Protocol, Factory
from txws                      import WebSocketFactory

class Echo( Protocol ):
    def dataReceived( self, data ):
        self.transport.write( data )

class EchoFactory( Factory ):
    def buildProtocol( self, addr ):
        return Echo()

listenTCP(8000, WebSocketFactory( EchoFactory() ))

print "Twisted + txWS (1 worker)"

run()
