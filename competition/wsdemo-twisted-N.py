from twisted.internet.reactor  import listenTCP, run
from twisted.internet.protocol import Protocol, Factory
from txws                      import WebSocketFactory
from prefork                   import prefork, cpu_count

class Echo( Protocol ):
    def dataReceived( self, data ):
        self.transport.write( data )

class EchoFactory( Factory ):
    def buildProtocol( self, addr ):
        return Echo()

if __name__ == "__main__":
    listenTCP(8000, WebSocketFactory( EchoFactory() ))

    num = cpu_count()
    print "Twisted + txWS (%s workers)" % (num+1)
    prefork( num )

    run()
