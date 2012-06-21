from tornado.ioloop     import IOLoop
from tornado.web        import Application
from tornado.websocket  import WebSocketHandler
from tornado.httpserver import HTTPServer
from tornado.netutil    import bind_sockets
from prefork            import prefork, cpu_count

class EchoApp( Application ):
    def __init__(self):
        Application.__init__( self, [("/", WSHandler)] )

class WSHandler( WebSocketHandler ):
    def allow_draft76( self ):
        return False

    def open( self ):
        pass

    def on_message( self, message ):
        self.write_message( message, binary=True )

if __name__ == "__main__":
    server  = HTTPServer( EchoApp() )
    sockets = bind_sockets( 8000 )

    num = cpu_count()
    print "Tornado (%s workers)" % (num+1)
    prefork( num )

    server.add_sockets( sockets )
    IOLoop.instance().start()
