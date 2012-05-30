from ws4py.server.geventserver import WebSocketServer
from ws4py.websocket import WebSocket
import gevent
import time


def ticker(websocket):
    while True:
        gevent.sleep(1)
        try:
            websocket.send("tick", True)
        except Exception, e:
            print unicode(e)
            return


class TickingEchoServer(WebSocket):
    def opened(self):
        self.ticker = gevent.spawn(ticker, self)
        
    def closed(self, code, reason=None):
        gevent.kill(self.ticker)

    def received_message(self, message):
        self.send(message.data, message.is_binary)


if __name__ == '__main__':
    server = WebSocketServer(('', 8000), 
                             websocket_class=TickingEchoServer)
    print "listening on port 8000"
    server.serve_forever()
