from ws4py.server.geventserver import WebSocketServer
from ws4py.websocket import WebSocket
import gevent
import time


class EchoServer(WebSocket):
    def received_message(self, message):
        self.send(message.data, message.is_binary)


if __name__ == '__main__':
    server = WebSocketServer(('', 8000), 
                             websocket_class=EchoServer)
    print "listening on port 8000"
    server.serve_forever()
