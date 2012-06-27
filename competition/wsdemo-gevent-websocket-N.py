from gevent.pywsgi           import WSGIServer
from geventwebsocket.handler import WebSocketHandler

from gevent                  import fork
from prefork                 import prefork, cpu_count

def connection( env, start ):
    ws = env["wsgi.websocket"]
    send, rcv = ws.send, ws.receive
    try:
        while True: send( rcv() )
    except:
        pass

if __name__ == "__main__":
    addr   = "0.0.0.0", 8000
    server = WSGIServer(addr, connection, handler_class=WebSocketHandler, backlog=768, log=None)

    if hasattr( server, 'pre_start' ):
        server.pre_start()
    else:
        server.init_socket()

    num = cpu_count()
    print "Gevent + gevent-websocket (%s workers)" % num
    if prefork( num, fork=fork ): exit()  # gevent needs a special fork

    server.serve_forever()
