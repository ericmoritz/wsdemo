#!/usr/bin/env python
# vim: fileencoding=utf-8 et ts=4 sts=4 sw=4 tw=0 fdm=marker fmr=#{,#}

from gevent.pywsgi           import WSGIServer
from geventwebsocket.handler import WebSocketHandler

from gevent                  import fork
from prefork                 import prefork, cpu_count

def connection( env, start ):
    ws = env["wsgi.websocket"]
    send, rcv = ws.send, ws.receive
    while True: send( rcv() )

if __name__ == "__main__":
    addr   = "0.0.0.0", 8000
    server = WSGIServer(addr, connection, handler_class=WebSocketHandler, log=None)

    # bind socket
    if hasattr( server, 'pre_start' ):
        server.pre_start()
    else:
        server.init_socket()

    num = cpu_count()
    print "Gevent + gevent-websocket (%s workers)" % (num+1)
    prefork( num, fork=fork )  # gevent needs special fork

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        server.stop()
