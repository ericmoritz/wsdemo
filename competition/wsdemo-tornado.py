import tornado.ioloop
import tornado.options
import tornado.web
import tornado.websocket
import tornado.httpserver
import tornado.netutil
import tornado.process

class Application(tornado.web.Application):
    def __init__(self):
        handlers = [
            (r"/", MyWebSocketHandler),
        ]
        tornado.web.Application.__init__(self, handlers)


class MyWebSocketHandler(tornado.websocket.WebSocketHandler):

    def allow_draft76(self):
        # for iOS 5.0 Safari
        return False

    def open(self):
	pass

    def on_message(self, message):
	self.write_message(message, binary=True)


def main():

        server = tornado.httpserver.HTTPServer(Application())
        sockets = tornado.netutil.bind_sockets(8000)
        job = tornado.process.fork_processes(None)
        server.add_sockets(sockets)
        tornado.ioloop.IOLoop.instance().start()



if __name__ == "__main__":
    main()

