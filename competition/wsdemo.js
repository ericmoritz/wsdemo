#!/usr/bin/env node
var WebSocketServer = require("websocket").server;
var http = require("http");

var server = http.createServer(function(request, response) {
  response.writeHead(404);
  response.end();
});

server.listen(8000, function() {
  console.log((new Date()) + ' Server is listening on port 8000');
});

wsServer = new WebSocketServer({
  httpServer: server,
  dropConnectionOnKeepaliveTimeout: false
});

wsServer.on("request", function(request) {
  var connection = request.accept(null, request.origin);

  connection.on("message", function(message) {
    if (message.type === 'utf8') {
      connection.sendUTF(message.utf8Data);
    }
    else if (message.type === 'binary') {
      connection.sendBytes(message.binaryData);
    }
  });

  connection.on('close', function(reasonCode, description) {
  });
});
