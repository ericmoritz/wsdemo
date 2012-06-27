#!/usr/bin/env node

var http = require('http')
  , cluster = require('cluster')
  , numCPUs = require('os').cpus().length

if (cluster.isMaster) {
  // Master Process
  var versionInfo = process.version.substr(1).split('.').map(function(v) { return parseInt(v); });
  if (versionInfo[1] !== 8) {
    console.error('Please use node v0.8.0+\nAfter installing v0.8, please run "npm rebuild ws".');
    process.exit(-1);
  }
  for (var i = 0; i < numCPUs; i++) cluster.fork();
}
else {
  // Worker Process
  var ws = require('ws')
    , server = http.createServer()
    , wss = new ws.Server({server: server});
  server.listen(8000, 3000); // increased socket backlog
  wss.on('connection', function(con) {
    con.on('message', con.send);
  });
}
