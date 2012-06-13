require 'rubygems'
require 'eventmachine'
require 'em-websocket'

EventMachine.run {

  EventMachine::WebSocket.start(:host => "0.0.0.0", :port => 8000) do |ws|
    ws.onmessage { |msg|
      ws.send msg
    }
  end
}
