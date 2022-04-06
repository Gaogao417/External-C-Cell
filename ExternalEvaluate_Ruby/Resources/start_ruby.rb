#!/usr/bin/env ruby

# guide: http://zguide.zeromq.org/rb:hwserver

require 'rubygems'
require 'ffi-rzmq'
require 'json'

context = ZMQ::Context.new(1)
socket = context.socket(ZMQ::PAIR)
sock = socket.bind("tcp://127.0.0.1:*")
array = []
socket.getsockopt(ZMQ::LAST_ENDPOINT,array)
puts array[0].strip
STDOUT.flush

def get_binding()
  binding
end

$globals = get_binding()

def handle_message(request)
  begin
    msg = JSON.parse(request)

    if msg["constants"]
      msg["constants"].each do |key, value|
        $globals.local_variable_set(key, value)
      end
    end

    result = $globals.eval(msg["input"])


    if msg.key?("args")
      case result
        when Symbol
          result = send(result, *msg["args"])
        else
          result = result.call(*msg["args"])
       end
    end

    if msg["return_type"] == "string"
      result = result.to_s
    end

    if result.class == Symbol
        return {:Function => {:System => "Ruby", :Command => ":" + (result.to_s)}
      }.to_json
    end

    result = {:output => result}

  rescue Exception => exception
    result = {:message => exception.to_s, :error => exception.backtrace}
  end

  return result.to_json
end




while true do
    request = ''
    socket.recv_string(request)
    socket.send_string(handle_message(request))
end