%%
%% This is a really basic client that works with Cowboy
%% Derived from https://github.com/davebryson/erlang_websocket
%%
%% 
%% Basic implementation of the WebSocket API:
%% http://dev.w3.org/html5/websockets/
%% However, it's not completely compliant with the WebSocket spec.
%% Specifically it doesn't handle the case where 'length' is included
%% in the TCP packet, SSL is not supported, and you don't pass a 'ws://type url to it.
%%
%% It also defines a behaviour to implement for client implementations.
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(websocket_client).

-behaviour(gen_server).

%% API
-export([start_link/3,start_link/4,write/2,write_sync/2,close/1, frame/1, websocket_mask/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Ready States
-define(CONNECTING,0).
-define(OPEN,1).
-define(CLOSED,2).

%% Behaviour definition
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{ws_init,0}, {ws_onopen,2}, {ws_onmessage,3}, {ws_info,3}, {ws_onclose,2}];
behaviour_info(_) ->
    undefined.

-record(state, {socket,readystate=undefined,headers=[],callback, callback_state,
                server,
                sofar = <<>>}).

start_link(Mod,Host,Port) ->
    start_link(Mod, Host,Port,"/").
  
start_link(Mod,Host,Port,Path) ->
    gen_server:start_link(?MODULE, [Mod,Host,Port,Path], []).

init([Mod,Host,Port,Path]) ->
    ModState = Mod:ws_init(),
    {ok, #state{server={Host,Port,Path},
                callback=Mod, callback_state=ModState}, 0}.

%% Write to the server
write(Pid, Data) ->
    gen_server:cast(Pid,{send,Data}).

write_sync(ClientState, Data) ->
    Frame = frame(Data),
    gen_tcp:send(ClientState#state.socket, Frame).

%% Close the socket
close(Pid) ->
    gen_server:cast(Pid,close).

handle_cast({send,Data}, State) ->
    write_sync(State, Data),
    {noreply, State};
handle_cast(close,State) ->
    Mod = State#state.callback,
    CBState = Mod:ws_onclose(State#state.callback_state),
    gen_tcp:close(State#state.socket),
    State1 = State#state{readystate=?CLOSED, callback_state=CBState},
    {stop,normal,State1}.

%% Start handshake
handle_info(timeout, #state{server={Host,Port,Path}} = State) ->
    
    case gen_tcp:connect(Host,Port,
                         [binary,{packet, http},{active,true}], 2000) of
        {ok, Sock} ->
            Req = initial_request(Host,Path),
            ok = gen_tcp:send(Sock,Req),
            inet:setopts(Sock, [{packet, http}]),
            {noreply, State#state{socket=Sock}};
        {error, timeout} ->
            {stop, connection_timeout, State}
    end;
handle_info({http,Socket,{http_response,{1,1},101,_Status}}, State) ->
    State1 = State#state{readystate=?CONNECTING,socket=Socket},
    {noreply, State1};
%% Extract the headers
handle_info({http,Socket,{http_header, _, Name, _, Value}},State) ->
    case State#state.readystate of
        ?CONNECTING ->
            H = [{Name,Value} | State#state.headers],
            State1 = State#state{headers=H,socket=Socket},
            {noreply, State1};
        Other ->
            %% Bad state should have received response first
            {stop,{error, {http_header, unexpected_readystate, Other}},State}
    end;
%% Once we have all the headers check for the 'Upgrade' flag 
handle_info({http,Socket,http_eoh},State) ->
    %% Validate headers, set state, change packet type back to raw
    case State#state.readystate of
        ?CONNECTING ->
            Headers = State#state.headers,
            case proplists:get_value('Upgrade',Headers) of
                undefined ->
                    {stop,{error, missing_upgrade_header},State};
                Value ->
                    case string:to_lower(Value) of
                        "websocket" ->
                            inet:setopts(Socket, [{packet, raw}]),
                            State1 = State#state{readystate=?OPEN,socket=Socket},
                            Mod = State#state.callback,
                            CBState = Mod:ws_onopen(State, State#state.callback_state),
                            {noreply,State1#state{callback_state=CBState}}
                    end
            end;
        Other ->
            %% Bad state should have received response first
            {stop,{error, {http_eoh, unexpected_readystate, Other}},State}
    end;
%% Handshake complete, handle packets
handle_info({tcp, _Socket, Data}, #state{callback=Mod, sofar=SoFar} = State) ->
    case State#state.readystate of
        ?OPEN ->
            State2 = case unframe(<<SoFar/bits, Data/bits>>) of
                         {ok, Frame, SoFar2} ->
                             CBState = Mod:ws_onmessage(State, Frame, State#state.callback_state),
                             State#state{callback_state=CBState, sofar=SoFar2};
                         {continue, SoFar2} ->
                             State#state{sofar=SoFar2}
                     end,
            {noreply, State2};
        Other ->
            {stop,{error, {tcp_data, unexpected_readystate, {Other, Data}}},State}
    end;
handle_info({tcp_closed, _Socket},State) ->
    Mod = State#state.callback,
    CBState = Mod:ws_onclose(State, State#state.callback_state),
    {stop,normal,State#state{callback_state=CBState}};
handle_info({tcp_error, _Socket, _Reason},State) ->
    {stop,tcp_error,State};
handle_info(Msg, State) ->
    Mod = State#state.callback,
    CBState = Mod:ws_info(State, Msg, State#state.callback_state),
    {noreply, State#state{callback_state=CBState}}.
  
handle_call(_Request,_From,State) ->
    {reply,ok,State}.

terminate(Reason, _State) ->
    error_logger:info_msg("Websocket Client Terminated ~p~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_request(Host,Path) ->
    "GET "++ Path ++" HTTP/1.1\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n" ++ 
    "Host: " ++ Host ++ "\r\n" ++
    "Sec-WebSocket-Key: x3JJHMbDL1EzLkh9GBhXDw==\r\n" ++
    "Sec-WebSocket-Protocol: chat\r\n" ++
    "Sec-WebSocket-Version: 13\r\n" ++
    "Origin: http://" ++ Host ++ "/\r\n\r\n".

%% not enough data for a complete frame
unframe(Data) when byte_size(Data) =:= 1 ->
    {continue, Data};
%% 7 bit payload frame
unframe(<<_Fin:1, _RSV:3, OpCode:4, _Mask:1, Len:7, Payload:Len/bytes, Rest/binary>>) when Len < 126 ->
    {ok, payload(OpCode, Payload), Rest};
%% 7+16 bits payload prefix exists
unframe(<<_Fin:1, _RSV:3, OpCode:4, _Mask:1, 126:7, Len:16, Payload:Len/bytes, Rest/binary>>) when Len > 125 ->
    {ok, payload(OpCode, Payload), Rest};
%% 7+16 bits payload incomplete, keep reading
unframe(<<_Fin:1, _RSV:3, _OpCode:4, _Mask:1, 126:7, _Rest/binary>> = Data) ->
    {continue, Data};
%% 7+64 bits payload prefix exists
unframe(<< _Fin:1, _Rsv:3, OpCode:4, _Mask:1, 127:7, 0:1, Len:63, Payload:Len/bytes, Rest/bits >>) when Len > 16#FFFF ->
    {ok, payload(OpCode, Payload), Rest};
%% 7+64 bits payload prefix incomplete, keep reading
unframe(<< _Fin:1, _Rsv:3, _Opcode:4, _Mask:1, 127:7, _Rest/bits>> = Data) ->
    {continue, Data};
%% invalid frame, give up.
unframe(_Data) ->
    {error, badframe}.

payload(OpCode, Payload) ->
    Type = case OpCode of
               1  -> text;
               2  -> binary;
               9  -> ping;
               8  -> close;
               10 -> pong
           end,
    {Type, Payload}.

frame({Type, Data}) ->
    Opcode = case Type of
                 text ->
                     1;
                 binary -> 2;
                 ping -> 9;
                 pong -> 10
             end,

    <<MaskKey:32>> = <<1:32>>,
    Len = hybi_payload_length(iolist_size(Data)),

    MaskedData = websocket_mask(Data, MaskKey),
    Payload = <<MaskKey:32,MaskedData/binary>>,
    << 1:1, 0:3, Opcode:4, 1:1, Len/bits, Payload/bits>>.

hybi_payload_length(N) ->
	case N of
		N when N =< 125 -> << N:7 >>;
		N when N =< 16#ffff -> << 126:7, N:16 >>;
		N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
	end.

websocket_mask(Payload, MaskKey) ->
	websocket_mask(Payload, MaskKey, <<>>).

websocket_mask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
    T = O bxor MaskKey,
    websocket_mask(Rest, MaskKey, << Acc/binary, T:32 >>);
websocket_mask(<< O:24 >>, MaskKey, Acc) ->
    << MaskKey2:24, _:8 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:24 >>;
websocket_mask(<< O:16 >>, MaskKey, Acc) ->
    << MaskKey2:16, _:16 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:16 >>;
websocket_mask(<< O:8 >>, MaskKey, Acc) ->
    << MaskKey2:8, _:24 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:8 >>;
websocket_mask(<<>>, _MaskKey, Acc) ->
    Acc.

