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
-export([start_link/4,start_link/5,write/2,close/1]).

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
    [{ws_onopen,1}, {ws_onmessage,2}, {ws_info,2}, {ws_onclose,1}];
behaviour_info(_) ->
    undefined.

-record(state, {socket,readystate=undefined,headers=[],callback, callback_state,
               framebuffer}).

start_link(Host,Port,Mod,ModState) ->
    start_link(Host,Port,"/",Mod,ModState).
  
start_link(Host,Port,Path,Mod,ModState) ->
    gen_server:start_link(?MODULE, [{Host,Port,Path,Mod,ModState}], []).

init(Args) ->
    process_flag(trap_exit,true),
    [{Host,Port,Path,Mod,ModState}] = Args,
    {ok, Sock} = gen_tcp:connect(Host,Port,[binary,{packet, http},{active,true}]),
    Req = initial_request(Host,Path),
    ok = gen_tcp:send(Sock,Req),
    inet:setopts(Sock, [{packet, http}]),
    {ok,#state{socket=Sock,callback=Mod,callback_state=ModState}}.


%% Write to the server
write(Pid, Data) ->
    gen_server:cast(Pid,{send,Data}).

%% Close the socket
close(Pid) ->
    gen_server:cast(Pid,close).

handle_cast({send,Data}, State) ->
    gen_tcp:send(State#state.socket,[0] ++ Data ++ [255]),
    {noreply, State};
handle_cast(close,State) ->
    Mod = State#state.callback,
    CBState = Mod:ws_onclose(State#state.callback_state),
    gen_tcp:close(State#state.socket),
    State1 = State#state{readystate=?CLOSED, callback_state=CBState},
    {stop,normal,State1}.

%% Start handshake
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
                "websocket" ->
                    inet:setopts(Socket, [{packet, raw}]),
                    State1 = State#state{readystate=?OPEN,socket=Socket},
                    Mod = State#state.callback,
                    CBState = Mod:ws_onopen(State#state.callback_state),
                    {noreply,State1#state{callback_state=CBState}};
                _Any  ->
                    {stop,error,State}
            end;
        Other ->
            %% Bad state should have received response first
            {stop,{error, {http_eoh, unexpected_readystate, Other}},State}
    end;
%% Handshake complete, handle packets
handle_info({tcp, _Socket, Data}, #state{callback=Mod} = State) ->
    case State#state.readystate of
        ?OPEN ->
            {ok, Frame} = unframe(Data),
            CBState = Mod:ws_onmessage(Frame, State#state.callback_state),
            {noreply, State#state{callback_state=CBState}};
        Other ->
            {stop,{error, {tcp_data, unexpected_readystate, {Other, Data}}},State}
    end;
handle_info({tcp_closed, _Socket},State) ->
    Mod = State#state.callback,
    CBState = Mod:ws_onclose(State#state.callback_state),
    {stop,normal,State#state{callback_state=CBState}};
handle_info({tcp_error, _Socket, _Reason},State) ->
    {stop,tcp_error,State};
handle_info({'EXIT', _Pid, _Reason},State) ->
    {noreply,State};
handle_info(Msg, State) ->
    Mod = State#state.callback,
    CBState = Mod:ws_info(Msg, State#state.callback_state),
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
    "Origin: http://" ++ Host ++ "/\r\n\r\n" ++
    "draft-hixie: 68".

unframe(<<Fin:1, RSV:3, OpCode:4, Mask:1, Len:7, Payload/binary>>) ->
    Type = case OpCode of
               1  -> text;
               2  -> binary;
               9  -> ping;
               10 -> pong
           end,
    % This is incomplete, PayloadLen can be larger
    {ok, {Type, Payload}}.
