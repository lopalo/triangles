-module(tri_connection).
-behaviour(cowboy_websocket_handler).

-export([
    init/3,
    websocket_init/3,
    websocket_terminate/3,
    websocket_handle/3,
    websocket_info/3
]).
-export([send/2]).


init({tcp, http}, _Req,  []) ->
    {upgrade, protocol, cowboy_websocket}.

%TODO:store player's pid in state
websocket_init(tcp, Req, []) ->
    {ok, Req, undefined_state, hibernate}.

websocket_handle({binary, Msg}, Req, State) ->
    Data = tri_utils:unpack_json(jiffy:decode(Msg)),
    tri_controller:handle(Data),
    {ok, Req, State}.

websocket_info({send, Data}, Req, State) ->
    Msg = jiffy:encode(tri_utils:pack_json(Data)),
    {reply, {binary, Msg}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

send(ConnPid, Data) ->
    ConnPid ! {send, Data},
    ok.



