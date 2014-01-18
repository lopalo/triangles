-module(tri_connection).
-behaviour(cowboy_websocket_handler).

-export([
    init/3,
    websocket_init/3,
    websocket_terminate/3,
    websocket_handle/3,
    websocket_info/3
]).
-export([send/2, create_player/2]).


% behaviour callbacks
init({tcp, http}, _Req,  []) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(tcp, Req, []) ->
    {ok, Req, none, hibernate}.

websocket_handle({text, Msg}, Req, PlayerPid) ->
    Data = tri_utils:unpack_json(jiffy:decode(Msg)),
    tri_controller:handle(Data, PlayerPid),
    {ok, Req, PlayerPid}.

websocket_info({send, Data}, Req, PlayerPid) ->
    Msg = jiffy:encode(tri_utils:pack_json(Data)),
    {reply, {text, Msg}, Req, PlayerPid, hibernate};
websocket_info({create_player, Data}, Req, none) ->
    {ok, PlayerPid} = tri_player:start_link(Data),
    {ok, Req, PlayerPid}.

websocket_terminate(_Reason, _Req, _PlayerPid) ->
    ok.


% external inteface
send(ConnPid, Data) ->
    ConnPid ! {send, Data},
    ok.

create_player(ConnPid, Data) ->
    ConnPid ! {create_player, Data},
    ok.

