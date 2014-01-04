-module(tri_test_cli).
-behaviour(websocket_client_handler).

-export([init/2, websocket_handle/3, websocket_info/3, websocket_terminate/3]).
-export([connect/1, send/3, recv_any/2, recv/3, flush/1]).


init([Pid], _ConnState) ->
    {ok, Pid}.

websocket_handle({binary, Msg}, _ConnState, RecvPid) ->
    Data = tri_utils:unpack_json(jiffy:decode(Msg)),
    NewData = tri_utils:atom_keys(Data),
    {cmd, Cmd} = lists:keyfind(cmd, 1, NewData),
    {args, Args} = lists:keyfind(args, 1, NewData),
    RecvPid ! {server_msg, self(),
        {binary_to_atom(Cmd, utf8), tri_utils:atom_keys(Args)}},
    {ok, RecvPid}.

websocket_info({send, Cmd, Args}, _ConnState, RecvPid) ->
    Data = [{cmd, Cmd}, {args, Args}],
    Msg = jiffy:encode(tri_utils:pack_json(Data)),
    {reply, {binary, Msg}, RecvPid}.

websocket_terminate(_Reason, _ConnState, _State) ->
    ok.


connect(Address) ->
    {ok, Pid} = websocket_client:start_link(Address, ?MODULE, [self()]),
    Pid.

send(Pid, Cmd, Args) ->
    Pid ! {send, Cmd, Args},
    ok.


recv_any(FromPid, Timeout) ->
    receive
        {server_msg, FromPid, Data} ->
            {ok, Data}
    after
        Timeout ->
            timeout
    end.

recv(FromPid, Cmd, Timeout) ->
    receive
        {server_msg, FromPid, {Cmd, Args}} ->
            {ok, Args}
    after
        Timeout ->
            timeout
    end.

flush(FromPid) ->
    receive
        {server_msg, FromPid, _Data} ->
            flush(FromPid)
    after
        0 -> ok
    end.


