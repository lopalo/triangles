-module(tri_controller).
-export([handle/2, send/3, broadcast/3]).


handle(Data, PlayerPid) ->
    NewData = tri_utils:atom_keys(Data),
    {cmd, Cmd} = lists:keyfind(cmd, 1, NewData),
    {args, Args} = lists:keyfind(args, 1, NewData),
    CmdParts = re:split(Cmd, <<"\\.">>, [{return, binary}]),
    Fun = fun(Bin) -> binary_to_atom(Bin, utf8) end,
    ArgDct = dict:from_list(tri_utils:atom_keys(Args)),
    handle(lists:map(Fun, CmdParts), ArgDct, PlayerPid).

handle([world, Cmd], Args, _PlayerPid) ->
    tri_world:client_cmd(Cmd, Args);
handle([user, Cmd], Args, PlayerPid) ->
    tri_player:client_cmd(PlayerPid, Cmd, Args);
handle([scores, Cmd], Args, _PlayerPid) ->
    tri_scores:client_cmd(Cmd, Args);
handle([echo], Args, _PlayerPid) ->
    Text = dict:fetch(text, Args),
    ReplyText = io_lib:format("Echo: ~s", [Text]),
    Reply = unicode:characters_to_binary(ReplyText, utf8),
    ok = send(self(), echo_reply, [{text, Reply}]).


send(ConnPid, Cmd, Args) ->
    Data = [{cmd, Cmd}, {args, Args}],
    tri_connection:send(ConnPid, Data).

broadcast(ConnPids, Cmd, Args) ->
    Send = fun(ConnPid) -> send(ConnPid, Cmd, Args) end,
    lists:foreach(Send, ConnPids).



