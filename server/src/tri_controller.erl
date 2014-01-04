-module(tri_controller).
-export([handle/2, send/3]).


handle(Data, PlayerPid) ->
    NewData = tri_utils:atom_keys(Data),
    {cmd, Cmd} = lists:keyfind(cmd, 1, NewData),
    {args, Args} = lists:keyfind(args, 1, NewData),
    CmdParts = re:split(Cmd, <<"\\.">>, [{return, binary}]),
    Fun = fun(Bin) -> binary_to_atom(Bin, utf8) end,
    handle(lists:map(Fun, CmdParts), tri_utils:atom_keys(Args), PlayerPid).

handle([world, Cmd], Args, _PlayerdPid) ->
    tri_world:client_cmd(Cmd, Args);
handle([echo], Args, _PlayerPid) ->
    {text, Text} = lists:keyfind(text, 1, Args),
    ReplyText = io_lib:format("Echo: ~s", [Text]),
    Reply = unicode:characters_to_binary(ReplyText, utf8),
    ok = send(self(), echo_reply, [{text, Reply}]);
handle([user, _Cmd], _Args, _PlayerPid) ->
    %TODO: implement
    ok.


send(ConnPid, Cmd, Args) ->
    Data = [{cmd, Cmd}, {args, Args}],
    tri_connection:send(ConnPid, Data).


