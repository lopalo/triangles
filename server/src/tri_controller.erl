-module(tri_controller).
-export([handle/1, send/3]).


handle(Data) ->
    NewData = tri_utils:atom_keys(Data),
    {cmd, Cmd} = lists:keyfind(cmd, 1, NewData),
    {args, Args} = lists:keyfind(args, 1, NewData),
    CmdParts = re:split(Cmd, <<"\\.">>, [{return, binary}]),
    Fun = fun(Bin) -> binary_to_atom(Bin, utf8) end,
    handle(lists:map(Fun, CmdParts), tri_utils:atom_keys(Args)).

handle([world|Cmd], Args)->
    gen_server:cast(tri_world, {client_cmd, self(), Cmd, Args});
handle([echo], Args) ->
    {text, Text} = lists:keyfind(text, 1, Args),
    Reply = io_lib:format("Echo: ~s", [Text]),
    ok = send(self(), echo_reply, [{text, list_to_binary(Reply)}]).


send(ConnPid, Cmd, Args) ->
    Data = [{cmd, Cmd}, {args, Args}],
    tri_connection:send(ConnPid, Data).


