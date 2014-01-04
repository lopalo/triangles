-module(basic_SUITE).
-include("ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_echo/1, test_start/1]).


all() -> 
    [test_echo, test_start].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    tri_main:start(),
    Config.

end_per_testcase(_, Config) ->
    tri_main:stop(),
    Config.

test_echo(_Config) ->
    U = tri_test_cli:connect("ws://localhost:9000"),
    tri_test_cli:send(U, echo, [{text, "Лопата"}]),
    {ok, [{text, Text}]} = tri_test_cli:recv(U, echo_reply, 1000),
    Exp = unicode:characters_to_binary("Echo: Лопата", utf8),
    Exp = Text.


test_start(_Config) ->
    U = tri_test_cli:connect("ws://localhost:9000"),
    tri_test_cli:send(U, 'world.start', [{name, "Семен"}]),
    {ok, Args} = tri_test_cli:recv(U, 'world.init', 1000),
    [
        {uid, _},
        {server_tick, 200},
        {level_size, [1000, 700]}
    ] = Args.
 



