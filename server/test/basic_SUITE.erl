-module(basic_SUITE).
-include("ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_echo/1]).


all() -> 
    [test_echo].

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
    tri_test_cli:send(U, echo, [{text, "Foo"}]),
    {ok, {echo_reply, [{text, Text}]}} = tri_test_cli:recv(U, echo_reply, 1000),
    <<"Echo: Foo">> = Text.


