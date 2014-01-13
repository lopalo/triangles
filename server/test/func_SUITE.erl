-module(func_SUITE).
-include("ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_echo/1,
         test_get_objects_info/1,
         test_start/1,
         test_tick/1]).


-define(ADDRESS, "ws://localhost:9000").


all() ->
    [test_echo, test_start, test_get_objects_info, test_tick].

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

% helpers

make_player(Name) when is_binary(Name)->
    U = tri_test_cli:connect(?ADDRESS),
    tri_test_cli:send(U, 'world.start', [{name, Name}]),
    {ok, [{uid, Uid}|_]} = tri_test_cli:recv(U, 'world.init', 1000),
    {Uid, U}.

% test cases
test_echo(_Config) ->
    U = tri_test_cli:connect(?ADDRESS),
    tri_test_cli:send(U, echo, [{text, "Лопата"}]),
    {ok, [{text, Text}]} = tri_test_cli:recv(U, echo_reply, 1000),
    Exp = unicode:characters_to_binary("Echo: Лопата", utf8),
    Exp = Text.


test_start(_Config) ->
    U = tri_test_cli:connect(?ADDRESS),
    tri_test_cli:send(U, 'world.start', [{name, <<"Семен">>}]),
    {ok, Args} = tri_test_cli:recv(U, 'world.init', 1000),
    [
        {uid, _},
        {server_tick, 200},
        {level_size, [1000, 700]}
    ] = Args.

test_get_objects_info(_Config) ->
    {Uid1, U1} = make_player(<<"player1">>),
    {Uid2, _U2} = make_player(<<"player2">>),
    Idents = [Uid1, Uid2, <<"some_id">>],
    tri_test_cli:send(U1, 'world.get_objects_info', [{idents, Idents}]),
    {ok, [{objects, Objs}]} = tri_test_cli:recv(U1, 'world.objects_info', 1000),
    [
        {Uid1,
            [{<<"type">>, <<"triangle">>},
             {<<"name">>, <<"player1">>},
             {<<"pos">>, [0, 0]},
             {<<"angle">>, 0}]
        },
        {Uid2,
             [{<<"type">>, <<"triangle">>},
              {<<"name">>, <<"player2">>},
              {<<"pos">>, [0, 0]},
              {<<"angle">>, 0}]
        }
    ] = Objs.

test_tick(_Config) ->
    {Uid1, _U1} = make_player(<<"player1">>),
    {Uid2, U2} = make_player(<<"player2">>),
    MoveVect = [3, 30],
    tri_test_cli:send(U2, 'user.commands', [{move_vector, MoveVect}]),
    tri_test_cli:recv(U2, 'world.tick', 1000),
    {ok, [{tick_data, Data}]} = tri_test_cli:recv(U2, 'world.tick', 1000),
    [
        {Uid1, [{<<"pos">>, [0, 0]},{<<"angle">>, 0}]},
        {Uid2, [{<<"pos">>, [104, 60]}, {<<"angle">>, 30}]}
    ] = Data.










