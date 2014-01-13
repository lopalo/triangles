-module(unit_SUITE).
-include("ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_player_tick/1]).



all() ->
    [test_player_tick].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.


% test cases
test_player_tick(_Config) ->
    DT = 200,
    Player = tri_player:make_player(<<"user:1">>, <<"user">>,
                                        48, [23, 39], [7, 4]),
    {reply, Reply, NewPlayer} = tri_player:handle_call({tick, DT},
                                                       none, Player),
    ExpPlayer = tri_player:make_player(<<"user:1">>, <<"user">>,
                                    48, [163, 119], [7, 4]),
    ExpPlayer = NewPlayer,
    {ok, [163, 119], 48} = Reply.


