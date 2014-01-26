-module(unit_SUITE).
-include("ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_player_tick/1]).



all() ->
    [test_player_tick].

init_per_suite(Config) ->
    application:set_env(tri, port, 9000),
    application:set_env(tri, server_tick, 200),
    application:set_env(tri, level_size, [1000, 700]),
    application:set_env(tri, max_speed, 37),
    application:set_env(tri, force_factor, 100),
    application:set_env(tri, player_spawn_step, 77),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.


% test cases
test_player_tick(_Config) ->
    DT = 0.2,
    Player = tri_player:make_player(<<"user:1">>, <<"user">>,
                                        48, [23, 39], [7, 4], [23, 16]),
    {reply, Reply, _} = tri_player:handle_call({tick, DT}, none, Player),
    {ok, [29, 43], 48} = Reply.


