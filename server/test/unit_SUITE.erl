-module(unit_SUITE).
-include("ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_player_tick/1]).

-include("player.hrl").


all() ->
    [test_player_tick].

init_per_suite(Config) ->
    application:set_env(tri, port, 9000),
    application:set_env(tri, server_tick, 200),
    application:set_env(tri, level_size, [1000, 700]),
    application:set_env(tri, max_speed, 37),
    application:set_env(tri, force_factor, 100),
    application:set_env(tri, player_spawn_step, 77),
    application:set_env(tri, reflection_factor, 1),
    application:set_env(tri, fire_rate, 1),
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
    Player = #player{uid = <<"user:1">>,
                     name = <<"user">>,
                     angle = 48,
                     pos = [23, 39],
                     speed = [7, 4],
                     force = [23, 16]},
    {reply, Reply, _} = tri_player:handle_call({tick, DT}, none, Player),
    {ok, [29, 43], 48, false} = Reply.


