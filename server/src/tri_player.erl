-module(tri_player).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/1]).


init([ConnPid, PlayerData]) ->
    tri_world:add_player(ConnPid),
    {ok, undefined_state}.

handle_call(_Arg, _From, _State) ->
    ok.

handle_cast(_Arg, _State) ->
    ok.

terminate(_Reason, _State) ->
    ok.


start_link(Data) ->
    gen_server:start_link(?MODULE, [self(), Data], []).
