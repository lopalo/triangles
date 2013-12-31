-module(tri_world).

-behaviour(gen_server).

-export([start_link/0, init/1]).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, undefinded_state}.
