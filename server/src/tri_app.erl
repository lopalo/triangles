-module(tri_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(tri, port),
    Dispatch = cowboy_router:compile([{'_', [{"/", tri_connection, []}]}]),
    {ok, _Pid} = cowboy:start_http(
        tri_server, 10, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    tri_sup:start_link().

stop(_State) ->
    ok.
