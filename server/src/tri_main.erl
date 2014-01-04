-module(tri_main).
-export([start/0, dev_start/0, stop/0]).

-define(APPS, [crypto, ranch, cowlib, cowboy, tri]).

start() ->
    ok = start_apps(?APPS).


dev_start() ->
    ok = start(),
    ok = sync:go().


stop() ->
    sync:stop(),
    stop_apps(lists:reverse(?APPS)).


start_apps([]) -> ok;
start_apps([App|Apps]) ->
    case application:start(App) of 
        ok -> 
            start_apps(Apps);
        {error, {already_started, App}} -> 
            start_apps(Apps)
    end.


stop_apps([]) -> ok;
stop_apps([App|Apps]) ->
    application:stop(App),
    stop_apps(Apps).
