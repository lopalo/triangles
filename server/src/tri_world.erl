-module(tri_world).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0, client_cmd/2, add_player/2]).

-include("settings.hrl").

% behaviour callbacks
init(_Args) ->
    Players = ets:new(players, [set, private, {keypos, 1}]),
    Bullets = ets:new(bullets, [set, private, {keypos, 1}]),
    {ok, {Players, Bullets}}.

handle_cast({client_cmd, {Cmd, Args, ConnPid}}, Tables) ->
    handle_client_cmd(Cmd, Args, ConnPid, Tables),
    {noreply, Tables};
handle_cast({add_player, {PlayerId, PlayerPid, ConnPid} = Info},
                                {Players, _Bullets} = Tables) ->
    true = ets:insert_new(Players, Info),
    erlang:monitor(process, PlayerPid),
    send_init(ConnPid, PlayerId),
    {noreply, Tables}.

handle_info({'DOWN', Ref, process, Pid, _Reason},
                    {Players, _Bullets} = Tables) ->
    erlang:demonitor(Ref, [flush]),
    ets:delete(Players, Pid),
    {noreply, Tables}.


terminate(_Reason, _Tables) ->
    ok.

% internal functions
handle_client_cmd(start, Args, ConnPid, _Tables) ->
    tri_connection:create_player(ConnPid, Args);
handle_client_cmd(get_objects_info, Args, ConnPid, {Players, _Bullets}) ->
    Idents = dict:fetch(idents, Args),
    GetInfo = fun(F, [Id|Ids]) ->
        case ets:member(Players, Id) of
            true ->
                Pid = ets:lookup_element(Players, Id, 2),
                [{Id, tri_player:get_client_info(Pid)}|F(F, Ids)];
            false ->
                F(F, Ids)
        end;
    (_F, []) -> []
    end,
    Objects = GetInfo(GetInfo, Idents),
    tri_controller:send(ConnPid, 'world.objects_info', [{objects, Objects}]).


send_init(ConnPid, PlayerId) ->
    StartData = [
        {uid, PlayerId},
        {server_tick, ?SERVER_TICK},
        {level_size, ?LEVEL_SIZE}
    ],
    tri_controller:send(ConnPid, 'world.init', StartData).


% external interface
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

client_cmd(Cmd, Args) ->
    gen_server:cast(?MODULE, {client_cmd, {Cmd, Args, self()}}).

add_player(PlayerId, ConnPid) ->
    gen_server:cast(?MODULE, {add_player, {PlayerId, self(), ConnPid}}).




