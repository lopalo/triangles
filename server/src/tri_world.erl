-module(tri_world).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0, client_cmd/2, add_player/2]).

-include("settings.hrl").

-record(state, {players, bullets, last_update, tick_number=0}).

% behaviour callbacks
init(_Args) ->
    Players = ets:new(players, [set, private, {keypos, 1}]),
    Bullets = ets:new(bullets, [set, private, {keypos, 1}]),
    {ok, ServerTick} = application:get_env(tri, server_tick),
    timer:send_after(ServerTick, tick),
    {ok, #state{players=Players, bullets=Bullets, last_update=ms()}}.

handle_cast({client_cmd, {Cmd, Args, ConnPid}}, State) ->
    NewState = handle_client_cmd(Cmd, Args, ConnPid, State),
    {noreply, NewState};
handle_cast({add_player, {PlayerId, PlayerPid, ConnPid} = Info}, State) ->
    true = ets:insert_new(State#state.players, Info),
    erlang:monitor(process, PlayerPid),
    send_init(ConnPid, PlayerId, State),
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    erlang:demonitor(Ref, [flush]),
    ets:match_delete(State#state.players, {'_', Pid, '_'}),
    {noreply, State};
handle_info(tick, State) ->
    {ok, ServerTick} = application:get_env(tri, server_tick),
    timer:send_after(ServerTick, tick),
    {TickData, NewState} = handle_tick(State),
    ConnPids = [C || [C] <- ets:match(State#state.players, {'_', '_', '$3'})],
    tri_controller:broadcast(ConnPids, 'world.tick', [{tick_data, TickData}]),
    {noreply, NewState}.


terminate(_Reason, _State) ->
    ok.

% internal functions
handle_client_cmd(start, Args, ConnPid, State) ->
    {ok, [W, H]} = application:get_env(tri, level_size),
    {ok, SpawnStep} = application:get_env(tri, player_spawn_step),
    Step = State#state.tick_number * SpawnStep,
    Pos = [Step rem W, Step rem H],
    Args1 = dict:store(pos, Pos, Args),
    Args2 = dict:store(angle, Step rem 360, Args1),
    tri_connection:create_player(ConnPid, Args2),
    State;
handle_client_cmd(get_objects_info, Args, ConnPid, State) ->
    Players = State#state.players,
    Idents = dict:fetch(idents, Args),
    GetInfo = fun(Id) ->
        case ets:member(Players, Id) of
            true ->
                Pid = ets:lookup_element(Players, Id, 2),
                {Id, tri_player:get_client_info(Pid)};
            false ->
                none
        end
    end,
    Filter = fun tri_utils:filter_none/1,
    Objects = lists:filter(Filter, safe_map(GetInfo, Idents)),
    tri_controller:send(ConnPid, 'world.objects_info', [{objects, Objects}]),
    State.


send_init(ConnPid, PlayerId, State) ->
    {ok, ServerTick} = application:get_env(tri, server_tick),
    {ok, LevelSize} = application:get_env(tri, level_size),
    Players = State#state.players,
    GetInfo = fun([Id, Pid]) ->
        {Id, tri_player:get_client_info(Pid)}
    end,
    Objects = safe_map(GetInfo, ets:match(Players, {'$1', '$2', '_'})),
    StartData = [
        {uid, PlayerId},
        {server_tick, ServerTick},
        {level_size, LevelSize},
        {objects, Objects}
    ],
    tri_controller:send(ConnPid, 'world.init', StartData).


safe_map(_F, []) -> [];
safe_map(F, [I|Is]) ->
    try F(I) of
        Value -> [Value|safe_map(F, Is)]
    catch
        exit:{noproc, _} ->
            Is
    end.

ms() ->
    N = now(),
    element(2, N) * 1000 + trunc(element(3, N) / 1000).

handle_tick(State) ->
    Now = ms(),
    LastUpdate = State#state.last_update,
    NewState = State#state{last_update=Now,
                           tick_number=State#state.tick_number + 1},
    DT = max(Now - LastUpdate, 0),
    {ok, LevelSize} = application:get_env(tri, level_size),
    PlayersTick = fun([PlayerId, PlayerPid]) ->
        {ok, Pos1, Angle} = tri_player:tick(PlayerPid, DT),
        {Pos2, TouchData} = check_borders(Pos1, LevelSize),
        case TouchData of
            none -> ok;
            TouchData ->
                tri_player:touch_border(PlayerPid, Pos2, TouchData)
        end,
        PlayerData = [{pos, Pos2}, {angle, Angle}],
        {PlayerId, PlayerData}
    end,
    Players = ets:match(State#state.players, {'$1', '$2', '_'}),
    TickData = safe_map(PlayersTick, Players),
    {TickData, NewState}.

check_borders([X1, Y1], [W, H]) ->
    Pos2 = [X2, Y2] = [erlang:max(erlang:min(X1, W), 0),
                       erlang:max(erlang:min(Y1, H), 0)],
    TouchData = case {X2 - X1, Y2 - Y1} of
        {0, 0} -> none;
        {DX, 0} -> {DX / abs(DX), 0};
        {0, DY} -> {0, DY / abs(DY)};
        {DX, DY} -> {DX / abs(DX), DY / abs(DY)}
    end,
    {Pos2, TouchData}.



% external interface
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

client_cmd(Cmd, Args) ->
    gen_server:cast(?MODULE, {client_cmd, {Cmd, Args, self()}}).

add_player(PlayerId, ConnPid) ->
    gen_server:cast(?MODULE, {add_player, {PlayerId, self(), ConnPid}}).




