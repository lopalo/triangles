-module(tri_world).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0, client_cmd/2, add_player/2]).


-record(state, {players, bullets, last_update, tick_number=0}).
-record(bullet, {id, shooter_id, pos, speed}).

% behaviour callbacks
init([]) ->
    Players = ets:new(players, [set, private, {keypos, 1}]),
    Bullets = [],
    {ok, ServerTick} = application:get_env(tri, server_tick),
    timer:send_after(ServerTick, tick),
    {ok, #state{players=Players, bullets=Bullets, last_update=tri_utils:ms()}}.

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
    {Objects, Bullets, NewState} = handle_tick(State),
    ConnPids = [C || [C] <- ets:match(State#state.players, {'_', '_', '$3'})],
    ToSend = [{objects, Objects}, {bullets, Bullets}],
    tri_controller:broadcast(ConnPids, 'world.tick', ToSend),
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
    Objects = [O || O <- safe_map(GetInfo, Idents), O /= none],
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
            safe_map(F, Is)
    end.


handle_tick(State) ->
    Now = tri_utils:ms(),
    LastUpdate = State#state.last_update,
    DT = max(Now - LastUpdate, 0) / 1000,
    ObjectsToSend = [],
    {ok, LevelSize} = application:get_env(tri, level_size),
    {ok, ReflFactor} = application:get_env(tri, reflection_factor),
    TickPlayer = fun(Player) ->
        tick_player(DT, LevelSize, ReflFactor, Player)
    end,
    Players = ets:match(State#state.players, {'$1', '$2', '_'}),
    TickPlayersData = safe_map(TickPlayer, Players),
    PlayersToSend = [{PlayerId, PlayerData} ||
                     {PlayerId, PlayerData, _} <- TickPlayersData],
    NewBullets = [{PlayerId, BulletData} ||
                  {PlayerId, _, BulletData} <- TickPlayersData,
                  BulletData /= none],
    Bullets1 = tick_bullets(DT, LevelSize, State#state.bullets),
    Bullets2 = add_bullets(Bullets1, NewBullets),
    {Bullets3, UpdateScores} = handle_collisions(TickPlayersData, Bullets2),
    case UpdateScores of
        true -> tri_scores:send_update();
        false -> ok
    end,
    BulletsToSend = [{BulletId, Pos} ||
                     #bullet{id=BulletId, pos=Pos} <- Bullets2],
    NewState = State#state{last_update=Now,
                           tick_number=State#state.tick_number + 1,
                           bullets=Bullets3},
    {ObjectsToSend ++ PlayersToSend, BulletsToSend, NewState}.


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


tick_player(DT, LevelSize, ReflFactor, [PlayerId, PlayerPid]) ->
    {ok, Pos1, Angle, BulletData} = tri_player:tick(PlayerPid, DT),
    {Pos2, TouchData} = check_borders(Pos1, LevelSize),
    case TouchData of
        none -> ok;
        TouchData ->
            tri_player:touch_border(PlayerPid, Pos2, TouchData, ReflFactor)
    end,
    PlayerData = [{pos, Pos2}, {angle, Angle}],
    {PlayerId, PlayerData, BulletData}.


add_bullets(Bullets, New) ->
    Bullets ++ [#bullet{id=bullet_id(),
                        shooter_id=PlayerId,
                        pos=Pos,
                        speed=Speed} ||
                {PlayerId, {Pos, Speed}} <- New].


tick_bullets(DT, [W, H], Bullets1) ->
    Move = fun(#bullet{pos=[X, Y], speed=[SX, SY]} = Bullet) ->
        Bullet#bullet{pos=[trunc(X + SX * DT), trunc(Y + SY * DT)]}
    end,
    Bullets2 = lists:map(Move, Bullets1),
    CheckBorders = fun
        (#bullet{pos=[X, Y]}) when X > 0, X < W, Y > 0, Y < H ->
            true;
        (_) ->
            false
    end,
    Bullets3 = lists:filter(CheckBorders, Bullets2),
    Bullets3.


bullet_id() ->
    N = now(),
    A = integer_to_binary(element(2, N)),
    B = integer_to_binary(element(3, N)),
    <<A/binary, B/binary>>.


handle_collisions(Players, Bullets) ->
    %TODO: use more effective algorithm
    {ok, HitCircle} = application:get_env(tri, hit_circle),
    CheckHit = fun([TX, TY], [BX, BY]) ->
        tri_utils:vect_length(TX - BX, TY - BY) < HitCircle
    end,
    Coll = fun
        (CF, [{TargetId, [{pos, PlPos}, _], _}|RestPlayers], Bullets1) ->
            Detect = fun
                (DF, [Bullet|RestBullets]) ->
                    ShooterId = Bullet#bullet.shooter_id,
                    case TargetId /= ShooterId andalso
                            CheckHit(PlPos, Bullet#bullet.pos) of
                        true ->
                            {{TargetId, ShooterId}, RestBullets};
                        false ->
                            {Hit, NewBullets} = DF(DF, RestBullets),
                            {Hit, [Bullet|NewBullets]}
                    end;
                (_DF, []) ->
                    {none, []}
            end,
            {Hit, Bullets2} = Detect(Detect, Bullets1),
            {NewHits, Bullets3} = CF(CF, RestPlayers, Bullets2),
            case Hit of
                none ->
                    {NewHits, Bullets3};
                {_, _} ->
                    {[Hit|NewHits], Bullets3}
            end;
        (_CF, [], RestBullets) ->
            {[], RestBullets};
        (_CF, _RestPlayers, []) ->
            {[], []}
    end,
    {Hits, NewBullets} = Coll(Coll, Players, Bullets),
    UpdateScores = fun({TargetId, ShooterId}) ->
        tri_scores:target_hit(TargetId, ShooterId)
    end,
    lists:foreach(UpdateScores, Hits),
    {NewBullets, Hits /= []}.


% external interface
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

client_cmd(Cmd, Args) ->
    gen_server:cast(?MODULE, {client_cmd, {Cmd, Args, self()}}).

add_player(PlayerId, ConnPid) ->
    gen_server:cast(?MODULE, {add_player, {PlayerId, self(), ConnPid}}).




