-module(tri_player).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/1, stop/1, client_cmd/3,
         tick/2, get_client_info/1, touch_border/4]).

-include("player.hrl").


% behaviour callbacks
init([ConnPid, PlayerData]) ->
    process_flag(trap_exit, true),
    Id = pid_to_id(),
    Name = dict:fetch(name, PlayerData),
    tri_world:add_player(Id, ConnPid),
    tri_scores:add_player(Id, Name, ConnPid),
    Player = #player{
        id=Id,
        name=Name,
        pos=dict:fetch(pos, PlayerData),
        angle=dict:fetch(angle, PlayerData),
        last_fire=tri_utils:ms()
    },
    {ok, Player}.


handle_call({tick, DT}, _From, Player1) ->
    {{Pos, Angle}, Player2} = tick_move(DT, Player1),
    {Bullet, Player3} = tick_fire(Player2),
    Resp = {ok, Pos, Angle, Bullet},
    {reply, Resp, Player3};
handle_call(get_client_info, _From,
        #player{name=Name, pos=Pos, angle=Angle} = Player) ->
    Info = [
        {type, triangle},
        {name, Name},
        {pos, Pos},
        {angle, Angle}
    ],
    {reply, Info, Player}.


handle_cast({client_cmd, {Cmd, Args}}, Player) ->
    NewPlayer = handle_client_cmd(Cmd, Args, Player),
    {noreply, NewPlayer};
handle_cast({touch_border, Pos, {DX, DY}, ReflFactor}, Player) ->
    [SX1, SY1] = Player#player.speed,
    {DSX, DSY} = {SX1 / abs(SX1), SY1 / abs(SY1)},
    SX2 = case DX == 0 orelse DSX == DX of
        true -> SX1;
        false -> -SX1
    end,
    SY2 = case DY == 0 orelse DSY == DY of
        true -> SY1;
        false -> -SY1
    end,
    NewSpeed = [SX2 * ReflFactor, SY2 * ReflFactor],
    {noreply, Player#player{pos=Pos, speed=NewSpeed}};
handle_cast(stop, Player) ->
    {stop, normal, Player}.


terminate(_Reason, Player) ->
    tri_scores:remove_player(Player#player.id),
    ok.


% internal functions
handle_client_cmd(commands, Args, Player) ->
    [Length, Angle] = dict:fetch(move_vector, Args),
    NewAngle = if
        Length == 0 -> Player#player.angle;
        true -> Angle
    end,
    Player#player{
        angle=NewAngle,
        force=tri_utils:vect_transform(Length, NewAngle),
        fire=dict:fetch(fire, Args)
    }.

pid_to_id() ->
    list_to_binary("user:" ++ pid_to_list(self())).

tick_move(DT, #player{speed=Speed1, pos=Pos,
                      angle=Angle, force=Force} = Player) ->
    [X1, Y1] = Pos,
    [SX1, SY1] = Speed1,
    [FX, FY] = Force,
    {ok, MaxSpeed} = application:get_env(tri, max_speed),
    {ok, FF} = application:get_env(tri, force_factor),
    Speed2 = [SX2, SY2] = [SX1 + FX * DT * FF, SY1 + FY * DT * FF],
    Speed = tri_utils:vect_length(SX2, SY2),
    Speed3 = [SX3, SY3] = if
        Speed =< MaxSpeed ->
            Speed2;
        true ->
            SCoef = Speed / MaxSpeed,
            [SX2 / SCoef, SY2 / SCoef]
    end,
    NewPos = [trunc(X1 + SX3 * DT), trunc(Y1 + SY3 * DT)],
    {{NewPos, Angle}, Player#player{pos=NewPos, speed=Speed3}}.


tick_fire(Player) ->
    case Player#player.fire of
        false ->
            {false, Player};
        true ->
            {ok, FR} = application:get_env(tri, fire_rate),
            LF = Player#player.last_fire,
            Now = tri_utils:ms(),
            if
                (Now - LF) / 1000 < 1 / FR ->
                    {none, Player};
                true ->
                    [PSX, PSY] = Player#player.speed,
                    {ok, BulletSpeed} = application:get_env(tri, bullet_speed),
                    [BSX, BSY] = tri_utils:vect_transform(BulletSpeed,
                                                Player#player.angle),
                    Speed = [BSX + PSX, BSY + PSY],
                    Bullet = {Player#player.pos, Speed},
                    {Bullet, Player#player{last_fire=Now}}
            end
    end.



% external interface
start_link(Data) ->
    gen_server:start_link(?MODULE, [self(), Data], []).

stop(PlayerPid) ->
    gen_server:cast(PlayerPid, stop).

client_cmd(PlayerPid, Cmd, Args) ->
    gen_server:cast(PlayerPid, {client_cmd, {Cmd, Args}}).

tick(PlayerPid, DT) ->
    gen_server:call(PlayerPid, {tick, DT}).

get_client_info(PlayerdPid) ->
    gen_server:call(PlayerdPid, get_client_info).

touch_border(PlayerdPid, Pos, Data, ReflFactor) ->
    gen_server:cast(PlayerdPid, {touch_border, Pos, Data, ReflFactor}).


