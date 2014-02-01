-module(tri_player).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/1, stop/1, client_cmd/3,
         tick/2, get_client_info/1, touch_border/4]).

-include("player.hrl").


% behaviour callbacks
init([ConnPid, PlayerData]) ->
    tri_world:add_player(pid_to_id(), ConnPid),
    Player = #player{
        name=dict:fetch(name, PlayerData),
        pos=dict:fetch(pos, PlayerData),
        angle=dict:fetch(angle, PlayerData)
    },
    {ok, Player}.


handle_call({tick, DT}, _From, #player{speed=Speed1, pos=Pos,
                                       angle=Angle, force=Force} = Player) ->
    [X1, Y1] = Pos,
    [SX1, SY1] = Speed1,
    [FX, FY] = Force,
    {ok, MaxSpeed} = application:get_env(tri, max_speed),
    {ok, FF} = application:get_env(tri, force_factor),
    Speed2 = [SX2, SY2] = [SX1 + FX * DT * FF, SY1 + FY * DT * FF],
    Speed = math:sqrt(math:pow(SX2, 2) + math:pow(SY2, 2)),
    Speed3 = [SX3, SY3] = if
        Speed =< MaxSpeed ->
            Speed2;
        true ->
            SCoef = Speed / MaxSpeed,
            [SX2 / SCoef, SY2 / SCoef]
    end,

    NewPos = [trunc(X1 + SX3 * DT), trunc(Y1 + SY3 * DT)],
    Bullet = none, %TODO
    Resp = {ok, NewPos, Angle, Bullet},
    {reply, Resp, Player#player{pos=NewPos, speed=Speed3}};

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


terminate(_Reason, _Player) ->
    ok.


% internal functions
handle_client_cmd(commands, Args, Player) ->
    [Length, Angle] = dict:fetch(move_vector, Args),
    Player#player{
        angle=Angle,
        force=vect_transform(Length, Angle)
    }.

vect_transform(Length, Angle) ->
    Rad = Angle / 57.3,
    X = Length * math:cos(Rad),
    Y = Length * math:sin(Rad),
    [X, Y].

pid_to_id() ->
    list_to_binary("user:" ++ pid_to_list(self())).


% external interface
start_link(Data) ->
    gen_server:start_link(?MODULE, [self(), Data], []).

stop(PlayerPid) ->
    gen_server:cast(PlayerPid, stop).

client_cmd(PlayerPid, Cmd, Args) ->
    gen_server:cast(PlayerPid, {client_cmd, {Cmd, Args}}).

tick(PlayerPid, DT) ->
    Seconds = DT / 1000,
    gen_server:call(PlayerPid, {tick, Seconds}).

get_client_info(PlayerdPid) ->
    gen_server:call(PlayerdPid, get_client_info).

touch_border(PlayerdPid, Pos, Data, ReflFactor) ->
    gen_server:cast(PlayerdPid, {touch_border, Pos, Data, ReflFactor}).


