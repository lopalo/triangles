-module(tri_player).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/1, stop/1, client_cmd/3, tick/2, get_client_info/1]).
-export([make_player/5]).

-include("settings.hrl").


-record(player, {uid, name, angle=0, pos=[0, 0], speed=[0, 0]}).


% behaviour callbacks
init([ConnPid, PlayerData]) ->
    tri_world:add_player(pid_to_id(), ConnPid),
    Player = #player{
        name=dict:fetch(name, PlayerData),
        pos=dict:fetch(pos, PlayerData),
        angle=dict:fetch(angle, PlayerData)
    },
    {ok, Player}.


handle_call({tick, DT}, _From,
        #player{speed=Speed, pos=Pos, angle=Angle} = Player) ->
    [CX, CY] = Pos,
    [SX, SY] = Speed,
    {ok, SpeedFactor} = application:get_env(tri, speed_factor),
    X = trunc(CX + SX * DT * SpeedFactor),
    Y = trunc(CY + SY * DT * SpeedFactor),
    NewPos = [X, Y],
    {reply, {ok, NewPos, Angle}, Player#player{pos=NewPos}};

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
handle_cast(stop, Player) ->
    {stop, normal, Player}.


terminate(_Reason, _Player) ->
    ok.


% internal functions
handle_client_cmd(commands, Args, Player) ->
    [Length, Angle] = dict:fetch(move_vector, Args),
    Player#player{
        angle=Angle,
        speed=vect_transform(Length, Angle)
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

tick(PlayerPid, Dt) ->
    gen_server:call(PlayerPid, {tick, Dt}).

get_client_info(PlayerdPid) ->
    gen_server:call(PlayerdPid, get_client_info).


% test function
make_player(Uid, Name, Angle, Pos, Speed) ->
    #player{uid=Uid, name=Name, angle=Angle, pos=Pos, speed=Speed}.

