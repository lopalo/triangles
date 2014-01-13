-module(tri_world).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0, client_cmd/2, add_player/2]).

-include("settings.hrl").

-record(state, {players, bullets, last_update}).

% behaviour callbacks
init(_Args) ->
    Players = ets:new(players, [set, private, {keypos, 1}]),
    Bullets = ets:new(bullets, [set, private, {keypos, 1}]),
    timer:send_after(?SERVER_TICK, tick),
    {ok, #state{players=Players, bullets=Bullets, last_update=ms()}}.

handle_cast({client_cmd, {Cmd, Args, ConnPid}}, State) ->
    NewState = handle_client_cmd(Cmd, Args, ConnPid, State),
    {noreply, NewState};
handle_cast({add_player, {PlayerId, PlayerPid, ConnPid} = Info}, State) ->
    true = ets:insert_new(State#state.players, Info),
    erlang:monitor(process, PlayerPid),
    send_init(ConnPid, PlayerId),
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    erlang:demonitor(Ref, [flush]),
    ets:delete(State#state.players, Pid),
    {noreply, State};
handle_info(tick, State) ->
    timer:send_after(?SERVER_TICK, tick),
    {TickData, NewState} = handle_tick(State),
    ConnPids = [C || [C] <- ets:match(State#state.players, {'_', '_', '$3'})],
    tri_controller:broadcast(ConnPids, 'world.tick', [{tick_data, TickData}]),
    {noreply, NewState}.


terminate(_Reason, _State) ->
    ok.

% internal functions
handle_client_cmd(start, Args, ConnPid, State) ->
    tri_connection:create_player(ConnPid, Args),
    State;
handle_client_cmd(get_objects_info, Args, ConnPid, State) ->
    Players = State#state.players,
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
    tri_controller:send(ConnPid, 'world.objects_info', [{objects, Objects}]),
    State.


send_init(ConnPid, PlayerId) ->
    StartData = [
        {uid, PlayerId},
        {server_tick, ?SERVER_TICK},
        {level_size, ?LEVEL_SIZE}
    ],
    tri_controller:send(ConnPid, 'world.init', StartData).

ms() ->
    N = now(),
    element(2, N) * 1000 + trunc(element(3, N) / 1000).

handle_tick(State) ->
    Now = ms(),
    LastUpdate = State#state.last_update,
    NewState = State#state{last_update=Now},
    DT = max(Now - LastUpdate, 0),
    PlayersTick = fun([PlayerId, PlayerPid], Data) ->
        {ok, Pos, Angle} = tri_player:tick(PlayerPid, DT),
        PlayerData = [{pos, Pos}, {angle, Angle}],
        [{PlayerId, PlayerData}|Data]
    end,
    Players = ets:match(State#state.players, {'$1', '$2', '_'}),
    TickData = lists:foldl(PlayersTick, [], Players),
    {TickData, NewState}.


% external interface
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

client_cmd(Cmd, Args) ->
    gen_server:cast(?MODULE, {client_cmd, {Cmd, Args, self()}}).

add_player(PlayerId, ConnPid) ->
    gen_server:cast(?MODULE, {add_player, {PlayerId, self(), ConnPid}}).




