-module(tri_scores).

-behaviour(gen_server).
-export([init/1, handle_cast/2, terminate/2]).
-export([start_link/0, client_cmd/2, add_player/3,
         remove_player/1, target_hit/2, send_update/0]).

% behaviour callbacks
init(_Args) ->
    Data = dict:new(),
    {ok, Data}.

handle_cast({add_player, {PlayerId, Name, ConnPid}}, Data) ->
    NewData = dict:store(PlayerId, {Name, ConnPid, 0}, Data),
    update(NewData),
    {noreply, NewData};
handle_cast({remove_player, PlayerId}, Data) ->
    NewData = dict:erase(PlayerId, Data),
    update(NewData),
    {noreply, NewData};
handle_cast({client_cmd, {Cmd, Args, ConnPid}}, Data) ->
    NewData = handle_client_cmd(Cmd, Args, ConnPid, Data),
    {noreply, NewData};
handle_cast({target_hit, {TargetId, ShooterId}}, Data) ->
    NewData = handle_target_hit(TargetId, ShooterId, Data),
    {noreply, NewData};
handle_cast(send_update, Data) ->
    update(Data),
    {noreply, Data}.


terminate(_Reason, _Data) ->
    ok.


% internal functions
handle_client_cmd(request_update, _Args, ConnPid, Data) ->
    ToSend = [{scores, [{Name, Score} ||
                    {_K, {Name, _, Score}} <- dict:to_list(Data)]}],
    tri_controller:send(ConnPid, 'scores.update', ToSend),
    Data.

handle_target_hit(TargetId, ShooterId, Data1) ->
    {TName, TConnPid, TScore} = dict:fetch(TargetId, Data1),
    TargetData = {TName, TConnPid, max(TScore - 1, 0)},
    Data2 = dict:store(TargetId, TargetData, Data1),
    {SName, SConnPid, SScore} = dict:fetch(ShooterId, Data2),
    ShooterData = {SName, SConnPid, SScore + 1},
    dict:store(ShooterId, ShooterData, Data2).

update(Data) ->
    List = [V || {_K, V} <- dict:to_list(Data)],
    ToSend = [{scores, [{Name, Score} || {Name, _, Score} <- List]}],
    ConnPids = [C || {_, C, _} <- List],
    tri_controller:broadcast(ConnPids, 'scores.update', ToSend).


% external interface
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_player(PlayerId, Name, ConnPid) ->
    gen_server:cast(?MODULE, {add_player, {PlayerId, Name, ConnPid}}).

remove_player(PlayerId) ->
    gen_server:cast(?MODULE, {remove_player, PlayerId}).

client_cmd(Cmd, Args) ->
    gen_server:cast(?MODULE, {client_cmd, {Cmd, Args, self()}}).

target_hit(TargetId, ShooterId) ->
    gen_server:cast(?MODULE, {target_hit, {TargetId, ShooterId}}).

send_update() ->
    gen_server:cast(?MODULE, send_update).
