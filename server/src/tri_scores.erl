-module(tri_scores).

-behaviour(gen_server).
-export([init/1, handle_cast/2, terminate/2]).
-export([start_link/0, client_cmd/2, target_hit/2, send_update/1]).

% behaviour callbacks
init(_Args) ->
    Scores = dict:new(),
    {ok, Scores}.

handle_cast({client_cmd, {Cmd, Args, ConnPid}}, Scores) ->
    NewScores = handle_client_cmd(Cmd, Args, ConnPid, Scores),
    {noreply, NewScores};
handle_cast({target_hit, {TargetId, ShooterId}}, Scores) ->
    NewScores = handle_target_hit(TargetId, ShooterId, Scores),
    {noreply, NewScores};
handle_cast({send_update, ConnPids}, Scores) ->
    ToSend = [{scores, dict:to_list(Scores)}],
    tri_controller:broadcast(ConnPids, 'scores.update', ToSend),
    {noreply, Scores}.


terminate(_Reason, _Scores) ->
    ok.


% internal functions
handle_client_cmd(request_update, _Args, ConnPid, Scores) ->
    ToSend = [{scores, dict:to_list(Scores)}],
    tri_controller:send(ConnPid, 'scores.update', ToSend),
    Scores.

handle_target_hit(TargetId, ShooterId, Scores1) ->
    TargetScore = case dict:find(TargetId, Scores1) of
        {ok, TValue} -> max(TValue - 1, 0);
        error -> 0
    end,
    Scores2 = dict:store(TargetId, TargetScore, Scores1),
    ShooterScore = case dict:find(ShooterId, Scores2) of
        {ok, SValue} -> SValue + 1;
        error -> 1
    end,
    Scores3 = dict:store(ShooterId, ShooterScore, Scores2),
    Scores3.


% external interface
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

client_cmd(Cmd, Args) ->
    gen_server:cast(?MODULE, {client_cmd, {Cmd, Args, self()}}).

target_hit(TargetId, ShooterId) ->
    gen_server:cast(?MODULE, {target_hit, {TargetId, ShooterId}}).

send_update(ConnPids) ->
    gen_server:cast(?MODULE, {send_update, ConnPids}).

%TODO: delete scores of disconnected players
%TODO: get player's names
