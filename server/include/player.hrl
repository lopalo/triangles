

% internal player's state
-record(player, {id, name, angle=0, pos=[0, 0],
                 speed=[0, 0], force=[0, 0],
                 fire=false, last_fire}).
