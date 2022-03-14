-module(db).
-record(animals, {name, location}).
-record(obstacles, {location, name}).
-compile(export_all).

init(State) ->
  {_Grid, _Teleporters, Obstacles} = State,
  load_obstacles(Obstacles).

get_animal(Name) ->
  F = fun() ->
    case mnesia:read(animals, Name) of
      [] -> [];
      [#animals{name=Name, location=Location}] -> [{Name, Location}]
    end
  end,
  mnesia:activity(transaction, F).

add_animal(Name, Location) ->
  F = fun() ->
          mnesia:write(#animals{name=Name, location=Location})
      end,
  mnesia:activity(transaction, F).

update_animal(Name, Location) ->
  F = fun() ->
          mnesia:write(#animals{name=Name, location=Location}),
          {Name, Location}
      end,
  mnesia:activity(transaction, F).

delete_animal(Name) ->
  F = fun() ->
          mnesia:delete(animals, Name)
      end,
  mnesia:activity(transaction, F).

animal_at_location(Location) ->
  F = fun() ->
          mnesia:match_object(#animals{name='$0', location=Location})
      end,
  mnesia:activity(transaction, F).

obstacle_at_location(Location) ->
  F = fun() ->
          mnesia:match_object(#obstacles{location=Location, name='$1'})
      end,
  mnesia:activity(transaction, F).

%%% private functions
load_obstacles(Obstacles) ->
  [load_obstacle(O) || O <- Obstacles].

load_obstacle({Obstacle, Locations}) ->
  [write_obstacle(L, O) || L <- Locations, O <- [Obstacle]].

write_obstacle(Location, Name) ->
  F = fun() ->
          mnesia:write(#obstacles{location=Location, name=Name})
      end,
  mnesia:activity(transaction, F).
