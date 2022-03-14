-module(db).
-record(animal, {name, location}).
-record(obstacle, {location, name}).
-compile(export_all).

init(State) ->
  {_Grid, _Teleporters, Obstacles} = State,
  ets:new(animals, [set, {keypos, #animal.name}, named_table]),
  ets:new(obstacles, [set, {keypos, #obstacle.location}, named_table]),
  load_obstacles(Obstacles).

get_animal(Name) ->
  case ets:lookup(animals, Name) of
    [] -> [];
    [#animal{name=Name, location=Location}] -> [{Name, Location}]
  end.

add_animal(Name, Location) ->
  ets:insert(animals, #animal{name=Name, location=Location}).

update_animal(Name, Location) ->
  delete_animal(Name),
  add_animal(Name, Location),
  {Name, Location}.

delete_animal(Name) ->
  ets:match_delete(animals, #animal{name=Name}).

animal_at_location(Location) ->
  ets:match_object(animals, #animal{name='$0', location=Location}).

obstacle_at_location(Location) ->
  ets:match_object(obstacles, #obstacle{location=Location, name='$1'}).

%%% private functions
load_obstacles(Obstacles) ->
  [load_obstacle(O) || O <- Obstacles].

load_obstacle({Obstacle, Locations}) ->
  [ets:insert(obstacles, #obstacle{location=L, name=O}) || L <- Locations, O <- [Obstacle]].
