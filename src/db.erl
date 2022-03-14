-module(db).
-compile(export_all).

init(State) ->
  {_Grid, _Teleporters, Obstacles} = State,
  ets:new(animals, [set, named_table]),
  ets:new(obstacles, [set, named_table]),
  load_obstacles(Obstacles).

get_animal(Name) ->
  ets:lookup(animals, Name).

add_animal(Name, Location) ->
  ets:insert(animals, {Name, Location}).

update_animal(Name, Location) ->
  ets:update_element(animals, Name, {2, Location}).

delete_animal(Name) ->
  ets:delete(animals, Name).

animal_at_location(Location) ->
  ets:match_object(animals, {'$0', Location}).

obstacle_at_location(Location) ->
  ets:match_object(obstacles, {Location, '$1'}).

%%% private functions
load_obstacles(Obstacles) ->
  [load_obstacle(O) || O <- Obstacles].

load_obstacle({Obstacle, Locations}) ->
  [ets:insert(obstacles, {L, O}) || L <- Locations, O <- [Obstacle]].
