-module(db).
-record(animals, {name, location}).
-record(obstacles, {location, name}).
-record(teleporters, {location, node, destination}).
-compile(export_all).

init(State) ->
  {_Grid, Teleporters, Obstacles} = State,
  load_teleporters(Teleporters),
  load_obstacles(Obstacles).

get_animal(Name) ->
  case mnesia:dirty_read(animals, Name) of
    [] -> [];
    [#animals{name=Name, location=Location}] -> [{Name, Location}]
  end.

add_animal(Name, Location) ->
  mnesia:dirty_write(#animals{name=Name, location=Location}).

update_animal(Name, Location) ->
  mnesia:dirty_write(#animals{name=Name, location=Location}),
  {Name, Location}.

delete_animal(Name) ->
  mnesia:dirty_delete(animals, Name).

animal_at_location(Location) ->
  mnesia:dirty_match_object(#animals{name='$0', location=Location}).

obstacle_at_location(Location) ->
  mnesia:dirty_match_object(#obstacles{location=Location, name='$1'}).

teleporter_at_location(Location) ->
  case mnesia:dirty_read(teleporters, Location) of
    [] -> [];
    [#teleporters{location=Location, node=Node, destination=Destination}] -> [{Node, Location, Destination}]
  end.

%%% private functions
load_obstacles(Obstacles) ->
  [load_obstacle(O) || O <- Obstacles].

load_obstacle({Obstacle, Locations}) ->
  [write_obstacle(L, O) || L <- Locations, O <- [Obstacle]].

write_obstacle(Location, Name) ->
  mnesia:dirty_write(#obstacles{location=Location, name=Name}).

load_teleporters(Teleporters) ->
  [write_teleporter(T) || T <- Teleporters].

write_teleporter({Node, Location, Destination}) ->
  mnesia:dirty_write(#teleporters{location=Location, node=Node, destination=Destination}).
