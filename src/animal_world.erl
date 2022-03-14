-module(animal_world).
-behaviour(application).
-export([start/2, stop/1, install/1]).

-record(animals, {name, location}).
-record(obstacles, {location, name}).

install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  application:start(mnesia),
  mnesia:create_table(animals,
                      [{attributes, record_info(fields, animals)},
                       {disc_copies, Nodes}]),
  mnesia:create_table(obstacles,
                      [{attributes, record_info(fields, obstacles)},
                       {disc_copies, Nodes}]),
  application:stop(mnesia).

start(normal, _Args) ->
  mnesia:wait_for_tables([animals, obstacles], 10000),
  ChildList = [{world, start_link, ["config.txt"]}, {animal_sup, start_link, []}],
  world_sup:start_link(ChildList).

stop(_) ->
  world_sup:stop().
