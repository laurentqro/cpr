-module(animal_world).
-behaviour(application).
-export([start/2, stop/1, install/1, uninstall/1]).

-record(animals, {name, location}).
-record(obstacles, {location, name}).
-record(teleporters, {location, name, destination}).

install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  rpc:multicall(Nodes, application, start, [mnesia]),
  mnesia:create_table(animals,
                      [{attributes, record_info(fields, animals)},
                       {local_content, true},
                       {disc_copies, Nodes}]),
  mnesia:create_table(obstacles,
                      [{attributes, record_info(fields, obstacles)},
                       {local_content, true},
                       {disc_copies, Nodes}]),
  mnesia:create_table(teleporters,
                      [{attributes, record_info(fields, teleporters)},
                       {local_content, true},
                       {disc_copies, Nodes}]).

uninstall(Nodes) ->
  rpc:multicall(Nodes, application, stop, [mnesia]),
  ok = mnesia:delete_schema(Nodes).

start(normal, _) ->
  mnesia:wait_for_tables([animals, obstacles, teleporters], 5000),
  ChildList = [{world, start_link, ["node1_config.txt"]}, {animal_sup, start_link, []}],
  world_sup:start_link(ChildList).

stop(_) ->
  world_sup:stop().
