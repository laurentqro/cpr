-module(animal_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, stop/0, add_animal/3]).

start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).

stop() -> exit(whereis(?MODULE), shutdown).

init(_Args) ->
  SupFlags   = #{strategy => simple_one_for_one, intensity => 0, period => 1},
  ChildSpecs = [#{id => animal,
                  start => {animal, start_link, []},
                  restart => permanent,
                  shutdown => 2000,
                  type => worker,
                  modules => [animal]}],
  {ok, {SupFlags, ChildSpecs}}.

add_animal(SupId, AnimalName, Location) ->
  supervisor:start_child(SupId, [AnimalName, Location]).