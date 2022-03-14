-module(animal_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, stop/0, add_animal/2, remove_animal/2]).

start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).

stop() -> exit(whereis(?MODULE), shutdown).

init(_) ->
  SupFlags   = #{strategy => simple_one_for_one, intensity => 5, period => 5000},
  ChildSpecs = [#{id => animal,
                  start => {animal, start_link, []},
                  restart => permanent,
                  shutdown => 2000,
                  type => worker,
                  modules => [animal]}],
  {ok, {SupFlags, ChildSpecs}}.

add_animal(AnimalName, Location) ->
  supervisor:start_child(whereis(?MODULE), [AnimalName, Location]).

remove_animal(SupPid, AnimalName) ->
  supervisor:terminate_child(SupPid, whereis(AnimalName)).
