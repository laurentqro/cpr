-module(world_sup).
-behaviour(supervisor).
-export([start_link/1, init/1, stop/0, child/1]).

start_link(ChildList) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ChildList).

stop() -> exit(whereis(?MODULE), shutdown).

init(ChildList) ->
  SupFlags   = #{strategy => one_for_one, intensity => 3, period => 60},
  ChildSpecs = [child(Child) || Child <- ChildList],
  {ok, {SupFlags, ChildSpecs}}.

child({world = Module, Func, Args}) ->
  #{id => Module,
    start => {Module, Func, Args},
    restart => transient,
    shutdown => 2000,
    type => worker,
    modules => [Module]};

child({animal_sup = Module, Func, Args}) ->
  #{id => Module,
    start => {Module, Func, Args},
    restart => transient,
    shutdown => 2000,
    type => supervisor,
    modules => [Module]}.
