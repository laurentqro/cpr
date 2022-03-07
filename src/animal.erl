-module(animal).

-export([start_link/2, move/2, sleep/2, stop/1]).
-export([init/2, loop/1, go_to_sleep/2, make_move/2]).

%%% Client API
start_link(Name, Location) ->
  Pid = spawn(?MODULE, init, [Name, Location]),
  register(Name, Pid),
  {ok, Pid}.

move(Name, Move) ->
  Name ! {move, Move, Name},
  ok.

sleep(Name, Time) ->
  Name ! {sleep, Time},
  ok.

stop(Name) ->
  Name ! stop,
  ok.

%%% Server functions
init(Name, Location) ->
  make_move(Name, Location),
  ok.

loop({X, Y} = Location) ->
  receive
    {move, up, Name} ->
      make_move(Name, {X, Y + 1});
    {move, down, Name} ->
      make_move(Name, {X, Y - 1});
    {move, right, Name} ->
      make_move(Name, {X + 1, Y});
    {move, left, Name} ->
      make_move(Name, {X - 1, Y});
    {sleep, Time } ->
      go_to_sleep(Location, Time);
    stop ->
      ok
  end.

go_to_sleep(Location, Time) ->
  io:format("Sleeping ~p milliseconds ...\n", [Time]),
  receive
  after
    Time ->
      loop(Location)
  end.

make_move(Name, Move) ->
  world:move(Name, Move),
  receive
    {ok, Move} ->
      io:format("~p: Moved to position ~p\n", [Name, Move]),
      loop(Move);
    {error, Reason} ->
      io:format("Failed to move into position ~p: ~p\n", [Move, Reason])
  after 1000 ->
    timeout
  end.
