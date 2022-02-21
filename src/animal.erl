-module(animal).

-export([start_link/2, move/2, sleep/2, stop/1]).
-export([loop/1, go_to_sleep/2, print_move/2]).

%%% Client API
start_link(Name, {X, Y} = Location) when X >= 0, Y >= 0 ->
  Pid = spawn(?MODULE, loop, [Location]),
  register(Name, Pid),
  io:format("~p: Started in position ~p\n", [Name, Location]),
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
loop({X, Y} = Location) ->
  receive
    {move, up, Name} ->
      NewLocation = {X, Y + 1},
      print_move(Name, NewLocation),
      loop(NewLocation),
      ok;
    {move, down, Name} ->
      NewLocation = {X, Y - 1},
      print_move(Name, NewLocation),
      loop(NewLocation),
      ok;
    {move, right, Name} ->
      NewLocation = {X + 1, Y},
      print_move(Name, NewLocation),
      loop(NewLocation),
      ok;
    {move, left, Name} ->
      NewLocation = {X - 1, Y},
      print_move(Name, NewLocation),
      loop(NewLocation),
      ok;
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

print_move(Name, Location) ->
  io:format("~p: Moved to position ~p\n", [Name, Location]).
