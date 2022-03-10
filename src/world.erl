-module(world).

-export([start_link/1, get/1, move/2, delete/1]).
-export([init/1, loop/1, validate_move/2, make_move/2, validate_positive_integers/2, validate_is_within_bounds/2, validate_has_no_animal/1]).

%%% Client API
start_link(FileName) ->
  {ok, [State]} = file:consult(FileName),
  Pid = spawn(?MODULE, init, [State]),
  register(?MODULE, Pid),
  {ok, Pid}.

get(AnimalName) ->
  ?MODULE ! {self(), get, AnimalName},
  receive
    {ok, [{_AnimalName, Location}]} -> {ok, Location};
    {ok, []} -> {error, not_found}
  after 1000 ->
          timeout
  end.

move(AnimalName, Move) ->
  ?MODULE ! {self(), validate_move, Move},

  receive
    ok ->
      ?MODULE ! {move, AnimalName, Move};
    {error, Reason} ->
      AnimalName ! {error, Reason}
  after 1000 ->
          timeout
  end.

delete(AnimalName) ->
  ?MODULE ! {delete, AnimalName},
  ok.

%% Server
init(State) ->
  {Grid, _Teleporters, _Obstacles} = State,
  ets:new(animals, [set, named_table]),
  loop(Grid).

loop(Grid) ->
  receive
    {From, validate_move, Move} ->
      case validate_move(Move, Grid) of
        ok ->
          From ! ok;
        {error, Reason} ->
          From ! {error, Reason}
      end,
      loop(Grid);
    {move, AnimalName, Move} ->
      make_move(AnimalName, Move),
      loop(Grid);
    {From, get, AnimalName} ->
      From ! {ok, ets:lookup(animals, AnimalName)},
      loop(Grid);
    {delete, AnimalName} ->
      ets:delete(animals, AnimalName),
      loop(Grid)
  end.

make_move(AnimalName, Move) ->
  Result = ets:lookup(animals, AnimalName),
  case Result of
    [] ->
      ets:insert(animals, {AnimalName, Move}),
      AnimalName ! {ok, Move};
    [{AnimalName, _Location}] ->
      ets:update_element(animals, AnimalName, {2, Move}),
      AnimalName ! {ok, Move}
  end.

validate_move(Move, Grid) ->
  case validate_positive_integers(Move, Grid) of
    ok -> ok;
    {error, Reason} -> {error, Reason}
  end.

validate_positive_integers({X,Y} = Move, Grid) ->
  case (is_integer(X)) and (is_integer(Y)) of
    true  -> validate_is_within_bounds(Move, Grid);
    false -> {error, negative_integer}
  end.

validate_is_within_bounds({X,Y} = Move, {P,Q} = _Grid) ->
  case (X =< P) and (Y =< Q) of
    true  -> validate_has_no_animal(Move);
    false -> {error, out_of_bounds}
  end.

validate_has_no_animal(Location) ->
  case ets:match(animals, {'$0', Location}) of
    [] -> ok;
    [_Animal] -> {error, location_has_animal}
  end.
