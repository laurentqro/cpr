-module(world).

-export([start_link/1, get/1, move/2, delete/1]).
-export([init/1, loop/1, validate_move/2, make_move/2]).

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
  {GridDimensions, _Teleporters, _Obstacles} = State,
  ets:new(animals, [set, named_table]),
  loop(GridDimensions).

loop(GridDimensions) ->
  receive
    {From, validate_move, Move} ->
      case validate_move(Move, GridDimensions) of
        ok ->
          From ! ok;
        {error, Reason} ->
          From ! {error, Reason}
      end,
      loop(GridDimensions);
    {move, AnimalName, Move} ->
      make_move(AnimalName, Move),
      loop(GridDimensions);
    {From, get, AnimalName} ->
      From ! {ok, ets:lookup(animals, AnimalName)},
      loop(GridDimensions);
    {delete, AnimalName} ->
      ets:delete(animals, AnimalName),
      loop(GridDimensions)
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

validate_move({X,Y} = _Move, {P,Q} = _GridDimensions) ->
  if
    (X > P) or (Y > Q) ->
      {error, out_of_bounds};
    (X < 0) or (Y < 0) ->
      {error, must_be_positive_integer};
    (is_integer(X) == false) or (is_integer(Y) == false) ->
      {error, must_be_positive_integer};
    true ->
      ok
  end.
