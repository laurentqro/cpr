-module(world).
-behaviour(gen_server).
-compile(export_all).

%%% Client API
start_link(FileName) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

get(AnimalName) ->
  gen_server:call(?MODULE, {get, AnimalName}).

move(AnimalName, Move) ->
  gen_server:call(?MODULE, {move, AnimalName, Move}).

delete(AnimalName) ->
  gen_server:cast(?MODULE, {delete, AnimalName}).

%%% Server functions
init(FileName) ->
  {ok, [State]} = file:consult(FileName),
  db:init(State),
  {ok, State}.

handle_call({get, AnimalName}, _From, State) ->
  {reply, db:get_animal(AnimalName), State};

handle_call({move, AnimalName, Move}, _From, State) ->
  {Grid, _, _} = State,
  case validate_move(Move, Grid) of
    ok ->
      make_move(AnimalName, Move);
    {teleport, {Node, Destination}} ->
      teleport(Node, AnimalName, Destination);
    {error, Reason} ->
      io:format("Could not move into ~p: ~p.~n", [Move, Reason])
  end,
  {reply, db:get_animal(AnimalName), State}.

handle_cast({delete, AnimalName}, State) ->
  db:delete_animal(AnimalName),
  {noreply, State}.

%%% private functions
validate_move(Move, Grid) ->
  case validate_positive_numbers(Move, Grid) of
    ok ->
      ok;
    {teleport, Target} ->
      {teleport, Target};
    {error, Reason} ->
      {error, Reason}
  end.

validate_positive_numbers({X,Y} = Move, Grid) ->
  case (X > 0) and (Y > 0) of
    true -> validate_integers(Move, Grid);
    false -> {error, must_be_positive}
  end.

validate_integers({X,Y} = Move, Grid) ->
  case (is_integer(X)) and (is_integer(Y)) of
    true  -> validate_is_within_bounds(Move, Grid);
    false -> {error, must_be_an_integer}
  end.

validate_is_within_bounds({X,Y} = Move, {P,Q} = _Grid) ->
  case (X =< P) and (Y =< Q) of
    true  -> validate_has_no_animal(Move);
    false -> {error, out_of_bounds}
  end.

validate_has_no_animal(Location) ->
  case db:animal_at_location(Location) of
    [] -> validate_has_no_obstacle(Location);
    [_Animal] -> {error, location_has_animal}
  end.

validate_has_no_obstacle(Location) ->
  case db:obstacle_at_location(Location) of
    [] -> validate_teleporter(Location);
    [_Obstacle] -> {error, location_has_obstacle}
  end.

validate_teleporter(Move) ->
  case db:teleporter_at_location(Move) of
   [{Node, _, Destination}] ->
      {teleport, {Node, Destination}};
    [] -> ok
  end.

make_move(AnimalName, Move) ->
  case db:get_animal(AnimalName) of
    [] ->
      io:format("~p placed into ~p~n", [AnimalName, Move]),
      db:add_animal(AnimalName, Move);
    [{AnimalName, _}] ->
      io:format("~p moved into ~p~n", [AnimalName, Move]),
      db:update_animal(AnimalName, Move)
  end.

teleport(Node, AnimalName, Destination) ->
  io:format("+++> TELEPORT!"),
  spawn(Node, animal_sup, add_animal, [AnimalName, Destination]),
  spawn(animal_sup, remove_animal, [AnimalName]),
  ok.
