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
  {_Grid, _Teleporters, Obstacles} = State,
  ets:new(animals, [set, named_table]),
  ets:new(obstacles, [set, named_table]),
  load_obstacles(Obstacles),
  {ok, State}.

handle_call({get, AnimalName}, _From, State) ->
  {reply, ets:lookup(animals, AnimalName), State};

handle_call({move, AnimalName, Move}, _From, State) ->
  {Grid, _, _} = State,
  case validate_move(Move, Grid) of
    ok ->
      make_move(AnimalName, Move),
      {reply, ets:lookup(animals, AnimalName), State};
    {error, Reason} ->
      io:format("Could not move into ~p: ~p.~n", [Move, Reason]),
      {reply, ets:lookup(animals, AnimalName), State}
  end.

handle_cast({delete, AnimalName}, State) ->
  ets:delete(animals, AnimalName),
  {noreply, State}.

%%% private functions
load_obstacles(Obstacles) ->
  [load_obstacle(O) || O <- Obstacles].

load_obstacle({Obstacle, Locations}) ->
  [ets:insert(obstacles, {L, O}) || L <- Locations, O <- [Obstacle]].

validate_move(Move, Grid) ->
  case validate_positive_numbers(Move, Grid) of
    ok -> ok;
    {error, Reason} -> {error, Reason}
  end.

validate_positive_numbers({X,Y} = Move, Grid) ->
  case (X >= 0) and (Y >= 0) of
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
  case ets:match_object(animals, {'$0', Location}) of
    [] -> validate_has_no_obstacle(Location);
    [_Animal] -> {error, location_has_animal}
  end.

validate_has_no_obstacle(Location) ->
  case ets:match_object(obstacles, {Location, '$1'}) of
    [] -> ok;
    [_Obstacle] -> {error, location_has_obstacle}
  end.

make_move(AnimalName, Move) ->
  case ets:lookup(animals, AnimalName) of
    [] ->
      io:format("~p placed into ~p~n", [AnimalName, Move]),
      ets:insert(animals, {AnimalName, Move});
    [{AnimalName, _Location}] ->
      io:format("~p moved into ~p~n", [AnimalName, Move]),
      ets:update_element(animals, AnimalName, {2, Move})
  end.
