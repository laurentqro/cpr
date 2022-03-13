-module(animal).
-behaviour(gen_server).
-compile(export_all).

%%% Client API
start_link(Name, Location) ->
  gen_server:start_link({local, Name}, ?MODULE, {Name, Location}, []).

move(Name, Direction) ->
  gen_server:call(Name, {move, Direction}).

sleep(Name, Time) ->
  gen_server:cast(Name, {sleep, Time}).

stop(Name) ->
  gen_server:call(Name, {stop, normal}).

%%% Server functions
init({Name, Location}) ->
  world:move(Name, Location),
  io:format("~p: Moved into position ~p~n", [Name, Location]),
  {ok, {Name, Location}}.

handle_call({reply, Reply, _State}, _From, {Name, _Location}) ->
  {reply, Reply, {Name, Reply}};

handle_call({stop, Reason}, _From, {Name, Location}) ->
  {stop, Reason, {Name, Location}};

handle_call({move, Direction}, _From, {Name, {X,Y}}) ->
  NewLocation = case Direction of
    up    -> {X, Y + 1};
    down  -> {X, Y - 1};
    right -> {X + 1, Y};
    left  -> {X - 1, Y}
  end,
  world:move(Name, NewLocation),
  {reply, io:format("~p moved into ~p~n", [Name, NewLocation]), {Name, NewLocation}}.

handle_cast({sleep, Time}, {Name, Location}) ->
  io:format("Sleeping ~p milliseconds ...\n", [Time]),
  timer:sleep(Time),
  io:format("Awake!~n"),
  {noreply, {Name, Location}}.

terminate(_Reason, {Name, _Location}) ->
  world:delete(Name).
