-module(animal).
-include("animal.hrl").
-behaviour(gen_server).
-compile(export_all).

%%% Client API
start_link(Name, Location) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Location], []).

move(Name, Direction) ->
  gen_server:cast(Name, {move, Direction}).

sleep(Name, Time) ->
  gen_server:cast(Name, {sleep, Time}).

stop(Name) ->
  gen_server:cast(Name, stop).

%%% Server functions
init([Name, Location]) ->
  process_flag(trap_exit, true),
  init(world:get(Name), [Name, Location]).

init([], [Name, Location] ) ->
  case world:move(Name, Location) of
    [NewState] -> {ok, NewState};
    [] -> ignore
  end;

init([State], [_Name, _Location] ) ->
  {ok, State}.

handle_cast({move, Direction}, #animal{name=Name, location={X,Y}}) ->
  Move = case Direction of
    up    -> {X, Y + 1};
    down  -> {X, Y - 1};
    right -> {X + 1, Y};
    left  -> {X - 1, Y}
  end,
  [NewState] = world:move(Name, Move),
  {noreply, NewState};

handle_cast({sleep, Time}, State) ->
  io:format("Sleeping ~p milliseconds ...\n", [Time]),
  timer:sleep(Time),
  io:format("Awake!~n"),
  {noreply, State};

handle_cast(stop, State) ->
  {stop, normal, State}.

terminate(shutdown, #animal{name=Name, location=_}) ->
  io:format("~p shutting down ...", [Name]),
  world:delete(Name);

terminate(normal, #animal{name=Name, location=_}) ->
  io:format("~p stopping ...", [Name]),
  world:delete(Name);

terminate(Reason, #animal{name=Name, location=_}) ->
  io:format("~p terminating: ~p.", [Name, Reason]).
