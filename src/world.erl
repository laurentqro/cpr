-module(world).

-export([start_link/1, get/1, move/2, delete/1]).
-export([init/1, loop/0]).

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
    ?MODULE ! {move, AnimalName, Move},
    ok.

delete(AnimalName) ->
    ?MODULE ! {delete, AnimalName},
    ok.

%%% Server functions
init(State) ->
    {_GridDimension, _Teleporters, _Obstacles} = State,
    ets:new(animals, [set, named_table]),
    loop().

loop() ->
    receive
        {move, AnimalName, Move} ->
            Result = ets:lookup(animals, AnimalName),
            case Result of
                [] ->
                    ets:insert(animals, {AnimalName, Move}),
                    AnimalName ! {ok, Move},
                    loop();
                [{AnimalName, _Location}] ->
                    ets:update_element(animals, AnimalName, {2, Move}),
                    AnimalName ! {ok, Move},
                    loop()
            end;
        {From, get, AnimalName} ->
            From ! {ok, ets:lookup(animals, AnimalName)},
            loop();
        {delete, AnimalName} ->
            ets:delete(animals, AnimalName),
            loop()
    end.
