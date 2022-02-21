-module(world).

-export([start_link/1]).

start_link(FileName) ->
    file:consult(FileName),
    ok.