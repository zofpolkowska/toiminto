-module(ghci).

-export([init/0]).

init() ->
    A =io:read("Prelude>"),
    io:format("~p~n",[A]).
