toiminto
=====

Smarll part of Haskell to Erlang compiler

example
==
Haskell file:

    module Baby where
    h x y = (x + 2) * x / y
    f = "a" ++ "b"
    g = "a" : "b" : ["s"]

Test:
    $ rebar3 shell
    >ghci:l(baby).
    baby.erl:3: Warning: export_all flag enabled - all functions will be exported
    module loaded: "baby.hs"
    ok
    2> baby:f().
    "ab"
    3> baby:g().
    ["a","b","s"]
    4> baby:h(1,3).
    
