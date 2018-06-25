-module(ghci).

-export([l/1]).

l(Module) ->
    File = lists:concat([
                         atom_to_list(Module), ".hs"
                        ]),
    {ok, Erl} = hs_file:erl(File),
    code:load_file(
      list_to_atom(
        string:to_lower(
          filename:basename(Erl, ".erl")))),
    io:format("module loaded: ~p~n",[File]).
