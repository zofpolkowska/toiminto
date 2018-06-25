-module(hs_file).
-include("../include/keywords.hrl").
-export([erl/1, translate/1, parse/1]).

% --- API ------------------------------------------------------------------------------------------
erl(File) ->
    try parse(File) of
        Tokens ->
            Content = translate(Tokens),
            Base = filename:basename(File, ".hs"),
            Erl = lists:concat([Base, ".erl"]),
            file:write_file(Erl, Content)
    catch
        error:Error ->
            {Type, {Class, {Line, _, [Description, A]}}} = Error,
            Msg = io:format("~s, line ~p: ~s\n\t\t~s ~s",[Class,Line,Type,Description,A]),
            Msg;
        exit:Exit ->
            Exit;
        throw:Throw ->
            Throw
    end.

%function
translate({{function,Function, {'=', L}, Expr}, More}) ->
    F = translate({function,Function, {'=', L}, Expr}),
    Rest = translate(More),
    <<F/binary, Rest/binary>>;

translate({function,{name, Name},{'=',_}, Expr}) ->
    A = atom_to_binary(Name, utf8),
    Body = translate(Expr),
    <<A/binary,"() ->\n\t", Body/binary, ".\n\n">>;

translate({function, {{name, Name}, More},{'=',_}, Expr}) ->
    Args = translate(More),
    A = atom_to_binary(Name, utf8),
    Body = translate(Expr),
    <<A/binary, "(", Args/binary ,") ->\n\t", Body/binary, ".\n\n">>;
    

translate({{name, Name}, More}) ->
    A =translate({name, Name}),
    B = translate(More),
    <<A/binary, ",", B/binary>>;

translate({name, Name}) ->
    atom_to_binary(Name, utf8);
%module
translate({{module, 1, Module}, Functions}) ->
    M = translate({module, 1, Module}),
    Count = tuple_size(Functions),
    F = translate(Functions),
    <<M/binary, "\n", ?EXPORT/binary, "\n", F/binary>>;

translate({module, 1, Module}) ->
    ?M(Module);

translate([]) ->
    <<"[]">>;

translate({list, Head, Tail}) ->
    H = translate(Head),
    T = translate(Tail),
    <<"[", H/binary , "|", T/binary, "]">>;

translate({'(',Expr, ')'}) ->
    E = translate(Expr),
    <<"(", E/binary, ")">>;

translate({ Expr1, {'+', _}, Expr2}) ->
    E1 = translate(Expr1),
    E2 = translate(Expr2),
    <<E1/binary, " + ", E2/binary>>;

translate({ Expr1, {'-', _}, Expr2}) ->
    E1 = translate(Expr1),
    E2 = translate(Expr2),
    <<E1/binary, " - ", E2/binary>>;

translate({ Expr1, {'*', _}, Expr2}) ->
    E1 = translate(Expr1),
    E2 = translate(Expr2),
    <<E1/binary, " * ", E2/binary>>;

translate({ Expr1, {'/', _}, Expr2}) ->
    E1 = translate(Expr1),
    E2 = translate(Expr2),
    <<E1/binary, " / ", E2/binary>>;

translate({Expr1, '++', Expr2}) ->
    E1 = translate(Expr1),
    E2 = translate(Expr2),
    <<E1/binary, " ++ ", E2/binary>>;

translate({integer, _, Expr}) ->
    integer_to_binary(Expr);
translate({name, _, Expr}) ->
    atom_to_binary(Expr, utf8);
translate({charlist, _, Expr}) ->
    A = atom_to_binary(Expr, utf8),
    <<"\"", A/binary, "\"">>.


% --- Internal -------------------------------------------------------------------------------------
parse(File) ->
    try file:read_file(File) of
        {ok, Binary} ->
            parse(binary, Binary)
    catch
        _:_ ->
            handle_error
    end.

parse(binary, Binary) ->
    L = binary_to_list(Binary),
    {ok, S, _} = hs_tokenizer:string(L),
    {ok, P} = hs_grammar:parse(S),
    P.
 
unwrap({_,_,V}) ->   
    V.





