-module(toiminto_parse).

-export([eval/1]).

eval({{'=',_}, _Name, Expr}) ->
    eval(Expr);

eval({{'+', _}, Expr1, Expr2}) ->
    eval(Expr1) + eval(Expr2);

eval({{'-', _}, Expr1, Expr2}) ->
    eval(Expr1) - eval(Expr2);

eval({{'*', _}, Expr1, Expr2}) ->
    eval(Expr1) * eval(Expr2);

eval({{'/', _}, Expr1, Expr2}) ->
    eval(Expr1) / eval(Expr2);

eval({integer, _, Expr}) ->
    Expr.
