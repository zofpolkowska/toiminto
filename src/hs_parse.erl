-module(hs_parse).

-export([eval/1]).

eval({_Name, {'=',_}, Expr}) ->
    eval(Expr);

eval({ Expr1, {'+', _}, Expr2}) ->
    eval(Expr1) + eval(Expr2);

eval({ Expr1, {'-', _}, Expr2}) ->
    eval(Expr1) - eval(Expr2);

eval({ Expr1, {'*', _}, Expr2}) ->
    eval(Expr1) * eval(Expr2);

eval({Expr1, {'/', _}, Expr2}) ->
    eval(Expr1) / eval(Expr2);

eval({integer, _, Expr}) ->
    Expr.
