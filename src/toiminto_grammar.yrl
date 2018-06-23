Nonterminals
root functions function fn value expression term block.

Terminals
integer name module mods where
'(' ')' '+' '-' '*' '/' '='.

Rootsymbol root.

root -> mods module where functions    : {'$2','$4'}.
root -> functions                       : '$1'.

functions -> function                   : '$1'.
functions -> function functions         : {'$1', '$2'}.

function -> fn '=' value              : {'$2', '$1', '$3'}.

fn -> name                            : '$1'.

value -> expression                      : '$1'.

expression -> term                      : '$1'.
expression -> expression '+' term       : {'$2', '$1', '$3'}.
expression -> expression '-' term       : {'$2', '$1', '$3'}.

term -> block                           : '$1'.
term -> term '*' block                  : {'$2', '$1', '$3'}.
term -> term '/' block                  : {'$2', '$1', '$3'}.

block -> '(' expression ')'             : '$2'.
block -> integer                        : '$1'.
