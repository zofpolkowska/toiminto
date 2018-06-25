Nonterminals
root functions function fn single value expression term block lists list integers charlists.

Terminals
integer name module mods where
'(' ')' '+' '-' '*' '/' '=' '"' '\[' '\]' '\:'.

Rootsymbol root.

root -> mods module where functions    : {'$2','$4'}.
root -> functions                       : {functions,'$1'}.

functions -> function                   : '$1'.
functions -> function functions         : {'$1', '$2'}.

function -> fn '=' value                : {function,'$1', '$2', '$3'}.

fn -> name fn                         : {unwrap('$1'), '$2'}.
fn -> name                            : unwrap('$1').


value -> expression                      : '$1'.
value -> lists                            :'$1'.

value -> integers : '$1'.
value -> '\[' '\]' : {list, [], []}.
integers -> '\[' integer '\]' : {list, '$2', []}.
integers -> integer '\:' integers : {list, '$1', '$3'}.

value -> charlists : '$1'.
charlists -> '\[' lists '\]' : {list, '$2', []}.
charlists -> lists '\:' charlists : {list, '$1', '$3'}.

lists -> list :'$1'.
lists -> lists '+' '+' list : {'$1', '++', '$4'}.

list -> '\"' name '\"'                   : charlist('$2').
list -> '\"' module '\"'                   : charlist('$2').

expression -> term                      : '$1'.
expression -> expression '+' term       : {'$1', '$2', '$3'}.
expression -> expression '-' term       : {'$1', '$2', '$3'}.

term -> block                           : '$1'.
term -> term '*' block                  : {'$1', '$2', '$3'}.
term -> term '/' block                  : {'$1', '$2', '$3'}.

block -> '(' expression ')'             : {'(', '$2', ')'}.
block -> integer                        : '$1'.
block -> name                           : '$1'.
Erlang code.

unwrap({Id,_,V}) -> {Id, V}.


charlist({name, L, V}) -> {charlist, L, V};
charlist({module, L, V}) -> {charlist, L, V}. 
