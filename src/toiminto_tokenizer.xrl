Definitions.

Dig = [0-9]
LowerCase = [a-z]
UpperCase = [A-Z]
F = [a-z][a-zA-Z]*
M = [A-Z][a-zA-Z]*


Rules.
%keywords
module       :        {token, {mods, TokenLine, list_to_atom(TokenChars)}}. 
where        :        {token, {where, TokenLine, list_to_atom(TokenChars)}}.
otherwise    :        {token, {otherwise, TokenLine, list_to_atom(TokenChars)}}.

({Dig}+) : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

%expr
\+ : {token, {'+', TokenLine}}.
\- : {token, {'-', TokenLine}}.
\* : {token, {'*', TokenLine}}.
\/ : {token, {'/', TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.

%comparators
\< : {token, {'<', TokenLine}}.

%function(arg) name --> functions f (g (h a)) == f g h a
{F}   : {token,{name,TokenLine,list_to_atom(TokenChars)}}.
%module name
{M}   : {token,{module,TokenLine,list_to_atom(TokenChars)}}.

%special characters
\= : {token, {'=', TokenLine}}.




(.|\n) : skip_token.

Erlang code.

-export([keyword/1]).

keyword('module') ->
    true;
keyword('where') ->
    true;
keyword('otherwise') ->
    true.


unwrap({_,_,V}) -> V.
