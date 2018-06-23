-module(toiminto_app).

-behaviour(application).

% Callbacks
-export([start/2, stop/1]).

%--- Callbacks -------------------------------------------------------------

start(_StartType, _StartArgs) ->
   toiminto_sup:start_link().


stop(_State) ->
    ok.
