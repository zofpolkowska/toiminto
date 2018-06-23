-module(toiminto_sup).

-behaviour(supervisor).

% API
-export([start_link/0]).

% Callback
-export([init/1]).

-define(SERVER, ?MODULE).

%--- API -------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%--- Callbacks -------------------------------------------------------

init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.
