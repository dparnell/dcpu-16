-module(dcpu16_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Server = {dcpu16_server, {dcpu16_server, start_link, []},
	      permanent, 2000, worker, [dcpu16_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 3, 1},
    {ok, {RestartStrategy, Children}}.
