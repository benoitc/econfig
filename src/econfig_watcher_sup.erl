-module(econfig_watcher_sup).
-behaviour(supervisor).

-export([start_link/0, stop/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_S) -> ok.

%% @private
init([]) ->
    {ok,
     {{simple_one_for_one, 10, 10},
      [{econfig_watcher,
        {econfig_watcher, start_link, []},
        permanent, 5000, worker, [econfig_watcher]}]}}.
