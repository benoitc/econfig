%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(econfig_watcher_sup).
-behaviour(supervisor).

-export([start_watcher/2, start_watcher/3]).
-export([start_link/0, stop/1]).

-export([init/1]).

start_watcher(ConfigName, Path) ->
    supervisor:start_child(?MODULE, [ConfigName, Path]).

start_watcher(ConfigName, Path, Delay) ->
    supervisor:start_child(?MODULE, [ConfigName, Path, Delay]).

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
