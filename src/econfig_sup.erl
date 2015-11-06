%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

%%  @hidden

-module(econfig_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% main config server
    Server = {econfig_server,
              {econfig_server, start_link, []},
              permanent, 5000, worker, [econfig_server]},

    %% watchr supervisor spec
    WatcherSup = {econfig_watcher_sup,
                  {econfig_watcher_sup, start_link, []},
                  permanent, infinity, supervisor, [econfig_watcher_sup]},

    {ok, { {one_for_one, 5, 10}, [Server, WatcherSup]} }.

