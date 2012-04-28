%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(econfig_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_app_deps(econfig),
    econfig_sup:start_link().

stop(_State) ->
    ok.


%% @spec start_app_deps(App :: atom()) -> ok
%% @doc Start depedent applications of App.
start_app_deps(App) ->
    {ok, DepApps} = application:get_key(App, applications),
    [ensure_started(A) || A <- DepApps],
    ok.

%% @spec ensure_started(Application :: atom()) -> ok
%% @doc Start the named application if not already started.
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok;
    Else ->
        error_logger:error_msg("Couldn't start ~p: ~p", [App, Else]),
        Else
    end.


