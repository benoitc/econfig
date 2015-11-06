%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

%%  @hidden

-module(econfig_watcher).
-behaviour(gen_server).

-export([start_link/2, start_link/3,
         pause/1, restart/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(watcher, {tc,
                  files = [],
                  scan_delay,
                  conf,
                  paths}).

-record(file, {path, last_mod}).

start_link(ConfName, Paths) ->
    start_link(ConfName, Paths, scan_delay()).

start_link(ConfName, Paths, Delay) ->
    gen_server:start_link(?MODULE, [ConfName, Paths, Delay] , []).


pause(Pid) ->
    gen_server:call(Pid, pause).

restart(Pid) ->
    gen_server:call(Pid, restart).

init([ConfName, Paths, Delay]) ->
    Files = econfig_util:find_files(Paths, fun file_info/1),
    Tc = erlang:start_timer(Delay, self(), scan),
    InitState = #watcher{tc=Tc,
                         scan_delay=Delay,
                         files=Files,
                         conf=ConfName,
                         paths=Paths},
    {ok, InitState}.


handle_call(pause, _From, State=#watcher{tc=Tc}) when Tc /= nil ->
    erlang:cancel_timer(Tc),
    {reply, ok, State#watcher{tc=nil}};
handle_call(restart, _From, State=#watcher{tc=nil, scan_delay=Delay,
                                           paths=Paths}) ->
    Files = econfig_util:find_files(Paths, fun file_info/1),
    Tc = erlang:start_timer(Delay, self(), scan),
    {reply, ok, State#watcher{tc=Tc, files=Files}};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event, State) ->
    {noreply, State}.


handle_info({timeout, Tc, scan}, State=#watcher{tc=Tc, scan_delay=Delay,
                                                files=OldFiles, conf=Conf,
                                                paths=Paths}) ->
    NewFiles = econfig_util:find_files(Paths, fun file_info/1),
    case OldFiles -- NewFiles of
        [] ->
            ok;
        _ ->
            IniFiles = lists:map(fun(#file{path=P}) -> P end, NewFiles),
            econfig_server:reload(Conf, IniFiles)
    end,
    {noreply, State#watcher{tc=erlang:start_timer(Delay, self(), scan),
                            files=NewFiles}};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%% --
%% internal functions
%%

scan_delay() ->
    case application:get_env(econfig, scan_delay) of
        undefined -> 5000;
        {ok, Delay} -> Delay
    end.

file_info(Path) ->
    LastMod = filelib:last_modified(Path),
    #file{path=Path, last_mod=LastMod}.
