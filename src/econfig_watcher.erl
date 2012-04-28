-module(econfig_watcher).
-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(watcher, {tc,
                  files = [],
                  conf,
                  paths}).

-record(file, {path, last_mod}).

start_link(ConfName, Paths) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfName, Paths] , []).

init([ConfName, Paths]) ->
    Files = find_files(Paths),
    Tc = erlang:start_timer(scan_delay(), self(), scan),
    InitState = #watcher{tc=Tc,
                         files=Files,
                         conf=ConfName,
                         paths=Paths},
    {ok, InitState}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event, State) ->
    {noreply, State}.


handle_info({timeout, Tc, scan}, State=#watcher{tc=Tc, files=OldFiles,
                                                conf=Conf, paths=Paths}) ->
    NewFiles = find_files(Paths),
    case OldFiles -- NewFiles of
        [] ->
            ok;
        _ ->
            IniFiles = lists:map(fun(#file{path=P}) -> P end, NewFiles),
            io:format("reload ~p~n", [IniFiles]),
            econfig_server:reload(Conf, IniFiles)
    end,
    {noreply, State#watcher{tc=erlang:start_timer(scan_delay(), self(),
                                             scan),
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

find_files(Paths) ->
    find_files(Paths, []).

find_files([], Acc) ->
    Acc;
find_files([Path | Rest], Acc) ->
    case filelib:is_file(Path) of
        true ->
            Acc1 = Acc ++  [file_info(Path)],
            find_files(Rest, Acc1);
        false ->
            IniFiles = econfig_util:find_ini_files(Path),
            Acc1 = Acc ++ lists:map(fun file_info/1, IniFiles),
            find_files(Rest, Acc1)
    end.

scan_delay() ->
    case application:get_env(econfig, delay) of
        undefined -> 5000;
        {ok, Delay} -> Delay
    end.

file_info(Path) ->
    LastMod = filelib:last_modified(Path),
    #file{path=Path, last_mod=LastMod}.
