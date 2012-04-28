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
                  path}).

-record(file, {path, last_mod}).

start_link(ConfName, Path) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfName, Path] , []).

init([ConfName, Path]) ->
    Files = find_files(Path),
    Tc = erlang:start_timer(scan_delay(), self(), scan),
    InitState = #watcher{tc=Tc,
                         files=Files,
                         conf=ConfName,
                         path=Path},
    {ok, InitState}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event, State) ->
    {noreply, State}.


handle_info({timeout, Tc, scan}, State=#watcher{tc=Tc, files=OldFiles,
                                                conf=Conf, path=Path}) ->
    NewFiles = find_files(Path),
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

find_files(Path) ->
    case filelib:is_file(Path) of
        true ->
            [file_info(Path)];
        false ->
            IniFiles = econfig_util:find_ini_files(Path),
            lists:map(fun file_info/1, IniFiles)

    end.

scan_delay() ->
    case application:get_env(econfig, delay) of
        undefined -> 5000;
        {ok, Delay} -> Delay
    end.

file_info(Path) ->
    LastMod = filelib:last_modified(Path),
    #file{path=Path, last_mod=LastMod}.
