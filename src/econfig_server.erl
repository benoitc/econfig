%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

%% @hidden

-module(econfig_server).
-behaviour(gen_server).

-export([register_config/2, register_config/3,
         open_config/2, open_config/3,
         unregister_config/1,
         subscribe/1, unsubscribe/1,
         reload/1, reload/2,
         start_autoreload/1, stop_autoreload/1,
         all/1, sections/1, prefix/2,
         cfg2list/1, cfg2list/2,
         get_value/2, get_value/3, get_value/4,
         set_value/3, set_value/4, set_value/5,
         delete_value/2, delete_value/3, delete_value/4]).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {confs = dict:new()}).
-record(config, {write_file=nil,
                 pid=nil,
                 change_fun,
                 options,
                 inifiles}).

-define(TAB, econfig).

start_link() ->
    _ = init_tabs(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_config(term(), econfig:inifiles()) -> ok | {error,
                                                           any()}.
%% @doc register inifiles
register_config(ConfigName, IniFiles) ->
    register_config(ConfigName, IniFiles, []).

register_config(ConfigName, IniFiles, Options) ->
    gen_server:call(?MODULE, {register_conf, {ConfigName, IniFiles, Options}}, infinity).

%% @doc unregister a conf
unregister_config(ConfigName) ->
    gen_server:call(?MODULE, {unregister_conf, ConfigName}).

%% @doc open or create an ini file an register it
open_config(ConfigName, IniFile) ->
    open_config(ConfigName, IniFile, []).

open_config(ConfigName, IniFile, Options) ->
    IniFileName = econfig_util:abs_pathname(IniFile),
    case filelib:is_file(IniFileName) of
        true ->
            register_config(ConfigName, [IniFile], Options);
        false ->
            case file:open(IniFileName, [write]) of
                {ok, Fd} ->
                    file:close(Fd),
                    register_config(ConfigName, [IniFile], Options);
                Error ->
                    Error
            end
    end.
subscribe(ConfigName) ->
    Key = {sub, ConfigName, self()},
    case ets:insert_new(?TAB, {Key, self()}) of
        false -> ok;
        true ->
            ets:insert(?TAB, {{self(), ConfigName}, Key}),
            %% maybe monitor the process
            case ets:insert_new(?TAB, {self(), m}) of
                false -> ok;
                true ->
                    gen_server:cast(?MODULE, {monitor_sub, self()})
            end
    end.

%% @doc Remove subscribtion created using `subscribe(ConfigName)'
unsubscribe(ConfigName) ->
    gen_server:call(?MODULE, {unsub, ConfigName}).


%% @doc reload the configuration
reload(ConfigName) ->
    reload(ConfigName, nil).

%% @doc reload the configuration
reload(ConfigName, IniFiles) ->
    gen_server:call(?MODULE, {reload, {ConfigName, IniFiles}}, infinity).

start_autoreload(ConfigName) ->
    gen_server:call(?MODULE, {start_autoreload, ConfigName}).

stop_autoreload(ConfigName) ->
    gen_server:call(?MODULE, {stop_autoreload, ConfigName}).


%% @doc get all values of a configuration
all(ConfigName) ->
    Matches = ets:match(?TAB, {{conf_key(ConfigName), '$1', '$2'}, '$3'}),
    [{Section, Key, Value} || [Section, Key, Value] <- Matches].

%% @doc get all sections of a configuration
sections(ConfigName) ->
    Matches = ets:match(?TAB, {{conf_key(ConfigName), '$1', '_'}, '_'}),
    lists:umerge(Matches).


%% @doc get all sections starting by Prefix
prefix(ConfigName, Prefix) ->
    Matches = ets:match(?TAB, {{conf_key(ConfigName), '$1', '_'}, '_'}),
    Found = lists:foldl(fun([Match], Acc) ->
                            case lists:prefix(Prefix, Match) of
                              true -> [Match | Acc];
                              false -> Acc
                            end
            end, [], Matches),
    lists:reverse(Found).

%% @doc retrive config as a proplist
cfg2list(ConfigName) ->
    Matches = ets:match(?TAB, {{conf_key(ConfigName), '$1', '$2'}, '$3'}),
    lists:foldl(fun([Section, Key, Value], Props) ->
                case lists:keyfind(Section, 1, Props) of
                    false ->
                        [{Section, [{Key, Value}]} | Props];
                    {Section, KVs} ->
                        KVs1 = lists:keymerge(1, KVs, [{Key, Value}]),
                        lists:keyreplace(Section, 1, Props, {Section, KVs1})
                end
        end, [], Matches).

%% @doc retrieve config as a proplist
cfg2list(ConfigName, GroupKey) ->
    Matches = ets:match(?TAB, {{conf_key(ConfigName), '$1', '$2'}, '$3'}),
    lists:foldl(fun([Section, Key, Value], Props) ->
                case re:split(Section, GroupKey, [{return,list}]) of
                    [Section] ->
                        case lists:keyfind(Section, 1, Props) of
                            false ->
                                [{Section, [{Key, Value}]} | Props];
                            {Section, KVs} ->
                                KVs1 = lists:keymerge(1, KVs, [{Key, Value}]),
                                lists:keyreplace(Section, 1, Props, {Section, KVs1})
                        end;
                    [Section1, SSection] ->
                        case lists:keyfind(Section1, 1, Props) of
                            false ->
                                [{Section1, [{SSection, [{Key, Value}]}]}
                                 | Props];
                            {Section1, KVs} ->
                                KVs1 = case lists:keyfind(SSection, 1, KVs) of
                                    false ->
                                        [{SSection, [{Key, Value}]} | KVs];
                                    {SSection, SKVs} ->
                                        SKVs1 = lists:keymerge(1, SKVs,
                                                               [{Key, Value}]),
                                        lists:keyreplace(SSection, 1,
                                                         KVs, {SSection,
                                                               SKVs1})
                                end,
                                lists:keyreplace(Section1, 1, Props,
                                                 {Section1, KVs1})
                        end
                end
        end, [], Matches).

%% @doc get values of a section
get_value(ConfigName, Section0) ->
    Section = econfig_util:to_list(Section0),
    Matches = ets:match(?TAB, {{conf_key(ConfigName), Section, '$1'}, '$2'}),
    [{Key, Value} || [Key, Value] <- Matches].

%% @doc get value for a key in a section
get_value(ConfigName, Section, Key) ->
    get_value(ConfigName, Section, Key, undefined).

get_value(ConfigName, Section0, Key0, Default) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),

    case ets:lookup(?TAB, {conf_key(ConfigName), Section, Key}) of
        [] -> Default;
        [{_, Match}] -> Match
    end.

%% @doc set a section
set_value(ConfigName, Section, List) ->
    set_value(ConfigName, Section, List, true).

set_value(ConfigName, Section0, List, Persist)
        when is_boolean(Persist) ->
    Section = econfig_util:to_list(Section0),
    List1 = [{econfig_util:to_list(K), econfig_util:to_list(V)}
              || {K, V} <- List],
    gen_server:call(?MODULE, {mset, {ConfigName, Section, List1,
                                     Persist}}, infinity).

set_value(ConfigName, Section0, Key0, Value0, Persist) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),
    Value = econfig_util:to_list(Value0),
    gen_server:call(?MODULE, {set, {ConfigName, Section, Key, Value, Persist}}, infinity).

delete_value(ConfigName, Section) ->
    delete_value(ConfigName, Section, true).

delete_value(ConfigName, Section0, Persist) when is_boolean(Persist) ->
    Section = econfig_util:to_list(Section0),
    gen_server:call(?MODULE, {mdel, {ConfigName, Section, Persist}},
                    infinity);

%% @doc delete a value
delete_value(ConfigName, Section, Key) ->
    delete_value(ConfigName, Section, Key, true).

delete_value(ConfigName, Section0, Key0, Persist) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),

    gen_server:call(?MODULE, {del, {ConfigName, Section, Key,
                                    Persist}}, infinity).


init_tabs() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?TAB, [ordered_set, public, named_table,
                           {read_concurrency, true},
                           {write_concurrency, true}]);
        _ ->
            true
    end.

%% -----------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------

init(_) ->
    process_flag(trap_exit, true),
    InitialState = initialize_app_confs(),
    {ok, InitialState}.

handle_call({register_conf, {ConfName, IniFiles, Options}}, _From, #state{confs=Confs}=State) ->
    {Resp, NewState} =
    try
        WriteFile = parse_inis(ConfName, IniFiles),
        {ok, Pid} = case proplists:get_value(autoreload, Options) of
                        true ->
                            Res = econfig_watcher_sup:start_watcher(ConfName, IniFiles),
                            Res;
                        Delay when is_integer(Delay) ->
                            econfig_watcher_sup:start_watcher(ConfName, IniFiles, Delay);
                        _ ->
                            {ok, nil}
                    end,

        ChangeFun = proplists:get_value(change_fun, Options, fun(_Change) -> ok end),
        ok = check_fun(ChangeFun),
        Confs1 = dict:store(ConfName, #config{write_file=WriteFile,
                                              pid=Pid,
                                              change_fun=ChangeFun,
                                              options=Options,
                                              inifiles=IniFiles},
                            Confs),
        State2 = State#state{confs=Confs1},
        notify_change(State2, ConfName, registered),
        {ok, State2#state{confs=Confs1}}
    catch _Tag:Error ->
              {{error, Error}, State}
    end,
    {reply, Resp, NewState};

handle_call({unregister_conf, ConfName}, _From, #state{confs=Confs}=State) ->
    true = ets:match_delete(?TAB, {{conf_key(ConfName), '_', '_'}, '_'}),
    case dict:find(ConfName, Confs) of
        {ok, #config{pid=Pid}} when is_pid(Pid) ->
            supervisor:terminate_child(econfig_watcher_sup, Pid),
            notify_change(State, ConfName, unregistered);
        _ ->
            ok
    end,
    {reply, ok, State#state{confs=dict:erase(ConfName, Confs)}};

handle_call({reload, {ConfName, IniFiles0}}, _From,
            #state{confs=Confs}=State) ->

    case dict:find(ConfName, Confs) of
        {ok, #config{inifiles=IniFiles1, options=Options}=Conf} ->

            true = ets:match_delete(?TAB, {{conf_key(ConfName), '_', '_'}, '_'}),
            IniFiles = case IniFiles0 of
                nil -> IniFiles1;
                _ -> IniFiles0
            end,

            %% do the reload
            WriteFile = parse_inis(ConfName, IniFiles),
            Confs1 = dict:store(ConfName, Conf#config{write_file=WriteFile,
                                                      options=Options,
                                                      inifiles=IniFiles},
                                Confs),
            State2 = State#state{confs=Confs1},
            notify_change(State2, ConfName, reload),
            {reply, ok, State2};
        _ ->
            {reply, ok, State}
    end;

handle_call({start_autoreload, ConfName}, _From, #state{confs=Confs}=State) ->
    case dict:find(ConfName, Confs) of
        {ok, #config{inifiles=IniFiles}=Config} ->
            {ok, Pid} = econfig_watcher_sup:start_watcher(ConfName, IniFiles),
            Config1 = Config#config{pid=Pid},
            {reply, ok, State#state{confs=dict:store(ConfName, Config1, Confs)}};
        _  ->
            {reply, ok, State}
    end;

handle_call({stop_autoreload, ConfName}, _From, #state{confs=Confs}=State) ->
    case dict:find(ConfName, Confs) of
        {ok, #config{pid=Pid}=Config} when is_pid(Pid) ->
            supervisor:terminate_child(econfig_watcher_sup, Pid),
            Config1 = Config#config{pid=nil},
            {reply, ok, State#state{confs=dict:store(ConfName, Config1,
                                                     Confs)}};
        _  ->
            {reply, ok, State}
    end;


handle_call({set, {ConfName, Section, Key, Value, Persist}}, _From,
            #state{confs=Confs}=State) ->

    Result = case {Persist, dict:find(ConfName, Confs)} of
        {true, {ok, #config{write_file=FileName}=Conf}} when FileName /= nil->
            maybe_pause(Conf, fun() ->
                econfig_file_writer:save_to_file({Section, [{Key, Value}]}, FileName)
            end);
        _ ->
            ok
    end,
    case Result of
        ok ->
            Value1 = econfig_util:trim_whitespace(Value),
            case Value1 of
                [] ->
                    true = ets:delete(?TAB, {conf_key(ConfName), Section, Key});
                _ ->
                    true = ets:insert(?TAB, {{conf_key(ConfName), Section, Key}, Value1})
            end,
            notify_change(State, ConfName, {set, {Section, Key}}),
            {reply, ok, State};
        _Error ->
            {reply, Result, State}
    end;
handle_call({mset, {ConfName, Section, List, Persist}}, _From,
            #state{confs=Confs}=State) ->
    Result = case {Persist, dict:find(ConfName, Confs)} of
                 {true, {ok, #config{write_file=FileName}=Conf}} when FileName /= nil->
                     maybe_pause(Conf, fun() ->
                                               econfig_file_writer:save_to_file({Section, List}, FileName)
                                       end);
                 _ ->
                     ok
             end,
    case Result of
        ok ->
            lists:foreach(fun({Key,Value}) ->
                                  Value1 = econfig_util:trim_whitespace(Value),
                                  if
                                      Value1 /= [] ->
                                          true = ets:insert(?TAB, {{conf_key(ConfName), Section, Key}, Value1}),
                                          notify_change(State, ConfName, {set, {Section, Key}});
                                      true ->
                                          true = ets:delete(?TAB, {conf_key(ConfName), Section, Key}),
                                          notify_change(State, ConfName, {delete, {Section, Key}})
                                  end
                          end, List),
            {reply, ok, State};
        _Error ->
            {reply, Result, State}
    end;

handle_call({del, {ConfName, Section, Key, Persist}}, _From,
            #state{confs=Confs}=State) ->

    true = ets:delete(?TAB, {conf_key(ConfName), Section, Key}),

    case {Persist, dict:find(ConfName, Confs)} of
        {true, {ok, #config{write_file=FileName}=Conf}} when FileName /= nil->
            maybe_pause(Conf, fun() ->
                                      econfig_file_writer:save_to_file({Section, [{Key, ""}]}, FileName)
                              end);
        _ ->
            ok
    end,
    notify_change(State, ConfName, {delete, {Section, Key}}),
    {reply, ok, State};
handle_call({mdel, {ConfName, Section, Persist}}, _From,
            #state{confs=Confs}=State) ->
    Matches = ets:match(?TAB, {{conf_key(ConfName), Section, '$1'}, '$2'}),
    ToDelete = lists:foldl(fun([Key, _Val], Acc) ->
                                   true = ets:delete(?TAB, {conf_key(ConfName), Section, Key}),
                                   notify_change(State, ConfName, {delete, {Section, Key}}),
                                   [{Key, ""} | Acc]
                           end, [], Matches),

    case {Persist, dict:find(ConfName, Confs)} of
        {true, {ok, #config{write_file=FileName}=Conf}} when FileName /= nil->
            maybe_pause(Conf, fun() ->
                                      econfig_file_writer:save_to_file({Section, ToDelete}, FileName)
                              end);
        _ ->
            ok
    end,
    {reply, ok, State};

handle_call({unsub, ConfName}, {Pid, _}, State) ->
    Key = {sub, ConfName, Pid},
    case ets:lookup(?TAB, Key) of
        [{Key, Pid}] ->
            _ = ets:delete(?TAB, Key),
            _ = ets:delete(?TAB, {Pid, ConfName});
        [] -> ok
    end,
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({monitor_sub, Pid}, State) ->
    erlang:monitor(process, Pid),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', _MRef, process, Pid, _}, State) ->
    _ = process_is_down(Pid),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason , _State) ->
    ok.


conf_key(Name) -> {c, Name}.

%% -----------------------------------------------
%% internal functions
%% -----------------------------------------------
%%

maybe_pause(#config{pid=Pid}, Fun) when is_pid(Pid) ->
    econfig_watcher:pause(Pid),
    Fun(),
    econfig_watcher:restart(Pid);
maybe_pause(_, Fun) ->
    Fun().

notify_change(State, ConfigName, Event) ->
    Msg = {config_updated, ConfigName, Event},
    run_change_fun(State, ConfigName, Msg),
    send(ConfigName, Msg).

send(ConfigName, Msg) ->
    Subs = ets:select(?TAB, [{{{sub, ConfigName, '_'}, '$1'}, [], ['$1']}]),
    lists:foreach(fun(Pid) ->
        catch Pid ! Msg
    end, Subs).

run_change_fun(State, ConfigName, Msg) ->
    {ok, #config{change_fun=ChangeFun}} = dict:find(ConfigName, State#state.confs),
    Ret = (catch apply_change_fun(ChangeFun, Msg)),
    case Ret of
        {'EXIT', Reason} ->
            error_logger:warning_msg("~p~n error running change hook: ~p~n", [?MODULE, Reason]),
            ok;
        _ ->
            ok
    end.

apply_change_fun(none, _Msg) -> ok;
apply_change_fun({M, F}, Msg) -> apply(M, F, [Msg]);
apply_change_fun(F, Msg) -> F(Msg).


initialize_app_confs() ->
    case application:get_env(econfig, confs) of
        undefined ->
            #state{};
        {ok, Confs} ->
            initialize_app_confs1(Confs, #state{})
    end.

initialize_app_confs1([], State) ->
    State;
initialize_app_confs1([{ConfName, IniFiles} | Rest], State) ->
    initialize_app_confs1([{ConfName, IniFiles, []} | Rest], State);
initialize_app_confs1([{ConfName, IniFiles, Options} | Rest],
                      #state{confs=Confs}=State) ->
    WriteFile = parse_inis(ConfName, IniFiles),
    {ok, Pid} = case proplists:get_value(autoreload, Options) of
        true ->
            econfig_watcher_sup:start_watcher(ConfName, IniFiles);
        _ ->
            {ok, nil}
    end,
    Confs1 = dict:store(ConfName, #config{write_file=WriteFile,
                                          pid=Pid,
                                          options=Options,
                                          inifiles=IniFiles},
                        Confs),

    initialize_app_confs1(Rest, State#state{confs=Confs1}).



parse_inis(ConfName, IniFiles0) ->
    IniFiles = econfig_util:find_files(IniFiles0),
    lists:map(fun(IniFile) ->
                {ok, ParsedIniValues, DelKeys} = parse_ini_file(ConfName, IniFile),
                ets:insert(?TAB, ParsedIniValues),
                lists:foreach(fun(Key) -> ets:delete(?TAB, Key) end, DelKeys)
        end, IniFiles),
    WriteFile = lists:last(IniFiles),
    WriteFile.


parse_ini_file(ConfName, IniFile) ->
    IniFilename = econfig_util:abs_pathname(IniFile),
    IniBin =
    case file:read_file(IniFilename) of
        {ok, IniBin0} ->
            IniBin0;
        {error, eacces} ->
            throw({file_permission_error, IniFile});
        {error, enoent} ->
            Fmt = "Couldn't find server configuration file ~s.",
            Msg = list_to_binary(io_lib:format(Fmt, [IniFilename])),
            throw({startup_error, Msg})
    end,

    Lines = re:split(IniBin, "\r\n|\n|\r|\032", [{return, list}]),
    {_, ParsedIniValues, DeleteIniKeys} =
    lists:foldl(fun(Line, {AccSectionName, AccValues, AccDeletes}) ->
            case string:strip(Line) of
            "[" ++ Rest ->
                case re:split(Rest, "\\]", [{return, list}]) of
                [NewSectionName, ""] ->
                    {NewSectionName, AccValues, AccDeletes};
                _Else -> % end bracket not at end, ignore this line
                    {AccSectionName, AccValues, AccDeletes}
                end;
            ";" ++ _Comment ->
                {AccSectionName, AccValues, AccDeletes};
            Line2 ->
                case re:split(Line2, "\s*=\s*", [{return, list}]) of
                [Value] ->
                    MultiLineValuePart = case re:run(Line, "^ \\S", []) of
                    {match, _} ->
                        true;
                    _ ->
                        false
                    end,
                    case {MultiLineValuePart, AccValues} of
                    {true, [{{_, ValueName}, PrevValue} | AccValuesRest]} ->
                        % remove comment
                        case re:split(Value, "\s*;|\t;", [{return, list}]) of
                        [[]] ->
                            % empty line
                            {AccSectionName, AccValues, AccDeletes};
                        [LineValue | _Rest] ->
                            E = {{AccSectionName, ValueName},
                                 PrevValue ++ " " ++
                                 econfig_util:trim_whitespace(LineValue)},
                            {AccSectionName, [E | AccValuesRest], AccDeletes}
                        end;
                    _ ->
                        {AccSectionName, AccValues, AccDeletes}
                    end;
                [""|_LineValues] -> % line begins with "=", ignore
                    {AccSectionName, AccValues, AccDeletes};
                [ValueName|LineValues] -> % yeehaw, got a line!
                    %% replace all tabs by an empty value.
                    ValueName1 = econfig_util:trim_whitespace(ValueName),
                    RemainingLine = econfig_util:implode(LineValues, "="),
                    % removes comments
                    case re:split(RemainingLine, "\s*;|\t;", [{return, list}]) of
                        [[]] ->
                            % empty line means delete this key
                            AccDeletes1 = [{conf_key(ConfName), AccSectionName, ValueName1}
                                           | AccDeletes],
                            {AccSectionName, AccValues, AccDeletes1};
                        [LineValue | _Rest] ->
                            {AccSectionName,
                             [{{conf_key(ConfName), AccSectionName, ValueName1},
                               econfig_util:trim_whitespace(LineValue)}
                              | AccValues], AccDeletes}
                        end
                end
            end
        end, {"", [], []}, Lines),
    {ok, ParsedIniValues, DeleteIniKeys}.

process_is_down(Pid) when is_pid(Pid) ->
    case ets:member(?TAB, Pid) of
        false ->
            ok;
        true ->
            Subs = ets:select(?TAB, [{{{Pid, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]),
            lists:foreach(fun({ConfName, SubKey}) ->
                    ets:delete(?TAB, {Pid, ConfName}),
                    ets:delete(?TAB, SubKey)
                end, Subs),
            ets:delete(?TAB, Pid),
            ok
    end.

check_fun(none) ->
    ok;
check_fun(Fun) when is_function(Fun) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 1} -> ok;
        _ -> {error, badarity}
    end;
check_fun({Mod, Fun}) ->
    _ = code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, Fun, 1) of
        true -> ok;
        false -> {error, function_not_exported}
    end.
