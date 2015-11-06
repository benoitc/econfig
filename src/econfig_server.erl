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
                 options,
                 inifiles}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_config(term(), econfig:inifiles()) -> ok | {error,
                                                           any()}.
%% @doc register inifiles
register_config(ConfigName, IniFiles) ->
    register_config(ConfigName, IniFiles, []).

register_config(ConfigName, IniFiles, Options) ->
    gen_server:call(?MODULE, {register_conf, {ConfigName, IniFiles,
                                              Options}},
                    infinity).

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

%% @doc Subscribe to config events for a config named `ConfigName'
%%
%% The message received to each subscriber will be of the form:
%%
%% `{config_updated, ConfigName, {Section, Key}}'
%%
%% @end
subscribe(ConfigName) ->
    gproc:reg({p,l,{config_updated, ConfigName}}).

%% @doc Remove subscribtion created using `subscribe(ConfigName)'
%%
%% @end
unsubscribe(ConfigName) ->
    gproc:unreg({p,l,{config_updated, ConfigName}}).

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
    Matches = ets:match(?MODULE, {{conf_key(ConfigName), '$1', '$2'}, '$3'}),
    [{Section, Key, Value} || [Section, Key, Value] <- Matches].

%% @doc get all sections of a configuration
sections(ConfigName) ->
    Matches = ets:match(?MODULE, {{conf_key(ConfigName), '$1', '_'}, '_'}),
    lists:umerge(Matches).


%% @doc get all sections starting by Prefix
prefix(ConfigName, Prefix) ->
    Matches = ets:match(?MODULE, {{conf_key(ConfigName), '$1', '_'}, '_'}),
    Found = lists:foldl(fun([Match], Acc) ->
                    case re:split(Match, Prefix, [{return,list}]) of
                        [Match] -> Acc;
                        _ ->
                            case lists:member(Match, Acc) of
                                true -> Acc;
                                false -> [Match | Acc]
                            end
                    end
            end, [], Matches),
    lists:reverse(Found).

%% @doc retrive config as a proplist
cfg2list(ConfigName) ->
    Matches = ets:match(?MODULE, {{conf_key(ConfigName), '$1', '$2'}, '$3'}),
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
    Matches = ets:match(?MODULE, {{conf_key(ConfigName), '$1', '$2'}, '$3'}),
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
    Matches = ets:match(?MODULE, {{conf_key(ConfigName), Section, '$1'}, '$2'}),
    [{Key, Value} || [Key, Value] <- Matches].

%% @doc get value for a key in a section
get_value(ConfigName, Section, Key) ->
    get_value(ConfigName, Section, Key, undefined).

get_value(ConfigName, Section0, Key0, Default) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),

    case ets:lookup(?MODULE, {conf_key(ConfigName), Section, Key}) of
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
                                     Persist}}, infinity);

%% @doc set a value
set_value(ConfigName, Section, Key, Value) ->
    set_value(ConfigName, Section, Key, Value, true).

set_value(ConfigName, Section0, Key0, Value0, Persist) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),
    Value = econfig_util:to_list(Value0),

    gen_server:call(?MODULE, {set, {ConfigName, Section, Key, Value,
                                    Persist}}, infinity).

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



%% -----------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------

init(_) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, set, protected]),
    InitialState = initialize_app_confs(),
    {ok, InitialState}.

handle_call({register_conf, {ConfName, IniFiles, Options}}, _From,
            #state{confs=Confs}=State) ->
    {Resp, NewState} =
        try
            WriteFile = parse_inis(ConfName, IniFiles),
            {ok, Pid} = case proplists:get_value(autoreload, Options) of
                true -> 
                    econfig_watcher_sup:start_watcher(ConfName, IniFiles);
                Delay when is_integer(Delay) ->
                    econfig_watcher_sup:start_watcher(ConfName, IniFiles, Delay);
                _ ->
                    {ok, nil}
            end,
            Confs1 = dict:store(ConfName, #config{write_file=WriteFile,
                                                  pid=Pid,
                                                  options=Options,
                                                  inifiles=IniFiles},
                                Confs),
            {ok, State#state{confs=Confs1}}
        catch _Tag:Error ->
            {{error, Error}, State}
        end,
    {reply, Resp, NewState};

handle_call({unregister_conf, ConfName}, _From, #state{confs=Confs}=State) ->
    true = ets:match_delete(?MODULE, {{conf_key(ConfName), '_', '_'}, '_'}),
    case dict:find(ConfName, Confs) of
        {ok, #config{pid=Pid}} when is_pid(Pid) ->
            supervisor:terminate_child(econfig_watcher_sup, Pid);
        _ ->
            ok
    end,
    {reply, ok, State#state{confs=dict:erase(ConfName, Confs)}};

handle_call({reload, {ConfName, IniFiles0}}, _From,
            #state{confs=Confs}=State) ->

    case dict:find(ConfName, Confs) of
        {ok, #config{inifiles=IniFiles1, options=Options}=Conf} ->

            true = ets:match_delete(?MODULE, {{conf_key(ConfName), '_', '_'}, '_'}),
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
            notify_change(ConfName, reload),
            {reply, ok, State#state{confs=Confs1}};
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
            pause_autoreload(Conf),
            econfig_file_writer:save_to_file({Section, [{Key, Value}]}, FileName),
            restart_autoreload(Conf);
        _ ->
            ok
    end,
    case Result of
        ok ->
            Value1 = econfig_util:trim_whitespace(Value),
            case Value1 of
                [] ->
                    true = ets:delete(?MODULE, {conf_key(ConfName), Section, Key});
                _ ->
                    true = ets:insert(?MODULE, {{conf_key(ConfName), Section, Key}, Value1})
            end,
            notify_change(ConfName, set, Section, Key),
            {reply, ok, State};
        _Error ->
            {reply, Result, State}
    end;
handle_call({mset, {ConfName, Section, List, Persist}}, _From,
            #state{confs=Confs}=State) ->
    Result = case {Persist, dict:find(ConfName, Confs)} of
        {true, {ok, #config{write_file=FileName}=Conf}} when FileName /= nil->
            pause_autoreload(Conf),
            econfig_file_writer:save_to_file({Section, List}, FileName),
            restart_autoreload(Conf);
        _ ->
            ok
    end,
    case Result of
        ok ->
            lists:foreach(
                fun({Key,Value}) ->
                    Value1 = econfig_util:trim_whitespace(Value),
                    case Value1 of
                        [] ->
                            true = ets:delete(?MODULE, {conf_key(ConfName), Section, Key});
                        _ ->
                            true = ets:insert(?MODULE, {{conf_key(ConfName), Section,Key}, Value1})
                    end
                end, List),
            notify_change(ConfName, set, Section),
            {reply, ok, State};
        _Error ->
            {reply, Result, State}
    end;

handle_call({del, {ConfName, Section, Key, Persist}}, _From,
            #state{confs=Confs}=State) ->

    true = ets:delete(?MODULE, {conf_key(ConfName), Section, Key}),

    case {Persist, dict:find(ConfName, Confs)} of
        {true, {ok, #config{write_file=FileName}=Conf}} when FileName /= nil->
            pause_autoreload(Conf),
            econfig_file_writer:save_to_file({Section, [{Key, ""}]},
                                             FileName),
            restart_autoreload(Conf);
        _ ->
            ok
    end,
    notify_change(ConfName, delete, Section, Key),
    {reply, ok, State};
handle_call({mdel, {ConfName, Section, Persist}}, _From,
            #state{confs=Confs}=State) ->
    Matches = ets:match(?MODULE, {{conf_key(ConfName), Section, '$1'}, '$2'}),
    ToDelete = lists:foldl(fun([Key, _Val], Acc) ->

                    true = ets:delete(?MODULE, {conf_key(ConfName), Section, Key}),
                    [{Key, ""} | Acc]
            end, [], Matches),

    case {Persist, dict:find(ConfName, Confs)} of
        {true, {ok, #config{write_file=FileName}=Conf}} when FileName /= nil->
            pause_autoreload(Conf),
            econfig_file_writer:save_to_file({Section, ToDelete}, FileName),
            restart_autoreload(Conf);
        _ ->
            ok
    end,
    notify_change(ConfName, delete, Section),
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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

pause_autoreload(#config{pid=Pid}) when is_pid(Pid) ->
    econfig_watcher:pause(Pid);
pause_autoreload(_) ->
    ok.

restart_autoreload(#config{pid=Pid}) when is_pid(Pid) ->
    econfig_watcher:restart(Pid);
restart_autoreload(_) ->
    ok.


notify_change(ConfigName, Type) ->
    gproc:send({p, l, {config_updated, ConfigName}},
               {config_updated, ConfigName, Type}).

notify_change(ConfigName, Type, Section) ->
    gproc:send({p, l, {config_updated, ConfigName}},
               {config_updated, ConfigName, {Type, Section}}).

notify_change(ConfigName, Type, Section, Key) ->
    gproc:send({p, l, {config_updated, ConfigName}},
               {config_updated, ConfigName, {Type, {Section, Key}}}).


initialize_app_confs() ->
    case application:get_env(econfig, confs) of
        undefined -> #state{};
        {ok, Confs} -> initialize_app_confs1(Confs, #state{})
    end.

initialize_app_confs1([], State) ->
    State;
initialize_app_confs1([{ConfName, IniFiles} | Rest], State) ->
    initialize_app_confs1([{ConfName, IniFiles, []} | Rest], State);
initialize_app_confs1([{ConfName, IniFiles, Options} | Rest],
                      #state{confs=Confs}=State) ->
    WriteFile = parse_inis(ConfName, IniFiles),
    Pid = case proplists:get_value(autoreload, Options) of
        true ->
            {ok, Pid0} =
                        econfig_watcher_sup:start_watcher(ConfName,
                                                          IniFiles),
            Pid0;
        _ ->
            nil
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
                ets:insert(?MODULE, ParsedIniValues),
                lists:foreach(fun(Key) -> ets:delete(?MODULE, Key) end, DelKeys)
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
