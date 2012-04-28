%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(econfig_server).
-behaviour(gen_server).

-export([register_config/2, register_config/3,
         unregister_config/1,
         subscribe/1, unsubscribe/1,
         reload/1, reload/2,
         all/1,
         get_value/2, get_value/3, get_value/4,
         set_value/4, set_value/5,
         delete_value/3, delete_value/4]).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {confs = dict:new()}).
-record(config, {write_file,
                 pid=nil,
                 options,
                 inifiles}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    gen_server:call(?MODULE, {reload, {ConfigName, IniFiles}},
                    infinity).

%% @doc get all values of a configuration
all(ConfigName) ->
    Matches = ets:match(?MODULE, {{ConfigName, '$1', '$2'}, '$3'}),
    [{Section, Key, Value} || [Section, Key, Value] <- Matches].

%% @doc get values of a section
get_value(ConfigName, Section0) ->
    Section = econfig_util:to_list(Section0),
    Matches = ets:match(?MODULE, {{ConfigName, Section, '$1'}, '$2'}),
    [{Key, Value} || [Key, Value] <- Matches].

%% @doc get value for a key in a section
get_value(ConfigName, Section, Key) ->
    get_value(ConfigName, Section, Key, undefined).

get_value(ConfigName, Section0, Key0, Default) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),

    case ets:lookup(?MODULE, {ConfigName, Section, Key}) of
        [] -> Default;
        [{_, Match}] -> Match
    end.

%% @doc set a value
set_value(ConfigName, Section, Key, Value) ->
    set_value(ConfigName, Section, Key, Value, true).

set_value(ConfigName, Section0, Key0, Value0, Persist) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),
    Value = econfig_util:to_list(Value0),

    gen_server:call(?MODULE, {set, {ConfigName, Section, Key, Value,
                                    Persist}}, infinity).

%% @doc delete a value
delete_value(ConfigName, Section, Key) ->
    delete_value(ConfigName, Section, Key, true).

delete_value(ConfigName, Section0, Key0, Persist) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),

    gen_server:call(?MODULE, {delete, {ConfigName, Section, Key,
                                       Persist}}, infinity).



%% -----------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------

init(_) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, set, protected]),
    {ok, #state{}}.

handle_call({register_conf, {ConfName, IniFiles, Options}}, _From,
            #state{confs=Confs}=State) ->
    {Resp, NewState} =
        try
            lists:map(fun(IniFile) ->
                        {ok, ParsedIniValues} = parse_ini_file(ConfName,
                                                               IniFile),
                        ets:insert(?MODULE, ParsedIniValues)
                end, IniFiles),
            WriteFile = lists:last(IniFiles),
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
            {ok, State#state{confs=Confs1}}
        catch _Tag:Error ->
            {{error, Error}, State}
        end,
    {reply, Resp, NewState};

handle_call({unregister_conf, ConfName}, _From, #state{confs=Confs}=State) ->
    true = ets:match_delete(?MODULE, {{ConfName, '_', '_'}, '_'}),
    {ok, #config{pid=Pid}} = dict:find(ConfName, Confs),
    supervisor:terminate_child(econfig_watcher_sup, Pid),
    {reply, ok, State#state{confs=dict:erase(ConfName, Confs)}};

handle_call({reload, {ConfName, IniFiles0}}, From,
            #state{confs=Confs}=State) ->
    true = ets:match_delete(?MODULE, {{ConfName, '_', '_'}, '_'}),
    IniFiles = case IniFiles0 of
        nil ->
            {ok, #config{inifiles=IniFiles1}} = dict:find(ConfName,
                                                         Confs),
            IniFiles1;
        _ ->
            IniFiles0
    end,
    handle_call({register_conf, {ConfName, IniFiles}}, From, State);


handle_call({set, {ConfName, Section, Key, Value, Persist}}, _From,
            #state{confs=Confs}=State) ->
    Result = case {Persist, dict:find(ConfName, Confs)} of
        {true, {ok, #config{write_file=FileName}}} ->
            econfig_file_writer:save_to_file({{Section, Key}, Value},
                                             FileName);
        _ ->
            ok
    end,
    case Result of
        ok ->
            true = ets:insert(?MODULE, {{ConfName, Section, Key},
                                        Value}),
            notify_change(ConfName, Section, Key),
            {reply, ok, State};
        _Error ->
            {reply, Result, State}
    end;

handle_call({delete, {ConfName, Section, Key, Persist}}, _From,
            #state{confs=Confs}=State) ->
    true = ets:delete(?MODULE, {ConfName, Section, Key}),
    case {Persist, dict:find(ConfName, Confs)} of
        {true, {ok, #config{write_file=FileName}}} ->
            econfig_file_writer:save_to_file({{Section, Key}, ""}, FileName);
        _ ->
            ok
    end,
    notify_change(ConfName, Section, Key),
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




%% -----------------------------------------------
%% internal functions
%% -----------------------------------------------

notify_change(ConfigName, Section, Key) ->
    gproc:send({p, l, {config_updated, ConfigName}},
               {config_updated, ConfigName, {Section, Key}}).

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
    {_, ParsedIniValues} =
    lists:foldl(fun(Line, {AccSectionName, AccValues}) ->
            case string:strip(Line) of
            "[" ++ Rest ->
                case re:split(Rest, "\\]", [{return, list}]) of
                [NewSectionName, ""] ->
                    {NewSectionName, AccValues};
                _Else -> % end bracket not at end, ignore this line
                    {AccSectionName, AccValues}
                end;
            ";" ++ _Comment ->
                {AccSectionName, AccValues};
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
                        case re:split(Value, " ;|\t;", [{return, list}]) of
                        [[]] ->
                            % empty line
                            {AccSectionName, AccValues};
                        [LineValue | _Rest] ->
                            E = {{AccSectionName, ValueName},
                                PrevValue ++ " " ++ LineValue},
                            {AccSectionName, [E | AccValuesRest]}
                        end;
                    _ ->
                        {AccSectionName, AccValues}
                    end;
                [""|_LineValues] -> % line begins with "=", ignore
                    {AccSectionName, AccValues};
                [ValueName|LineValues] -> % yeehaw, got a line!
                    RemainingLine = econfig_util:implode(LineValues, "="),
                    % removes comments
                    case re:split(RemainingLine, " ;|\t;", [{return, list}]) of
                        [[]] ->
                            % empty line means delete this key
                            ets:delete(?MODULE, {ConfName, AccSectionName,
                                                 ValueName}),
                            {AccSectionName, AccValues};
                        [LineValue | _Rest] ->
                            {AccSectionName,
                             [{{ConfName, AccSectionName, ValueName}, LineValue}
                              | AccValues]}
                        end
                end
            end
        end, {"", []}, Lines),
    {ok, ParsedIniValues}.


