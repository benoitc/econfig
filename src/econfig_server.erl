-module(econfig_server).
-behaviour(gen_server).

-export([register_config/2,
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

-record(config, {notify_funs = [],
                 files = dict:new()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register_config(ConfigName, IniFiles) ->
    gen_server:call(?MODULE, {register_conf, {ConfigName, IniFiles}},
                    infinity).


get_value(ConfigName, Section0) ->
    Section = econfig_util:to_list(Section0),
    Matches = ets:match(?MODULE, {{ConfigName, Section, '$1'}, '$2'}),
    [{Key, Value} || [Key, Value] <- Matches].


get_value(ConfigName, Section, Key) ->
    get_value(ConfigName, Section, Key, undefined).

get_value(ConfigName, Section0, Key0, Default) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),

    case ets:lookup(?MODULE, {ConfigName, Section, Key}) of
        [] -> Default;
        [{_, Match}] -> Match
    end.

set_value(ConfigName, Section, Key, Value) ->
    set_value(ConfigName, Section, Key, Value, true).

set_value(ConfigName, Section0, Key0, Value0, Persist) ->
    Section = econfig_util:to_list(Section0),
    Key = econfig_util:to_list(Key0),
    Value = econfig_util:to_list(Value0),

    gen_server:call(?MODULE, {set, {ConfigName, Section, Key, Value,
                                    Persist}}, infinity).

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
    {ok, #config{}}.

handle_call({register_conf, {ConfName, IniFiles}}, _From,
            #config{files=Files}=State) ->
    {Resp, NewState} =
        try
            lists:map(fun(IniFile) ->
                        {ok, ParsedIniValues} = parse_ini_file(ConfName,
                                                               IniFile),
                        ets:insert(?MODULE, ParsedIniValues)
                end, IniFiles),
            WriteFile = lists:last(IniFiles),
            {ok, State#config{files=dict:store(ConfName, WriteFile, Files)}}
        catch _Tag:Error ->
            {{error, Error}, State}
        end,
    {reply, Resp, NewState};

handle_call({set, {ConfName, Section, Key, Value, Persist}}, _From,
            #config{files=Files}=State) ->
    Result = case {Persist, dict:find(ConfName, Files)} of
        {true, {ok, FileName}} ->
            econfig_file_writer:save_to_file({{Section, Key}, Value},
                                             FileName);
        _ ->
            ok
    end,
    case Result of
        ok ->
            lager:info("insert ~p~n", [{{ConfName, Section, Key},
                                        Value}]),
            true = ets:insert(?MODULE, {{ConfName, Section, Key},
                                        Value}),
            {reply, ok, State};
        _Error ->
            lager:info("got an error ~p", [Result]),
            {reply, Result, State}
    end;

handle_call({delete, {ConfName, Section, Key, Persist}}, _From,
            #config{files=Files}=State) ->
    true = ets:delete(?MODULE, {ConfName, Section, Key}),
    case {Persist, dict:find(ConfName, Files)} of
        {true, {ok, FileName}} ->
            econfig_file_writer:save_to_file({{Section, Key}, ""}, FileName);
        _ ->
            ok
    end,
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
            lager:error("~s~n", [Msg]),
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


