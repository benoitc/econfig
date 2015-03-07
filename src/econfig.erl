%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

%% @doc Public API of econfig.
-module(econfig).

-export([start/0, stop/0]).

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
         delete_value/2, delete_value/3, delete_value/4,
         get_bool/2, get_int/2, get_float/2, get_list/2, get_string/2, get_binary/2,
         get_bool/3, get_int/3, get_float/3, get_list/3, get_string/3, get_binary/3]).

-type inifile() :: string().
-type inifiles() :: [inifile()].
-type options() :: [autoreload].

%% @doc Start the couchbeam process. Useful when testing using the shell.
start() ->
    econfig_deps:ensure(),
    application:load(econfig),
    econfig_app:ensure_deps_started(),
    application:start(econfig).

%% @doc Stop the couchbeam process. Useful when testing using the shell.
stop() ->
    application:stop(econfig).


-spec register_config(term(), econfig:inifiles()) -> ok | {error,
                                                           any()}.
%% @doc register inifiles or config dirs
register_config(ConfigName, IniFiles) ->
    econfig_server:register_config(ConfigName, IniFiles).

-spec register_config(term(), econfig:inifiles(), econfig:options())
    -> ok | {error, any()}.
%% @doc register inifiles of config dirs with options
%% For now the only option is  `autoreload' to auto reload the config on
%% files or dirs changes.
%% Configs can also be registererd in the app configuration at startup:
%%
%%      [confs, [{ConfigName, IniFile},
%%               {ConfigName1, IniFiles1, [Options]}, ..]]
%%
%% Options:
%%
%% - `autoreload': auto reload the config on files or dirs changes
%% - `{autoreload, Delay}': autoreload the config file or dir
%%  changes. Delay set the time between each scan. Default is 5000
%%  and can be set using the `scan_delay' application environement
%%  for econfig.
register_config(ConfigName, IniFiles, Options) ->
    econfig_server:register_config(ConfigName, IniFiles, Options).

-spec unregister_config(term()) -> ok.
%% @doc unregister a conf
unregister_config(ConfigName) ->
    econfig_server:unregister_config(ConfigName).

%% @doc open or create an ini file an register it
open_config(ConfigName, IniFile) ->
    econfig_server:open_config(ConfigName, IniFile).

%% @doc open or create an ini file an register it. See the
%% register_config function for a list of available functions.
open_config(ConfigName, IniFile, Options) ->
    econfig_server:open_config(ConfigName, IniFile, Options).

%% @doc Subscribe to config events for a config named `ConfigName'
%%
%% The message received to each subscriber will be of the form:
%%
%% `{config_updated, ConfigName, {Section, Key}}'
%%
%% @end
subscribe(ConfigName) ->
    econfig_server:subscribe(ConfigName).

%% @doc Remove subscribtion created using `subscribe(ConfigName)'
%%
%% @end
unsubscribe(ConfigName) ->
    econfig_server:unsubscribe(ConfigName).

%% @doc reload the configuration
reload(ConfigName) ->
    econfig_server:reload(ConfigName).

%% @doc reload the configuration
reload(ConfigName, IniFiles) ->
    econfig_server:reload(ConfigName, IniFiles).


start_autoreload(ConfigName) ->
    econfig_server:start_autoreload(ConfigName).

stop_autoreload(ConfigName) ->
    econfig_server:stop_autoreload(ConfigName).

%% @doc get all values of a configuration
all(ConfigName) ->
    econfig_server:all(ConfigName).

%% @doc get all sections of a configuration
sections(ConfigName) ->
    econfig_server:sections(ConfigName).

%% @doc get all sections starting by Prefix
prefix(ConfigName, Prefix) ->
    econfig_server:prefix(ConfigName, Prefix).

%% @doc retrive config as a proplist
cfg2list(ConfigName) ->
    econfig_server:cfg2list(ConfigName).

%% @doc retrieve config as a proplist
cfg2list(ConfigName, GroupKey) ->
    econfig_server:cfg2list(ConfigName, GroupKey).

%% @doc get values of a section
get_value(ConfigName, Section) ->
    econfig_server:get_value(ConfigName, Section).

%% @doc get value for a key in a section
get_value(ConfigName, Section, Key) ->
    econfig_server:get_value(ConfigName, Section, Key).

get_value(ConfigName, Section, Key, Default) ->
    econfig_server:get_value(ConfigName, Section, Key, Default).

%% @doc set a value
set_value(ConfigName, Section, Value) ->
    econfig_server:set_value(ConfigName, Section, Value).

%% @doc set a value
set_value(ConfigName, Section, Key, Value) ->
    econfig_server:set_value(ConfigName, Section, Key, Value).

set_value(ConfigName, Section, Key, Value, Persist) ->
    econfig_server:set_value(ConfigName, Section, Key, Value, Persist).

%% @doc delete a value
delete_value(ConfigName, Section) ->
    econfig_server:delete_value(ConfigName, Section).

%% @doc delete a value
delete_value(ConfigName, Section, Key) ->
    econfig_server:delete_value(ConfigName, Section, Key).

%% @doc delete a value
delete_value(ConfigName, Section, Key, Persist) ->
    econfig_server:delete_value(ConfigName, Section, Key, Persist).

%% @ doc get value for a key in a section, try to cast it to boolean()
get_bool(Section, Key) -> get_bool(main, Section, Key).

get_bool(ConfigName, Section, Key) ->
    case econfig_server:get_value(ConfigName, Section, Key) of
        "true" -> true;
        "false" -> false;
        undefined -> false;
        Value -> Error = lists:flatten(io_lib:format("~ts.~ts.~ts wait for boolean but got ~p",
                                                     [ConfigName, Section, Key, Value])),
                 throw({econfig_error, Error})
    end.

%% @ doc get value for a key in a section, try to cast it to integer()
get_int(Section, Key) -> get_int(main, Section, Key).

get_int(ConfigName, Section, Key) ->
    Value = econfig_server:get_value(ConfigName, Section, Key),
    case string:to_integer(Value) of
        {Int, []} -> Int;
        _ -> Error = lists:flatten(io_lib:format("~ts.~ts.~ts wait for integer but got ~p",
                                                 [ConfigName, Section, Key, Value])),
             throw({econfig_error, Error})
    end.

%% @ doc get value for a key in a section, try to cast it to float()
get_float(Section, Key) -> get_float(main, Section, Key).

get_float(ConfigName, Section, Key) ->
    Value = econfig_server:get_value(ConfigName, Section, Key),
    case string:to_float(Value) of
        {Float, []} -> Float;
        _ -> Error = lists:flatten(io_lib:format("~ts.~ts.~ts wait for float but got ~p",
                                                 [ConfigName, Section, Key, Value])),
             throw({econfig_error, Error})
    end.

%% @ doc get value for a key in a section, parse it as [string()]
get_list(Section, Key) -> get_list(main, Section, Key).

get_list(ConfigName, Section, Key) ->
    case econfig_server:get_value(ConfigName, Section, Key) of
        undefined -> [];
        Value ->
            lists:filtermap(fun(V) ->
                                    case string:strip(V) of
                                        "" -> false;
                                        V2 -> {true, V2}
                                    end
                            end,
                            string:tokens(Value, ","))
    end.

%% @ doc just synonym to get_value
get_string(Section, Key) -> get_string(main, Section, Key).

get_string(ConfigName, Section, Key) ->
    econfig_server:get_value(ConfigName, Section, Key).

%% @ doc get value for a key in a section, return it as binary()
get_binary(Section, Key) -> get_binary(main, Section, Key).

get_binary(ConfigName, Section, Key) ->
    case econfig_server:get_value(ConfigName, Section, Key) of
        undefined -> undefined;
        Value -> list_to_binary(Value)
    end.
