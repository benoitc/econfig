%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(econfig).

-export([register_config/2, register_config/3,
         unregister_config/1,
         subscribe/1, unsubscribe/1,
         reload/1, reload/2,
         all/1,
         get_value/2, get_value/3, get_value/4,
         set_value/4, set_value/5,
         delete_value/3, delete_value/4]).


%% @doc register inifiles
register_config(ConfigName, IniFiles) ->
    econfig_server:register_config(ConfigName, IniFiles).

%% @doc register inifiles
register_config(ConfigName, IniFiles, Options) ->
    econfig_server:register_config(ConfigName, IniFiles, Options).

%% @doc unregister a conf
unregister_config(ConfigName) ->
    econfig_server:unregister_config(ConfigName).


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

%% @doc get all values of a configuration
all(ConfigName) ->
    econfig_server:all(ConfigName).

%% @doc get values of a section
get_value(ConfigName, Section) ->
    econfig_server:get_value(ConfigName, Section).

%% @doc get value for a key in a section
get_value(ConfigName, Section, Key) ->
    econfig_server:get_value(ConfigName, Section, Key).

get_value(ConfigName, Section, Key, Default) ->
    econfig_server:get_value(ConfigName, Section, Key, Default).

%% @doc set a value
set_value(ConfigName, Section, Key, Value) ->
    econfig_server:set_value(ConfigName, Section, Key, Value).

set_value(ConfigName, Section, Key, Value, Persist) ->
    econfig_server:set_value(ConfigName, Section, Key, Value, Persist).

%% @doc delete a value
delete_value(ConfigName, Section, Key) ->
    econfig_server:delete_value(ConfigName, Section, Key).

%% @doc delete a value
delete_value(ConfigName, Section, Key, Persist) ->
    econfig_server:delete_value(ConfigName, Section, Key, Persist).
