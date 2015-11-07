%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

%% @doc Public API of econfig
%% econfig rely on a central gen_server an an ETS ordered-set.
%% @end
-module(econfig).

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

-type conf() :: atom() | string() | binary().
-type inifile() :: string().
-type inifiles() :: [inifile()].
-type config_options() :: [autoreload | {autoreload, integer}
                          | {change_fun, fun()}].
-type section() :: string().
-type key() :: string().
-type value() :: string().
-type kvs() :: [{key(), value()}].

-export_type([conf/0, inifile/0, inifiles/0, config_options/0, section/0, key/0, value/0, kvs/0]).

%% @doc register inifiles or config dirs
-spec register_config(ConfigName::conf(), IniFiles::inifiles()) -> ok | {error, any()}.
register_config(ConfigName, IniFiles) ->
    econfig_server:register_config(ConfigName, IniFiles).


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
-spec register_config(ConfigName::conf(), IniFiles::inifiles(), Options::config_options()) -> ok | {error, any()}.
register_config(ConfigName, IniFiles, Options) ->
    econfig_server:register_config(ConfigName, IniFiles, Options).


%% @doc unregister a conf
-spec unregister_config(conf()) -> ok.
unregister_config(ConfigName) ->
    econfig_server:unregister_config(ConfigName).

%% @doc open or create an ini file an register it
-spec open_config(ConfigName::conf(), IniFiles::inifiles()) -> ok | {error, any()}.
open_config(ConfigName, IniFile) ->
    econfig_server:open_config(ConfigName, IniFile).

%% @doc open or create an ini file an register it. See the
%% register_config function for a list of available functions.
-spec open_config(ConfigName::conf(), IniFiles::inifiles(), Options::config_options()) -> ok | {error, any()}.
open_config(ConfigName, IniFile, Options) ->
    econfig_server:open_config(ConfigName, IniFile, Options).

%% @doc Subscribe to config events for a config named `ConfigName'
%%
%% The message received to each subscriber will be of the form:
%%
%% `{config_updated, ConfigName, {Section, Key}}'
%%
%% @end
-spec subscribe(ConfigName::conf()) -> ok.
subscribe(ConfigName) ->
    econfig_server:subscribe(ConfigName).

%% @doc Remove subscribtion created using `subscribe(ConfigName)'
%%
%% @end
-spec unsubscribe(ConfigName::conf()) -> ok.
unsubscribe(ConfigName) ->
    econfig_server:unsubscribe(ConfigName).

%% @doc reload the configuration
-spec reload(ConfigName::conf()) -> ok.
reload(ConfigName) ->
    econfig_server:reload(ConfigName).

%% @doc reload the configuration
-spec reload(ConfigName::conf(), IniFiles::inifiles()) -> ok.
reload(ConfigName, IniFiles) ->
    econfig_server:reload(ConfigName, IniFiles).

%% @doc start the config watcher.
-spec start_autoreload(ConfigName::conf()) -> ok.
start_autoreload(ConfigName) ->
    econfig_server:start_autoreload(ConfigName).

%% @doc stop the config watcher.
-spec stop_autoreload(ConfigName::conf()) -> ok.
stop_autoreload(ConfigName) ->
    econfig_server:stop_autoreload(ConfigName).

%% @doc get all values of a configuration
-spec all(ConfigName::conf()) -> [{section(), [{key(), value()}]}].
all(ConfigName) ->
    econfig_server:all(ConfigName).

%% @doc get all sections of a configuration
-spec sections(ConfigName::conf()) -> [section()].
sections(ConfigName) ->
    econfig_server:sections(ConfigName).

%% @doc get all sections starting by Prefix
-spec prefix(ConfigName::conf(), Prefix::string()) -> [{section(), [{key(), value()}]}].
prefix(ConfigName, Prefix) ->
    econfig_server:prefix(ConfigName, Prefix).

%% @doc retrive config as a proplist
-spec cfg2list(ConfigName::conf()) -> [{section(), [{key(), value()}]}].
cfg2list(ConfigName) ->
    econfig_server:cfg2list(ConfigName).

%% @doc retrieve config as a proplist
-spec cfg2list(ConfigName::conf(), GroupKey::string()) -> [{section(), [{key(), value()}]}].
cfg2list(ConfigName, GroupKey) ->
    econfig_server:cfg2list(ConfigName, GroupKey).

%% @doc get keys/values of a section
-spec get_value(ConfigName::conf(), Section::string()) -> [{key(), value()}].
get_value(ConfigName, Section) ->
    econfig_server:get_value(ConfigName, Section).

%% @doc get value for a key in a section
-spec get_value(ConfigName::conf(), Section::section(), Key::key()) -> Value::value() | undefined.
get_value(ConfigName, Section, Key) ->
    econfig_server:get_value(ConfigName, Section, Key).

-spec get_value(ConfigName::conf(), Section::section(), Key::key(), Default::value()) -> Value::value().
get_value(ConfigName, Section, Key, Default) ->
    econfig_server:get_value(ConfigName, Section, Key, Default).

%% @doc set a list of key/value for a section
-spec set_value(ConfigName::conf(), Section::section(), KVs::kvs()) -> ok.
set_value(ConfigName, Section, KVs) ->
    econfig_server:set_value(ConfigName, Section, KVs).

%% @doc set a value and persist it to the file
-spec set_value(ConfigName::conf(), Section::section(), Key::key(), Value::value()) -> ok.
set_value(ConfigName, Section, Key, Value) ->
    econfig_server:set_value(ConfigName, Section, Key, Value).

%% @doc set a value and optionnaly persist it.
-spec set_value(ConfigName::conf(), Section::section(), Key::key(), Value::value(), Persist::boolean()) -> ok.
set_value(ConfigName, Section, Key, Value, Persist) ->
    econfig_server:set_value(ConfigName, Section, Key, Value, Persist).

%% @doc delete all key/values from a section
-spec delete_value(ConfigName::conf(), Section::section()) -> ok.
delete_value(ConfigName, Section) ->
    econfig_server:delete_value(ConfigName, Section).

%% @doc delete a value and persist the change to the file
-spec delete_value(ConfigName::conf(), Section::section(), Key::key()) -> ok.
delete_value(ConfigName, Section, Key) ->
    econfig_server:delete_value(ConfigName, Section, Key).

%% @doc delete a value and optionnally persist it
-spec delete_value(ConfigName::conf(), Section::section(), Key::key(), Persist::boolean()) -> ok.
delete_value(ConfigName, Section, Key, Persist) ->
    econfig_server:delete_value(ConfigName, Section, Key, Persist).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

fixture_path(Name) ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppPath = filename:dirname(EbinDir),
    filename:join([AppPath, "test", "fixtures", Name]).



setup_common() ->
    {ok, _} = application:ensure_all_started(econfig),
    ok = file:write_file(fixture_path("local.ini"), <<>>).

setup() ->
    setup_common(),
    ok = econfig:register_config(t, [fixture_path("test.ini"), fixture_path("local.ini")], []).

setup_multi() ->
    setup_common(),
    ok = econfig:register_config(t, [fixture_path("test.ini"), fixture_path("test2.ini"), fixture_path("local.ini")], []).

cleanup(_State) ->
    error_logger:tty(false),
    ok = application:stop(econfig),
    error_logger:tty(true),
    ok = file:delete(fixture_path("local.ini")).

setup_change_fun() ->
    setup_common(),
    ets:new(econfig_test, [named_table, public]),
    ChangeFun = fun(Change) ->
                    ets:insert(econfig_test, Change)
                end,
    ok = econfig:register_config(t, [fixture_path("test.ini"), fixture_path("local.ini")], [{change_fun, ChangeFun}]).

cleanup_change_fun(State) ->
    ets:delete(econfig_test),
    cleanup(State).

parse_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_assertEqual(lists:sort(["section1", "section 2", "section3"]), lists:sort(econfig:sections(t))),
      ?_assertEqual("value1", econfig:get_value(t, "section1", "key1")),
      ?_assertEqual("value 2", econfig:get_value(t, "section1", "key2")),
      ?_assertEqual("value 3", econfig:get_value(t, "section1", "key 3")),
      ?_assertEqual("value 4", econfig:get_value(t, "section1", "key4")),
      ?_assertEqual("value5", econfig:get_value(t, "section1", "key5")),
      ?_assertEqual("value6", econfig:get_value(t, "section 2", "key6")),
      ?_assertEqual("value7", econfig:get_value(t, "section 2", "key7")),
      ?_assertEqual("value8", econfig:get_value(t, "section 2", "key8")),
      ?_assertEqual("value 9", econfig:get_value(t, "section 2", "key9")),
      ?_assertEqual("value10", econfig:get_value(t, "section 2", "key10")),
      ?_assertEqual("new-val-11", econfig:get_value(t, "section3", "key11")),
      ?_assertEqual("this is a value for key 13", econfig:get_value(t, "section3", "key13")),
      ?_assertEqual("some-collection.of+random@characters", econfig:get_value(t, "section3", "key14")),
      ?_assertEqual(undefined, econfig:get_value(t, "section3", "key15"))
     ]}.

parse_multi_test_() ->
    {setup,
     fun setup_multi/0,
     fun cleanup/1,
     [% matching section/key should have value from latter file
      ?_assertEqual("value6 overwrite", econfig:get_value(t, "section 2", "key6")),
      % non-matching from first file
      ?_assertEqual("value10", econfig:get_value(t, "section 2", "key10")),
      % non-matching from last file
      ?_assertEqual("value 888", econfig:get_value(t, "section 2", "key888"))
     ]}.

modify_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {inorder,
      [% modify existing key
       fun() ->
           econfig:set_value(t, "section 2", "key6", "value6_modified"),
           ?assertEqual("value6_modified", econfig:get_value(t, "section 2", "key6"))
       end,
       % delete an existing key
       fun() ->
           econfig:delete_value(t, "section 2", "key6"),
           ?assertEqual(undefined, econfig:get_value(t, "section 2", "key6"))
       end,
       % delete an existing key and reload
       fun() ->
           econfig:delete_value(t, "section 2", "key6"),
           econfig:reload(t),
           ?assertEqual(undefined, econfig:get_value(t, "section 2", "key6"))
       end,
       % add a new key
       fun() ->
           econfig:set_value(t, "section 2", "key666", "value666"),
           ?assertEqual("value666", econfig:get_value(t, "section 2", "key666"))
       end,
       % modify to empty
       fun() ->
           econfig:set_value(t, "section 2", "key7", ""),
           ?assertEqual(undefined, econfig:get_value(t, "section 2", "key7"))
       end,
       % modify to empty and reload
       fun() ->
           econfig:set_value(t, "section 2", "key7", ""),
           econfig:reload(t),
           ?assertEqual(undefined, econfig:get_value(t, "section 2", "key7"))
       end
     ]}}.

modify_multi_test_() ->
    {setup,
     fun setup_multi/0,
     fun cleanup/1,
     {inorder,
      [% modify existing key first file
       fun() ->
           econfig:set_value(t, "section 2", "key10", "value10modified"),
           ?assertEqual("value10modified", econfig:get_value(t, "section 2", "key10"))
       end,
       % modify existing key first file with reload
       fun() ->
           econfig:set_value(t, "section 2", "key10", "value10modified"),
           econfig:reload(t),
           ?assertEqual("value10modified", econfig:get_value(t, "section 2", "key10"))
       end,
       % modify existing key last file
       fun() ->
           econfig:set_value(t, "section 2", "key888", "value 888 modified"),
           ?assertEqual("value 888 modified", econfig:get_value(t, "section 2", "key888"))
       end,
       % modify existing key last file with reload
       fun() ->
           econfig:set_value(t, "section 2", "key888", "value 888 modified"),
           econfig:reload(t),
           ?assertEqual("value 888 modified", econfig:get_value(t, "section 2", "key888"))
       end,
       % modify existing matching key
       fun() ->
           econfig:set_value(t, "section 2", "key6", "value6modified"),
           ?assertEqual("value6modified", econfig:get_value(t, "section 2", "key6"))
       end,
       % modify existing matching key with reload
       fun() ->
           econfig:set_value(t, "section 2", "key6", "value6modified"),
           econfig:reload(t),
           ?assertEqual("value6modified", econfig:get_value(t, "section 2", "key6"))
       end,
       % modify existing matching key to empty value
       fun() ->
           econfig:set_value(t, "section 2", "key6", ""),
           ?assertEqual(undefined, econfig:get_value(t, "section 2", "key6"))
       end,
       % modify existing matching key to empty value with reload
       fun() ->
           econfig:set_value(t, "section 2", "key6", ""),
           econfig:reload(t),
           ?assertEqual(undefined, econfig:get_value(t, "section 2", "key6"))
       end,
       % delete existing matching key
       fun() ->
           econfig:delete_value(t, "section 2", "key7"),
           ?assertEqual(undefined, econfig:get_value(t, "section 2", "key7"))
       end,
       % modify existing key via list
       fun() ->
           econfig:set_value(t, "section10", [{"key1", "value1"}, {"key2", ""}]),
           ?assertEqual("value1", econfig:get_value(t, "section10", "key1"))
       end,
       % modify existing key to empty value via list
       fun() ->
           econfig:set_value(t, "section10", [{"key1", "value1"}, {"key2", ""}]),
           ?assertEqual(undefined, econfig:get_value(t, "section10", "key2"))
       end,
       % modify existing key to empty value via list with reload
       fun() ->
           econfig:set_value(t, "section10", [{"key1", "value1"}, {"key2", ""}]),
           econfig:reload(t),
           ?assertEqual(undefined, econfig:get_value(t, "section10", "key2"))
       end
      ]}}.

subscribe_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [% test basic subscribe/unsubscribe results
      fun() ->
          ResultSubscribe = econfig:subscribe(t),
          ResultUnsubscribe = econfig:unsubscribe(t),
          ?assertEqual([ok, ok], [ResultSubscribe, ResultUnsubscribe])
      end,
      % test subscribe update
      fun() ->
          econfig:subscribe(t),
          econfig:set_value(t, "section 2", "key666", "value666"),
          econfig:unsubscribe(t),
          receive
              Message ->
                  ?assertEqual({config_updated, t, {set, {"section 2", "key666"}}}, Message)
          end
      end,
      % test subscribe delete
      fun() ->
          econfig:subscribe(t),
          econfig:delete_value(t, "section 2", "key6"),
          econfig:unsubscribe(t),
          receive
              Message ->
                  ?assertEqual({config_updated, t, {delete, {"section 2", "key6"}}}, Message)
          end
      end,
      % test unsubscribe
      fun() ->
          econfig:subscribe(t),
          econfig:unsubscribe(t),
          econfig:set_value(t, "section 2", "key666", "value666"),
          Result = receive
              _ ->
                  true
              after 0 ->
                  false
          end,
          ?assertEqual(false, Result)
      end
     ]}.

change_fun_test_() ->
    {setup,
     fun setup_change_fun/0,
     fun cleanup_change_fun/1,
     [% test update
      fun() ->
          econfig:set_value(t, "section 2", "key666", "value666"),
          Changes = ets:tab2list(econfig_test),
          ?assertEqual([{config_updated, t, {set, {"section 2", "key666"}}}], Changes)
      end,
      % test subscribe delete
      fun() ->
          econfig:delete_value(t, "section 2", "key6"),
          Changes = ets:tab2list(econfig_test),
          ?assertEqual([{config_updated, t, {delete, {"section 2", "key6"}}}], Changes)
      end
     ]}.
-endif.