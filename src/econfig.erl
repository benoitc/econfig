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

-type inifile() :: string().
-type inifiles() :: [inifile()].
-type options() :: [autoreload].

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
-endif.