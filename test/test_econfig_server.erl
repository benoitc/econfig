-module(test_econfig_server).
-include_lib("eunit/include/eunit.hrl").

setup_common() ->
    ok = application:load(econfig),
    ok = econfig:start(),
    ?cmd("cp ../priv/fixtures/test*.ini ./").

setup() ->
    setup_common(),
    ok = econfig:register_config(t, ["test.ini"], []).

setup_multi() ->
    setup_common(),
    ok = econfig:register_config(t, ["test.ini", "test2.ini"], []).

cleanup(_State) ->
    ok = application:stop(econfig),
    ok = application:unload(econfig),
    ?cmd("rm test*.ini"),
    ok = application:stop(gproc).

parse_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_assertEqual(lists:sort(["section1", "section 2", "section3", "section4"]), lists:sort(econfig:sections(t))),
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
      ?_assertEqual(undefined, econfig:get_value(t, "section3", "key15")),
      ?_assertEqual("value 16", econfig:get_value(t, "section4", "key16")),
      ?_assertEqual("value 17", econfig:get_value(t, "section4", "key17")),
      ?_assertEqual("", econfig:get_value(t, "section4", "key18")),
      ?_assertEqual("one, two, three, four", econfig:get_value(t, "section4", "key19"))
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
          ?assertEqual([true, true], [ResultSubscribe, ResultUnsubscribe])
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
