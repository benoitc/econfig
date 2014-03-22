-module(test_econfig_server).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok = application:load(econfig),
    ok = econfig:start(),
    ?cmd("cp ../priv/fixtures/test.ini ./test.ini"),
    ok = econfig:register_config(t, ["test.ini"], []).

cleanup(_State) ->
    ok = application:stop(econfig),
    ok = application:unload(econfig),
    ?cmd("rm test.ini"),
    ok = application:stop(gproc).

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
      ?_assertEqual("some-collection.of+random@characters", econfig:get_value(t, "section3", "key14"))
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
       % add a new key
       fun() ->
           econfig:set_value(t, "section 2", "key666", "value666"),
           ?assertEqual("value666", econfig:get_value(t, "section 2", "key666"))
       end
     ]}}.

subscribe_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [% test subscribe
      fun() ->
          econfig:subscribe(t),
          econfig:set_value(t, "section 2", "key666", "value666"),
          econfig:unsubscribe(t),
          receive
              Message ->
                  ?assertEqual({config_updated, t, {set, {"section 2", "key666"}}}, Message)
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
