-module(test_econfig_server).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok = application:load(econfig),
    ok = econfig:start(),
    ok = econfig:register_config(t, ["../priv/fixtures/test.ini"], []).

cleanup(_State) ->
    ok = application:stop(econfig),
    ok = application:stop(gproc).

simple_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_assertEqual("value1", econfig:get_value(t, "section1", "key1")),
      ?_assertEqual("value 2", econfig:get_value(t, "section1", "key2")),
      ?_assertEqual("value 3", econfig:get_value(t, "section1", "key 3")),
      ?_assertEqual("value 4", econfig:get_value(t, "section1", "key4")),
      ?_assertEqual("value5", econfig:get_value(t, "section1", "key5")),
      ?_assertEqual("value6", econfig:get_value(t, "section 2", "key6")),
      ?_assertEqual("value7", econfig:get_value(t, "section 2", "key7")),
      ?_assertEqual("value8", econfig:get_value(t, "section 2", "key8")),
      ?_assertEqual("value9", econfig:get_value(t, "section 2", "key9")),
      ?_assertEqual("value10", econfig:get_value(t, "section 2", "key10")),
      ?_assertEqual("new-val-11", econfig:get_value(t, "section3", "key11")),
      ?_assertEqual("this is a value for key 13", econfig:get_value(t, "section3", "key13")),
      ?_assertEqual("some-collection.of+random@characters", econfig:get_value(t, "section3", "key14"))
     ]}.
