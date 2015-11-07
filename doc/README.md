

# econfig - simple Erlang config handler using INI files #

Copyright (c) 2012-2015 Benoît Chesneau.

__Version:__ 0.5.0

# econfig

econfig is a simple Erlang config handler to manage a config from INI
files.

econfig can be use to read and update INI files. Values are cached in an
ETS table and you can manage multiple configuration profiles. A process
can also subscribe to config updates events.

Autoreload of the config when an INI file is updated is supported, you can even
manage changes from a full config directory.

See the [NEWS](http://github.com/benoitc/econfig/blob/master/NEWS.md)
for last changes.

#### Useful modules are:

- [`econfig`](econfig.md): main module. It contains all the API.

## Examples

Quick usage example:

```
1> application:ensure_all_started(econfig).
ok
2> econfig:register_config(test, ["test/fixtures/test.ini", "test/fixtures/test2.ini"], [autoreload]).
ok
3> econfig:subscribe(test).
true
4> econfig:get_value(test, "section1").
[{"key 3","value 3"},
 {"key1","value1"},
 {"key2","value 2"},
 {"key4","value 4"},
 {"key5","value5"}]
5> econfig:set_value(test, "section1", "key6", "value6").
ok
6> flush().
Shell got {config_updated,test,{set,{"section1","key6"}}}
ok
```

## Specific features

### on_change hook

Some application may want to handle changes without suscribing to change. This change allows a user to pass a change function when registering the configuation. This function will be called each time a change happen.

Contribute
----------
For issues, comments or feedback please [create an issue!] [1]

[1]: http://github.com/benoitc/econfig/issues "econfig issues"
