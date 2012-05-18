

#econfig - simple Erlang config handler using INI files#


Copyright (c) 2012 Benoît Chesneau.

__Version:__ 0.3

econfig
=======

econfig is a simple Erlang config handler to manage a config from INI
files.

econfig can be use to read and update INI files. Values are cached in an
ETS table and you can manage multiple configuration profile. A process
can also subscribe to config updates events.

Autoreload of the config when an INI file is updated is supported, you can even
manage changes from a full config directory.

See the [NEWS](http://github.com/benoitc/econfig/blob/master/NEWS.md)
for last changes.

Examples
--------<pre>
1> application:start(gproc).
ok
2> application:start(econfig).
ok
3> econfig:register_config(couchdb, ["/Users/benoitc/refuge/rcouch/rel/rcouch/etc/default.ini", "/Users/benoitc/refuge/rcouch/rel/rcouch/etc/local.ini"], [autoreload]).
ok
4> econfig:subscribe(couchdb).
true
5> econfig:get_value(couchdb, "couchdb").
[{"delayed_commits","true"},
 {"file_compression","snappy"},
 {"os_process_timeout","5000"},
 {"uri_file","./data/couch.uri"},
 {"index_dir","./data"},
 {"max_document_size","4294967296"},
 {"database_dir","./data"},
 {"max_dbs_open","100"}]
6> econfig:set_value(couchdb, "ssl", "test", "1").
ok
7> flush().
Shell got {config_updated,couchdb,{set,{"ssl","test"}}}
ok
</pre>

Contribute
----------
For issues, comments or feedback please [create an issue!] [1][1]: http://github.com/benoitc/econfig/issues "econfig issues"


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/benoitc/econfig/blob/master/doc/econfig.md" class="module">econfig</a></td></tr>
<tr><td><a href="http://github.com/benoitc/econfig/blob/master/doc/econfig_app.md" class="module">econfig_app</a></td></tr>
<tr><td><a href="http://github.com/benoitc/econfig/blob/master/doc/econfig_file_writer.md" class="module">econfig_file_writer</a></td></tr>
<tr><td><a href="http://github.com/benoitc/econfig/blob/master/doc/econfig_server.md" class="module">econfig_server</a></td></tr>
<tr><td><a href="http://github.com/benoitc/econfig/blob/master/doc/econfig_sup.md" class="module">econfig_sup</a></td></tr>
<tr><td><a href="http://github.com/benoitc/econfig/blob/master/doc/econfig_util.md" class="module">econfig_util</a></td></tr>
<tr><td><a href="http://github.com/benoitc/econfig/blob/master/doc/econfig_watcher.md" class="module">econfig_watcher</a></td></tr>
<tr><td><a href="http://github.com/benoitc/econfig/blob/master/doc/econfig_watcher_sup.md" class="module">econfig_watcher_sup</a></td></tr></table>

