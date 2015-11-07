

# Module econfig #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Public API of econfig
econfig rely on a central gen_server an an ETS ordered-set.

<a name="types"></a>

## Data Types ##




### <a name="type-conf">conf()</a> ###


<pre><code>
conf() = atom() | string() | binary()
</code></pre>




### <a name="type-config_options">config_options()</a> ###


<pre><code>
config_options() = [autoreload | {autoreload, integer} | {change_fun, function()}]
</code></pre>




### <a name="type-inifile">inifile()</a> ###


<pre><code>
inifile() = string()
</code></pre>




### <a name="type-inifiles">inifiles()</a> ###


<pre><code>
inifiles() = [<a href="#type-inifile">inifile()</a>]
</code></pre>




### <a name="type-key">key()</a> ###


<pre><code>
key() = string()
</code></pre>




### <a name="type-kvs">kvs()</a> ###


<pre><code>
kvs() = [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]
</code></pre>




### <a name="type-section">section()</a> ###


<pre><code>
section() = string()
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = string()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-1">all/1</a></td><td>get all values of a configuration.</td></tr><tr><td valign="top"><a href="#cfg2list-1">cfg2list/1</a></td><td>retrive config as a proplist.</td></tr><tr><td valign="top"><a href="#cfg2list-2">cfg2list/2</a></td><td>retrieve config as a proplist.</td></tr><tr><td valign="top"><a href="#delete_value-2">delete_value/2</a></td><td>delete all key/values from a section.</td></tr><tr><td valign="top"><a href="#delete_value-3">delete_value/3</a></td><td>delete a value and persist the change to the file.</td></tr><tr><td valign="top"><a href="#delete_value-4">delete_value/4</a></td><td>delete a value and optionnally persist it.</td></tr><tr><td valign="top"><a href="#get_binary-3">get_binary/3</a></td><td>get a value and convert it to an float.</td></tr><tr><td valign="top"><a href="#get_binary-4">get_binary/4</a></td><td>get a value and convert it to an float.</td></tr><tr><td valign="top"><a href="#get_boolean-3">get_boolean/3</a></td><td>get a value and convert it to a boolean if possible
This method is case-insensitive and recognizes Boolean values from 'yes'/'no', 'on'/'off', 'true'/'false' and '1'/'0'
a badarg error is raised if the value can't be parsed to a boolean.</td></tr><tr><td valign="top"><a href="#get_boolean-4">get_boolean/4</a></td><td>get a value and convert it to a boolean if possible.</td></tr><tr><td valign="top"><a href="#get_float-3">get_float/3</a></td><td>get a value and convert it to an float.</td></tr><tr><td valign="top"><a href="#get_float-4">get_float/4</a></td><td>get a value and convert it to an float.</td></tr><tr><td valign="top"><a href="#get_integer-3">get_integer/3</a></td><td>get a value and convert it to an integer.</td></tr><tr><td valign="top"><a href="#get_integer-4">get_integer/4</a></td><td>get a value and convert it to an integer.</td></tr><tr><td valign="top"><a href="#get_list-3">get_list/3</a></td><td>get a value and convert it to an float.</td></tr><tr><td valign="top"><a href="#get_list-4">get_list/4</a></td><td>get a value and convert it to an float.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>get keys/values of a section.</td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td>get value for a key in a section.</td></tr><tr><td valign="top"><a href="#get_value-4">get_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#open_config-2">open_config/2</a></td><td>open or create an ini file an register it.</td></tr><tr><td valign="top"><a href="#open_config-3">open_config/3</a></td><td>open or create an ini file an register it.</td></tr><tr><td valign="top"><a href="#prefix-2">prefix/2</a></td><td>get all sections starting by Prefix.</td></tr><tr><td valign="top"><a href="#register_config-2">register_config/2</a></td><td>register inifiles or config dirs.</td></tr><tr><td valign="top"><a href="#register_config-3">register_config/3</a></td><td>register inifiles of config dirs with options
For now the only option is<code>autoreload</code> to auto reload the config on
files or dirs changes.</td></tr><tr><td valign="top"><a href="#reload-1">reload/1</a></td><td>reload the configuration.</td></tr><tr><td valign="top"><a href="#reload-2">reload/2</a></td><td>reload the configuration.</td></tr><tr><td valign="top"><a href="#sections-1">sections/1</a></td><td>get all sections of a configuration.</td></tr><tr><td valign="top"><a href="#set_value-3">set_value/3</a></td><td>set a list of key/value for a section.</td></tr><tr><td valign="top"><a href="#set_value-4">set_value/4</a></td><td>set a value and persist it to the file.</td></tr><tr><td valign="top"><a href="#set_value-5">set_value/5</a></td><td>set a value and optionnaly persist it.</td></tr><tr><td valign="top"><a href="#start_autoreload-1">start_autoreload/1</a></td><td>start the config watcher.</td></tr><tr><td valign="top"><a href="#stop_autoreload-1">stop_autoreload/1</a></td><td>stop the config watcher.</td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td>Subscribe to config events for a config named <code>ConfigName</code></td></tr><tr><td valign="top"><a href="#unregister_config-1">unregister_config/1</a></td><td>unregister a conf.</td></tr><tr><td valign="top"><a href="#unsubscribe-1">unsubscribe/1</a></td><td>Remove subscribtion created using <code>subscribe(ConfigName)</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-1"></a>

### all/1 ###

<pre><code>
all(ConfigName::<a href="#type-conf">conf()</a>) -&gt; [{<a href="#type-section">section()</a>, [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]}]
</code></pre>
<br />

get all values of a configuration

<a name="cfg2list-1"></a>

### cfg2list/1 ###

<pre><code>
cfg2list(ConfigName::<a href="#type-conf">conf()</a>) -&gt; [{<a href="#type-section">section()</a>, [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]}]
</code></pre>
<br />

retrive config as a proplist

<a name="cfg2list-2"></a>

### cfg2list/2 ###

<pre><code>
cfg2list(ConfigName::<a href="#type-conf">conf()</a>, GroupKey::string()) -&gt; [{<a href="#type-section">section()</a>, [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]}]
</code></pre>
<br />

retrieve config as a proplist

<a name="delete_value-2"></a>

### delete_value/2 ###

<pre><code>
delete_value(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>) -&gt; ok
</code></pre>
<br />

delete all key/values from a section

<a name="delete_value-3"></a>

### delete_value/3 ###

<pre><code>
delete_value(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>) -&gt; ok
</code></pre>
<br />

delete a value and persist the change to the file

<a name="delete_value-4"></a>

### delete_value/4 ###

<pre><code>
delete_value(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>, Persist::boolean()) -&gt; ok
</code></pre>
<br />

delete a value and optionnally persist it

<a name="get_binary-3"></a>

### get_binary/3 ###

<pre><code>
get_binary(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>) -&gt; Value::binary() | undefined
</code></pre>
<br />

get a value and convert it to an float

<a name="get_binary-4"></a>

### get_binary/4 ###

<pre><code>
get_binary(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>, Default::binary()) -&gt; Value::binary()
</code></pre>
<br />

get a value and convert it to an float

<a name="get_boolean-3"></a>

### get_boolean/3 ###

<pre><code>
get_boolean(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>) -&gt; Value::boolean() | undefined
</code></pre>
<br />

get a value and convert it to a boolean if possible
This method is case-insensitive and recognizes Boolean values from 'yes'/'no', 'on'/'off', 'true'/'false' and '1'/'0'
a badarg error is raised if the value can't be parsed to a boolean

<a name="get_boolean-4"></a>

### get_boolean/4 ###

<pre><code>
get_boolean(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>, Default::boolean()) -&gt; Value::boolean()
</code></pre>
<br />

get a value and convert it to a boolean if possible. It fallback to default if not set.
This method is case-insensitive and recognizes Boolean values from 'yes'/'no', 'on'/'off', 'true'/'false' and '1'/'0'
a badarg error is raised if the value can't be parsed to a boolean

<a name="get_float-3"></a>

### get_float/3 ###

<pre><code>
get_float(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>) -&gt; Value::float() | undefined
</code></pre>
<br />

get a value and convert it to an float

<a name="get_float-4"></a>

### get_float/4 ###

<pre><code>
get_float(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>, Default::float()) -&gt; Value::float()
</code></pre>
<br />

get a value and convert it to an float

<a name="get_integer-3"></a>

### get_integer/3 ###

<pre><code>
get_integer(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>) -&gt; Value::integer() | undefined
</code></pre>
<br />

get a value and convert it to an integer

<a name="get_integer-4"></a>

### get_integer/4 ###

<pre><code>
get_integer(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>, Default::integer()) -&gt; Value::integer()
</code></pre>
<br />

get a value and convert it to an integer

<a name="get_list-3"></a>

### get_list/3 ###

<pre><code>
get_list(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>) -&gt; Value::list() | undefined
</code></pre>
<br />

get a value and convert it to an float

<a name="get_list-4"></a>

### get_list/4 ###

<pre><code>
get_list(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>, Default::list()) -&gt; Value::list()
</code></pre>
<br />

get a value and convert it to an float

<a name="get_value-2"></a>

### get_value/2 ###

<pre><code>
get_value(ConfigName::<a href="#type-conf">conf()</a>, Section::string()) -&gt; [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]
</code></pre>
<br />

get keys/values of a section

<a name="get_value-3"></a>

### get_value/3 ###

<pre><code>
get_value(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>) -&gt; Value::<a href="#type-value">value()</a> | undefined
</code></pre>
<br />

get value for a key in a section

<a name="get_value-4"></a>

### get_value/4 ###

<pre><code>
get_value(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>, Default::<a href="#type-value">value()</a>) -&gt; Value::<a href="#type-value">value()</a>
</code></pre>
<br />

<a name="open_config-2"></a>

### open_config/2 ###

<pre><code>
open_config(ConfigName::<a href="#type-conf">conf()</a>, IniFiles::<a href="#type-inifiles">inifiles()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />

open or create an ini file an register it

<a name="open_config-3"></a>

### open_config/3 ###

<pre><code>
open_config(ConfigName::<a href="#type-conf">conf()</a>, IniFiles::<a href="#type-inifiles">inifiles()</a>, Options::<a href="#type-config_options">config_options()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />

open or create an ini file an register it. See the
register_config function for a list of available functions.

<a name="prefix-2"></a>

### prefix/2 ###

<pre><code>
prefix(ConfigName::<a href="#type-conf">conf()</a>, Prefix::string()) -&gt; [{<a href="#type-section">section()</a>, [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]}]
</code></pre>
<br />

get all sections starting by Prefix

<a name="register_config-2"></a>

### register_config/2 ###

<pre><code>
register_config(ConfigName::<a href="#type-conf">conf()</a>, IniFiles::<a href="#type-inifiles">inifiles()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />

register inifiles or config dirs

<a name="register_config-3"></a>

### register_config/3 ###

<pre><code>
register_config(ConfigName::<a href="#type-conf">conf()</a>, IniFiles::<a href="#type-inifiles">inifiles()</a>, Options::<a href="#type-config_options">config_options()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />

register inifiles of config dirs with options
For now the only option is`autoreload` to auto reload the config on
files or dirs changes.
Configs can also be registererd in the app configuration at startup:

[confs, [{ConfigName, IniFile},
{ConfigName1, IniFiles1, [Options]}, ..]]

Options:

- `autoreload`: auto reload the config on files or dirs changes
- `{autoreload, Delay}`: autoreload the config file or dir
changes. Delay set the time between each scan. Default is 5000
and can be set using the `scan_delay` application environement
for econfig.

<a name="reload-1"></a>

### reload/1 ###

<pre><code>
reload(ConfigName::<a href="#type-conf">conf()</a>) -&gt; ok
</code></pre>
<br />

reload the configuration

<a name="reload-2"></a>

### reload/2 ###

<pre><code>
reload(ConfigName::<a href="#type-conf">conf()</a>, IniFiles::<a href="#type-inifiles">inifiles()</a>) -&gt; ok
</code></pre>
<br />

reload the configuration

<a name="sections-1"></a>

### sections/1 ###

<pre><code>
sections(ConfigName::<a href="#type-conf">conf()</a>) -&gt; [<a href="#type-section">section()</a>]
</code></pre>
<br />

get all sections of a configuration

<a name="set_value-3"></a>

### set_value/3 ###

<pre><code>
set_value(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, KVs::<a href="#type-kvs">kvs()</a>) -&gt; ok
</code></pre>
<br />

set a list of key/value for a section

<a name="set_value-4"></a>

### set_value/4 ###

<pre><code>
set_value(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>) -&gt; ok
</code></pre>
<br />

set a value and persist it to the file

<a name="set_value-5"></a>

### set_value/5 ###

<pre><code>
set_value(ConfigName::<a href="#type-conf">conf()</a>, Section::<a href="#type-section">section()</a>, Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Persist::boolean()) -&gt; ok
</code></pre>
<br />

set a value and optionnaly persist it.

<a name="start_autoreload-1"></a>

### start_autoreload/1 ###

<pre><code>
start_autoreload(ConfigName::<a href="#type-conf">conf()</a>) -&gt; ok
</code></pre>
<br />

start the config watcher.

<a name="stop_autoreload-1"></a>

### stop_autoreload/1 ###

<pre><code>
stop_autoreload(ConfigName::<a href="#type-conf">conf()</a>) -&gt; ok
</code></pre>
<br />

stop the config watcher.

<a name="subscribe-1"></a>

### subscribe/1 ###

<pre><code>
subscribe(ConfigName::<a href="#type-conf">conf()</a>) -&gt; ok
</code></pre>
<br />

Subscribe to config events for a config named `ConfigName`

The message received to each subscriber will be of the form:

`{config_updated, ConfigName, {Section, Key}}`

<a name="unregister_config-1"></a>

### unregister_config/1 ###

<pre><code>
unregister_config(ConfigName::<a href="#type-conf">conf()</a>) -&gt; ok
</code></pre>
<br />

unregister a conf

<a name="unsubscribe-1"></a>

### unsubscribe/1 ###

<pre><code>
unsubscribe(ConfigName::<a href="#type-conf">conf()</a>) -&gt; ok
</code></pre>
<br />

Remove subscribtion created using `subscribe(ConfigName)`

