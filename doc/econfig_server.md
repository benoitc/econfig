

#Module econfig_server#
* [Function Index](#index)
* [Function Details](#functions)






__Behaviours:__ [`gen_server`](gen_server.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-1">all/1</a></td><td>get all values of a configuration.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete_value-3">delete_value/3</a></td><td>delete a value.</td></tr><tr><td valign="top"><a href="#delete_value-4">delete_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>get values of a section.</td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td>get value for a key in a section.</td></tr><tr><td valign="top"><a href="#get_value-4">get_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_config-2">register_config/2</a></td><td>register inifiles.</td></tr><tr><td valign="top"><a href="#register_config-3">register_config/3</a></td><td></td></tr><tr><td valign="top"><a href="#reload-1">reload/1</a></td><td>reload the configuration.</td></tr><tr><td valign="top"><a href="#reload-2">reload/2</a></td><td>reload the configuration.</td></tr><tr><td valign="top"><a href="#set_value-4">set_value/4</a></td><td>set a value.</td></tr><tr><td valign="top"><a href="#set_value-5">set_value/5</a></td><td></td></tr><tr><td valign="top"><a href="#start_autoreload-1">start_autoreload/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop_autoreload-1">stop_autoreload/1</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td>Subscribe to config events for a config named <code>ConfigName</code></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#unregister_config-1">unregister_config/1</a></td><td>unregister a conf.</td></tr><tr><td valign="top"><a href="#unsubscribe-1">unsubscribe/1</a></td><td>Remove subscribtion created using <code>subscribe(ConfigName)</code>.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="all-1"></a>

###all/1##




`all(ConfigName) -> any()`



get all values of a configuration<a name="code_change-3"></a>

###code_change/3##




`code_change(OldVsn, State, Extra) -> any()`

<a name="delete_value-3"></a>

###delete_value/3##




`delete_value(ConfigName, Section, Key) -> any()`



delete a value<a name="delete_value-4"></a>

###delete_value/4##




`delete_value(ConfigName, Section0, Key0, Persist) -> any()`

<a name="get_value-2"></a>

###get_value/2##




`get_value(ConfigName, Section0) -> any()`



get values of a section<a name="get_value-3"></a>

###get_value/3##




`get_value(ConfigName, Section, Key) -> any()`



get value for a key in a section<a name="get_value-4"></a>

###get_value/4##




`get_value(ConfigName, Section0, Key0, Default) -> any()`

<a name="handle_call-3"></a>

###handle_call/3##




`handle_call(Msg, From, State) -> any()`

<a name="handle_cast-2"></a>

###handle_cast/2##




`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

###handle_info/2##




`handle_info(Info, State) -> any()`

<a name="init-1"></a>

###init/1##




`init(X1) -> any()`

<a name="register_config-2"></a>

###register_config/2##




<pre>register_config(ConfigName::term(), IniFiles::<a href="econfig.md#type-inifiles">econfig:inifiles()</a>) -> ok | {error, any()}</pre>
<br></br>




register inifiles<a name="register_config-3"></a>

###register_config/3##




`register_config(ConfigName, IniFiles, Options) -> any()`

<a name="reload-1"></a>

###reload/1##




`reload(ConfigName) -> any()`



reload the configuration<a name="reload-2"></a>

###reload/2##




`reload(ConfigName, IniFiles) -> any()`



reload the configuration<a name="set_value-4"></a>

###set_value/4##




`set_value(ConfigName, Section, Key, Value) -> any()`



set a value<a name="set_value-5"></a>

###set_value/5##




`set_value(ConfigName, Section0, Key0, Value0, Persist) -> any()`

<a name="start_autoreload-1"></a>

###start_autoreload/1##




`start_autoreload(ConfigName) -> any()`

<a name="start_link-0"></a>

###start_link/0##




`start_link() -> any()`

<a name="stop_autoreload-1"></a>

###stop_autoreload/1##




`stop_autoreload(ConfigName) -> any()`

<a name="subscribe-1"></a>

###subscribe/1##




`subscribe(ConfigName) -> any()`





Subscribe to config events for a config named `ConfigName`



The message received to each subscriber will be of the form:

`{config_updated, ConfigName, {Section, Key}}`
<a name="terminate-2"></a>

###terminate/2##




`terminate(Reason, State) -> any()`

<a name="unregister_config-1"></a>

###unregister_config/1##




`unregister_config(ConfigName) -> any()`



unregister a conf<a name="unsubscribe-1"></a>

###unsubscribe/1##




`unsubscribe(ConfigName) -> any()`



Remove subscribtion created using `subscribe(ConfigName)`
