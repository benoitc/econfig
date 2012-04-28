

#Module econfig_server#
* [Function Index](#index)
* [Function Details](#functions)






__Behaviours:__ [`gen_server`](gen_server.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete_value-3">delete_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete_value-4">delete_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-4">get_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_config-2">register_config/2</a></td><td>register inifiles.</td></tr><tr><td valign="top"><a href="#set_value-4">set_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#set_value-5">set_value/5</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="code_change-3"></a>

###code_change/3##




`code_change(OldVsn, State, Extra) -> any()`

<a name="delete_value-3"></a>

###delete_value/3##




`delete_value(ConfigName, Section, Key) -> any()`

<a name="delete_value-4"></a>

###delete_value/4##




`delete_value(ConfigName, Section0, Key0, Persist) -> any()`

<a name="get_value-2"></a>

###get_value/2##




`get_value(ConfigName, Section0) -> any()`

<a name="get_value-3"></a>

###get_value/3##




`get_value(ConfigName, Section, Key) -> any()`

<a name="get_value-4"></a>

###get_value/4##




`get_value(ConfigName, Section0, Key0, Default) -> any()`

<a name="handle_call-3"></a>

###handle_call/3##




`handle_call(Msg, From, Config) -> any()`

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




`register_config(ConfigName, IniFiles) -> any()`



register inifiles<a name="set_value-4"></a>

###set_value/4##




`set_value(ConfigName, Section, Key, Value) -> any()`

<a name="set_value-5"></a>

###set_value/5##




`set_value(ConfigName, Section0, Key0, Value0, Persist) -> any()`

<a name="start_link-0"></a>

###start_link/0##




`start_link() -> any()`

<a name="terminate-2"></a>

###terminate/2##




`terminate(Reason, State) -> any()`

