#econfig news

0.4.1 - 2013-06-20
------------------

- add `econfig:set_value/3`: function to fill a complete section with a
  proplists.
- add `econfig:delete_value/2`: function to delete all the keys in a
  section.

0.4 - 2013-06-22
----------------

- add econfig:start/0 and econfig:stop/0 functions to start and stop easily econfig in tests or on the shell.
- add `econfig:section/1`  function to get a list of all sections.
- add `econfig:prefix` function to get al l sections starting with Prefix
- add `econfig:cfg2list/2`  to retrieve all the configuration as a proplist
- add `econfig:cfg2list/3`  to retrieves all the config as a proplist and group sections by key:

0.3 - 2012-05-18
----------------

- add `{autoreload, ScanDelay}` to `register_config/3`
- advertise the reload event and describe the updates types.

0.2 - 2012-04-30
----------------

- add the possibility to initialize econfig with defaults configs.
- improve files changes handling: don't reload when a value is
  updated from econfig
- fix config dirs handling

0.1 - 2012-04-29
----------------

- Initial release
