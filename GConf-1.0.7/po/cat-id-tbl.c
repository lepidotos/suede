# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR Free Software Foundation, Inc.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: PACKAGE VERSION\n"
"POT-Creation-Date: 2001-10-29 13:44-0500\n"
"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
"Language-Team: LANGUAGE <LL@li.org>\n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=CHARSET\n"
"Content-Transfer-Encoding: 8bit\n"

#: backends/bdb-backend.c:211
msgid "Unloading BerkeleyDB (BDB) backend module."
msgstr ""

#: backends/bdb-backend.c:234
#, c-format
msgid "Opened BerkeleyDB source at root %s"
msgstr ""

#: backends/bdb-backend.c:574
msgid "Initializing BDB backend module"
msgstr ""

#: backends/dir-utils.c:108
#, c-format
msgid "Couldn't find the %s root directory in the address `%s'"
msgstr ""

#: backends/dir-utils.c:124 backends/xml-backend.c:301 backends/xml-dir.c:1053
#, c-format
msgid "Could not make directory `%s': %s"
msgstr ""

#: backends/dir-utils.c:212
#, c-format
msgid "Can't read from or write to the %s root directory in the address `%s'"
msgstr ""

#: backends/xml-backend.c:233
msgid "Unloading XML backend module."
msgstr ""

#: backends/xml-backend.c:286
#, c-format
msgid "Couldn't find the XML root directory in the address `%s'"
msgstr ""

#: backends/xml-backend.c:386
#, c-format
msgid "Can't read from or write to the XML root directory in the address `%s'"
msgstr ""

#: backends/xml-backend.c:396
#, c-format
msgid "Directory/file permissions for XML source at root %s are: %o/%o"
msgstr ""

#: backends/xml-backend.c:680
msgid "Initializing XML backend module"
msgstr ""

#: backends/xml-backend.c:745
#, c-format
msgid "Failed to give up lock on XML dir `%s': %s"
msgstr ""

#: backends/xml-cache.c:119
msgid "Unsynced directory deletions when shutting down XML backend"
msgstr ""

#: backends/xml-cache.c:229
#, c-format
msgid ""
"Unable to remove directory `%s' from the XML backend cache, because it has "
"not been successfully synced to disk"
msgstr ""

#: backends/xml-cache.c:256
#, c-format
msgid ""
"%u items remain in the cache after cleaning already-synced items older than %"
"u seconds"
msgstr ""

#: backends/xml-dir.c:164
#, c-format
msgid "Could not stat `%s': %s"
msgstr ""

#: backends/xml-dir.c:174
#, c-format
msgid "XML filename `%s' is a directory"
msgstr ""

#: backends/xml-dir.c:298 backends/xml-dir.c:305
#, c-format
msgid "Failed to delete `%s': %s"
msgstr ""

#: backends/xml-dir.c:346
#, c-format
msgid "Failed to write file `%s': %s"
msgstr ""

#: backends/xml-dir.c:359
#, c-format
msgid "Failed to set mode on `%s': %s"
msgstr ""

#: backends/xml-dir.c:373 backends/xml-dir.c:383
#, c-format
msgid "Failed to rename `%s' to `%s': %s"
msgstr ""

#: backends/xml-dir.c:389
#, c-format
msgid "Failed to restore `%s' from `%s': %s"
msgstr ""

#: backends/xml-dir.c:401
#, c-format
msgid "Failed to delete old file `%s': %s"
msgstr ""

#. These are all fatal errors
#: backends/xml-dir.c:806
#, c-format
msgid "Failed to stat `%s': %s"
msgstr ""

#: backends/xml-dir.c:950
#, c-format
msgid "Duplicate entry `%s' in `%s', ignoring"
msgstr ""

#: backends/xml-dir.c:972
#, c-format
msgid "Entry with no name in XML file `%s', ignoring"
msgstr ""

#: backends/xml-dir.c:980
#, c-format
msgid "A toplevel node in XML file `%s' is <%s> rather than <entry>, ignoring"
msgstr ""

#: backends/xml-dir.c:1069
#, c-format
msgid "Failed to create file `%s': %s"
msgstr ""

#: backends/xml-dir.c:1077
#, c-format
msgid "Failed to close file `%s': %s"
msgstr ""

#. There was an error
#: backends/xml-entry.c:149
#, c-format
msgid "Ignoring XML node with name `%s': %s"
msgstr ""

#: backends/xml-entry.c:327
#, c-format
msgid "Ignoring schema name `%s', invalid: %s"
msgstr ""

#: backends/xml-entry.c:375
#, c-format
msgid "Ignoring XML node `%s': %s"
msgstr ""

#: backends/xml-entry.c:726
#, c-format
msgid "Failed reading default value for schema: %s"
msgstr ""

#: backends/xml-entry.c:940
#, c-format
msgid "No \"type\" attribute for <%s> node"
msgstr ""

#: backends/xml-entry.c:954
#, c-format
msgid "A node has unknown \"type\" attribute `%s', ignoring"
msgstr ""

#: backends/xml-entry.c:969
msgid "No \"value\" attribute for node"
msgstr ""

#: backends/xml-entry.c:1017 backends/xml-entry.c:1093
#, c-format
msgid "Didn't understand XML node <%s> inside an XML list node"
msgstr ""

#: backends/xml-entry.c:1051
msgid "Invalid type (list, pair, or unknown) in a list node"
msgstr ""

#: backends/xml-entry.c:1074
#, c-format
msgid "Bad XML node: %s"
msgstr ""

#: backends/xml-entry.c:1082
#, c-format
msgid "List contains a badly-typed node (%s, should be %s)"
msgstr ""

#: backends/xml-entry.c:1134
#, c-format
msgid "Ignoring bad car from XML pair: %s"
msgstr ""

#: backends/xml-entry.c:1143 backends/xml-entry.c:1166
msgid "parsing XML file: lists and pairs may not be placed inside a pair"
msgstr ""

#: backends/xml-entry.c:1156
#, c-format
msgid "Ignoring bad cdr from XML pair: %s"
msgstr ""

#: backends/xml-entry.c:1175
#, c-format
msgid "Didn't understand XML node <%s> inside an XML pair node"
msgstr ""

#: backends/xml-entry.c:1193
msgid "Didn't find car and cdr for XML pair node"
msgstr ""

#: backends/xml-entry.c:1199
msgid "Missing cdr from pair of values in XML file"
msgstr ""

#: backends/xml-entry.c:1206
msgid "Missing car from pair of values in XML file"
msgstr ""

#: backends/xml-entry.c:1211
msgid "Missing both car and cdr values from pair in XML file"
msgstr ""

#. -- end debug only
#: gconf/gconf-backend.c:167
#, c-format
msgid "No such file `%s'\n"
msgstr ""

#: gconf/gconf-backend.c:195
#, c-format
msgid "Bad address `%s'"
msgstr ""

#: gconf/gconf-backend.c:220
msgid "GConf won't work without dynamic module support (gmodule)"
msgstr ""

#: gconf/gconf-backend.c:230
#, c-format
msgid "Error opening module `%s': %s\n"
msgstr ""

#: gconf/gconf-backend.c:262
#, c-format
msgid "Couldn't locate backend module for `%s'"
msgstr ""

#: gconf/gconf-backend.c:299
msgid "Failed to shut down backend"
msgstr ""

#: gconf/gconf-database.c:232
msgid "Received invalid value in set request"
msgstr ""

#: gconf/gconf-database.c:240
#, c-format
msgid "Couldn't make sense of CORBA value received in set request for key `%s'"
msgstr ""

#: gconf/gconf-database.c:518
msgid "Received request to drop all cached data"
msgstr ""

#: gconf/gconf-database.c:535
msgid "Received request to sync synchronously"
msgstr ""

#: gconf/gconf-database.c:770
msgid "Fatal error: failed to get object reference for ConfigDatabase"
msgstr ""

#: gconf/gconf-database.c:929
#, c-format
msgid "Failed to sync one or more sources: %s"
msgstr ""

#. This error is not fatal; we basically ignore it.
#. * Because it's likely the right thing for the client
#. * app to simply continue.
#.
#: gconf/gconf-database.c:1015
#, c-format
msgid ""
"Failed to log addition of listener (%s); will not be able to restore this "
"listener on gconfd restart, resulting in unreliable notification of "
"configuration changes."
msgstr ""

#: gconf/gconf-database.c:1042
#, c-format
msgid "Listener ID %lu doesn't exist"
msgstr ""

#: gconf/gconf-database.c:1051
#, c-format
msgid ""
"Failed to log removal of listener to logfile (most likely harmless, may "
"result in a notification weirdly reappearing): %s"
msgstr ""

#: gconf/gconf-database.c:1169 gconf/gconf-sources.c:1294
#, c-format
msgid "Error getting value for `%s': %s"
msgstr ""

#: gconf/gconf-database.c:1216
#, c-format
msgid "Error setting value for `%s': %s"
msgstr ""

#: gconf/gconf-database.c:1259
#, c-format
msgid "Error unsetting `%s': %s"
msgstr ""

#: gconf/gconf-database.c:1283
#, c-format
msgid "Error getting default value for `%s': %s"
msgstr ""

#: gconf/gconf-database.c:1323
#, c-format
msgid "Error checking existence of `%s': %s"
msgstr ""

#: gconf/gconf-database.c:1347
#, c-format
msgid "Error removing dir `%s': %s"
msgstr ""

#: gconf/gconf-database.c:1374
#, c-format
msgid "Failed to get all entries in `%s': %s"
msgstr ""

#: gconf/gconf-database.c:1400
#, c-format
msgid "Error listing dirs in `%s': %s"
msgstr ""

#: gconf/gconf-database.c:1421
#, c-format
msgid "Error setting schema for `%s': %s"
msgstr ""

#: gconf/gconf-error.c:25
msgid "Success"
msgstr ""

#: gconf/gconf-error.c:26
msgid "Failed"
msgstr ""

#: gconf/gconf-error.c:27
msgid "Configuration server couldn't be contacted"
msgstr ""

#: gconf/gconf-error.c:28
msgid "Permission denied"
msgstr ""

#: gconf/gconf-error.c:29
msgid "Couldn't resolve address for configuration source"
msgstr ""

#: gconf/gconf-error.c:30
msgid "Bad key or directory name"
msgstr ""

#: gconf/gconf-error.c:31
msgid "Parse error"
msgstr ""

#: gconf/gconf-error.c:32
msgid "Corrupt data in configuration source database"
msgstr ""

#: gconf/gconf-error.c:33
msgid "Type mismatch"
msgstr ""

#: gconf/gconf-error.c:34
msgid "Key operation on directory"
msgstr ""

#: gconf/gconf-error.c:35
msgid "Directory operation on key"
msgstr ""

#: gconf/gconf-error.c:36
msgid "Can't overwrite existing read-only value"
msgstr ""

#: gconf/gconf-error.c:37
msgid "Object Activation Framework error"
msgstr ""

#: gconf/gconf-error.c:38
msgid "Operation not allowed without configuration server"
msgstr ""

#: gconf/gconf-error.c:39
msgid "Failed to get a lock"
msgstr ""

#: gconf/gconf-error.c:40
msgid "No database available to save your configuration"
msgstr ""

#: gconf/gconf-internals.c:114
#, c-format
msgid "No '/' in key `%s'"
msgstr ""

#: gconf/gconf-internals.c:227
#, c-format
msgid "Invalid UTF-8 in string value in '%s'"
msgstr ""

#: gconf/gconf-internals.c:286
msgid "Couldn't interpret CORBA value for list element"
msgstr ""

#: gconf/gconf-internals.c:288
#, c-format
msgid "Incorrect type for list element in %s"
msgstr ""

#: gconf/gconf-internals.c:301
msgid "Received list from gconfd with a bad list type"
msgstr ""

#: gconf/gconf-internals.c:482
msgid "Failed to convert object to IOR"
msgstr ""

#: gconf/gconf-internals.c:619
msgid "Invalid UTF-8 in locale for schema"
msgstr ""

#: gconf/gconf-internals.c:627
msgid "Invalid UTF-8 in short description for schema"
msgstr ""

#: gconf/gconf-internals.c:635
msgid "Invalid UTF-8 in long description for schema"
msgstr ""

#: gconf/gconf-internals.c:643
msgid "Invalid UTF-8 in owner for schema"
msgstr ""

#: gconf/gconf-internals.c:868
#, c-format
msgid "Couldn't open path file `%s': %s\n"
msgstr ""

#: gconf/gconf-internals.c:928
#, c-format
msgid "Adding source `%s'\n"
msgstr ""

#: gconf/gconf-internals.c:940
#, c-format
msgid "Read error on file `%s': %s\n"
msgstr ""

#: gconf/gconf-internals.c:1271 gconf/gconf-internals.c:1337
#: gconf/gconf-value.c:125 gconf/gconf-value.c:224 gconf/gconf-value.c:366
#: gconf/gconf-value.c:1084
msgid "Text contains invalid UTF-8"
msgstr ""

#: gconf/gconf-internals.c:1422
#, c-format
msgid "Expected list, got %s"
msgstr ""

#: gconf/gconf-internals.c:1432
#, c-format
msgid "Expected list of %s, got list of %s"
msgstr ""

#: gconf/gconf-internals.c:1571
#, c-format
msgid "Expected pair, got %s"
msgstr ""

#: gconf/gconf-internals.c:1585
#, c-format
msgid "Expected (%s,%s) pair, got a pair with one or both values missing"
msgstr ""

#: gconf/gconf-internals.c:1601
#, c-format
msgid "Expected pair of type (%s,%s) got type (%s,%s)"
msgstr ""

#: gconf/gconf-internals.c:1717
msgid "Quoted string doesn't begin with a quotation mark"
msgstr ""

#: gconf/gconf-internals.c:1779
msgid "Quoted string doesn't end with a quotation mark"
msgstr ""

#: gconf/gconf-internals.c:1915
msgid "Encoded value is not valid UTF-8"
msgstr ""

#: gconf/gconf-internals.c:2265 gconf/gconf.c:3082
#, c-format
msgid "CORBA error: %s"
msgstr ""

#: gconf/gconf-internals.c:2281
#, c-format
msgid "OAF problem description: '%s'"
msgstr ""

#: gconf/gconf-internals.c:2287
msgid "attempt to remove not-listed OAF object directory"
msgstr ""

#: gconf/gconf-internals.c:2292
msgid "attempt to add already-listed OAF directory"
msgstr ""

#: gconf/gconf-internals.c:2299
#, c-format
msgid "OAF parse error: %s"
msgstr ""

#: gconf/gconf-internals.c:2304
msgid "Unknown OAF error"
msgstr ""

#: gconf/gconf-internals.c:2437
#, c-format
msgid "Could not lock temporary file '%s': %s"
msgstr ""

#: gconf/gconf-internals.c:2464
#, c-format
msgid "Could not create file '%s', probably because it already exists"
msgstr ""

#: gconf/gconf-internals.c:2510
#, c-format
msgid "Failed to create or open '%s'"
msgstr ""

#: gconf/gconf-internals.c:2520
#, c-format
msgid "Failed to lock '%s': another process has the lock (%s)"
msgstr ""

#: gconf/gconf-internals.c:2548
#, c-format
msgid "IOR file '%s' not opened successfully, no gconfd located: %s"
msgstr ""

#: gconf/gconf-internals.c:2578
#, c-format
msgid "gconftool or other non-gconfd process has the lock file '%s'"
msgstr ""

#: gconf/gconf-internals.c:2595
msgid "couldn't contact ORB to resolve existing gconfd object reference"
msgstr ""

#: gconf/gconf-internals.c:2627
#, c-format
msgid "couldn't create directory `%s': %s"
msgstr ""

#: gconf/gconf-internals.c:2686
#, c-format
msgid "Can't write to file `%s': %s"
msgstr ""

#: gconf/gconf-internals.c:2727
#, c-format
msgid "We didn't have the lock on file `%s', but we should have"
msgstr ""

#: gconf/gconf-internals.c:2748
#, c-format
msgid "Failed to link '%s' to '%s': %s"
msgstr ""

#: gconf/gconf-internals.c:2760
#, c-format
msgid "Failed to remove lock file `%s': %s"
msgstr ""

#: gconf/gconf-internals.c:2779
#, c-format
msgid "Failed to clean up file '%s': %s"
msgstr ""

#: gconf/gconf-internals.c:2793
#, c-format
msgid "Failed to remove lock directory `%s': %s"
msgstr ""

#: gconf/gconf-internals.c:3003 gconf/gconfd.c:580
#, c-format
msgid "Failed to create %s: %s"
msgstr ""

#: gconf/gconf-internals.c:3036
#, c-format
msgid "Failed to create pipe for communicating with spawned gconf daemon: %s\n"
msgstr ""

#: gconf/gconf-internals.c:3060
#, c-format
msgid "Failed to launch configuration server: %s\n"
msgstr ""

#: gconf/gconf-internals.c:3082
msgid ""
"Failed to contact configuration server (a likely cause of this is that you "
"have an existing configuration server (gconfd) running, but it isn't "
"reachable from here - if you're logged in from two machines at once, you may "
"need to enable TCP networking for ORBit)\n"
msgstr ""

#: gconf/gconf-internals.c:3719
#, c-format
msgid "Failed to read from child pipe (%s)"
msgstr ""

#: gconf/gconf-internals.c:3784
#, c-format
msgid "Failed to fork (%s)"
msgstr ""

#: gconf/gconf-internals.c:3924
#, c-format
msgid "Failed to change to directory '%s' (%s)"
msgstr ""

#: gconf/gconf-internals.c:3934
#, c-format
msgid "Failed to execute child process (%s)"
msgstr ""

#: gconf/gconf-internals.c:3943
#, c-format
msgid "Failed to redirect output or input of child process (%s)"
msgstr ""

#: gconf/gconf-internals.c:3952
#, c-format
msgid "Failed to fork child process (%s)"
msgstr ""

#: gconf/gconf-internals.c:3960
msgid "Unknown error executing child process"
msgstr ""

#: gconf/gconf-internals.c:3981
#, c-format
msgid "Failed to read enough data from child pid pipe (%s)"
msgstr ""

#: gconf/gconf-internals.c:4031
#, c-format
msgid "Failed to create pipe for communicating with child process (%s)"
msgstr ""

#: gconf/gconf-sources.c:320
#, c-format
msgid "Failed to load source `%s': %s"
msgstr ""

#: gconf/gconf-sources.c:510
#, c-format
msgid "Schema `%s' specified for `%s' stores a non-schema value"
msgstr ""

#: gconf/gconf-sources.c:567
msgid "The '/' name can only be a directory, not a key"
msgstr ""

#: gconf/gconf-sources.c:596
#, c-format
msgid ""
"Value for `%s' set in a read-only source at the front of your configuration "
"path."
msgstr ""

#: gconf/gconf-sources.c:608
#, c-format
msgid ""
"Unable to store a value at key '%s', as the configuration server has no "
"writeable databases. There are two common causes of this problem: 1) your "
"configuration path file doesn't contain any databases or wasn't found or 2) "
"somehow we mistakenly created two gconfd processes. If you have two gconfd "
"processes (or had two at the time the second was launched), logging out, "
"killing all copies of gconfd, and logging back in may help. Perhaps the "
"problem is that you attempted to use GConf from two machines at once, and "
"ORBit still has its default configuration that prevents remote CORBA "
"connections? As always, check the user.* syslog for details on problems "
"gconfd encountered."
msgstr ""

#: gconf/gconf-sources.c:1164
#, c-format
msgid "Error finding metainfo: %s"
msgstr ""

#: gconf/gconf-sources.c:1233
#, c-format
msgid "Error getting metainfo: %s"
msgstr ""

#: gconf/gconf-sources.c:1257
#, c-format
msgid "Key `%s' listed as schema for key `%s' actually stores type `%s'"
msgstr ""

#: gconf/gconf-value.c:81
#, c-format
msgid "Didn't understand `%s' (expected integer)"
msgstr ""

#: gconf/gconf-value.c:91
#, c-format
msgid "Integer `%s' is too large or small"
msgstr ""

#: gconf/gconf-value.c:112
#, c-format
msgid "Didn't understand `%s' (expected real number)"
msgstr ""

#: gconf/gconf-value.c:156
#, c-format
msgid "Didn't understand `%s' (expected true or false)"
msgstr ""

#: gconf/gconf-value.c:232
#, c-format
msgid "Didn't understand `%s' (list must start with a '[')"
msgstr ""

#: gconf/gconf-value.c:245
#, c-format
msgid "Didn't understand `%s' (list must end with a ']')"
msgstr ""

#: gconf/gconf-value.c:296
#, c-format
msgid "Didn't understand `%s' (extra unescaped ']' found inside list)"
msgstr ""

#: gconf/gconf-value.c:327 gconf/gconf-value.c:488
#, c-format
msgid "Didn't understand `%s' (extra trailing characters)"
msgstr ""

#: gconf/gconf-value.c:374
#, c-format
msgid "Didn't understand `%s' (pair must start with a '(')"
msgstr ""

#: gconf/gconf-value.c:387
#, c-format
msgid "Didn't understand `%s' (pair must end with a ')')"
msgstr ""

#: gconf/gconf-value.c:417 gconf/gconf-value.c:503
#, c-format
msgid "Didn't understand `%s' (wrong number of elements)"
msgstr ""

#: gconf/gconf-value.c:457
#, c-format
msgid "Didn't understand `%s' (extra unescaped ')' found inside pair)"
msgstr ""

#: gconf/gconf.c:54
#, c-format
msgid "`%s': %s"
msgstr ""

#: gconf/gconf.c:287
#, c-format
msgid "Server couldn't resolve the address `%s'"
msgstr ""

#: gconf/gconf.c:572
msgid "Can't add notifications to a local configuration source"
msgstr ""

#: gconf/gconf.c:1868
#, c-format
msgid "Adding client to server's list failed, CORBA error: %s"
msgstr ""

#: gconf/gconf.c:2183
msgid "Failed to init GConf, exiting\n"
msgstr ""

#: gconf/gconf.c:2220
msgid "Must begin with a slash (/)"
msgstr ""

#: gconf/gconf.c:2242
msgid "Can't have two slashes (/) in a row"
msgstr ""

#: gconf/gconf.c:2244
msgid "Can't have a period (.) right after a slash (/)"
msgstr ""

#: gconf/gconf.c:2265
#, c-format
msgid "`%c' is an invalid character in key/directory names"
msgstr ""

#: gconf/gconf.c:2279
msgid "Key/directory may not end with a slash (/)"
msgstr ""

#: gconf/gconf.c:2520
#, c-format
msgid "Failure shutting down config server: %s"
msgstr ""

#: gconf/gconf.c:2581
#, c-format
msgid "Expected float, got %s"
msgstr ""

#: gconf/gconf.c:2616
#, c-format
msgid "Expected int, got %s"
msgstr ""

#: gconf/gconf.c:2651
#, c-format
msgid "Expected string, got %s"
msgstr ""

#: gconf/gconf.c:2688
#, c-format
msgid "Expected bool, got %s"
msgstr ""

#: gconf/gconf.c:2721
#, c-format
msgid "Expected schema, got %s"
msgstr ""

#: gconf/gconfd.c:247
msgid "Shutdown request received"
msgstr ""

#: gconf/gconfd.c:279
msgid ""
"gconfd compiled with debugging; trying to load gconf.path from the source "
"directory"
msgstr ""

#: gconf/gconfd.c:297
#, c-format
msgid ""
"No configuration files found, trying to use the default config source `%s'"
msgstr ""

#: gconf/gconfd.c:306
msgid ""
"No configuration sources in the source path, configuration won't be saved; "
"edit "
msgstr ""

#: gconf/gconfd.c:306
msgid "/path"
msgstr ""

#: gconf/gconfd.c:320
#, c-format
msgid "Error loading some config sources: %s"
msgstr ""

#: gconf/gconfd.c:332
msgid ""
"No config source addresses successfully resolved, can't load or store config "
"data"
msgstr ""

#: gconf/gconfd.c:349
msgid ""
"No writable config sources successfully resolved, may not be able to save "
"some configuration changes"
msgstr ""

#: gconf/gconfd.c:375
#, c-format
msgid "Received signal %d, dumping core. Please report a GConf bug."
msgstr ""

#: gconf/gconfd.c:391
#, c-format
msgid ""
"Received signal %d, shutting down abnormally. Please file a GConf bug report."
msgstr ""

#: gconf/gconfd.c:408
#, c-format
msgid "Received signal %d, shutting down cleanly"
msgstr ""

#. openlog() does not copy logname - what total brokenness.
#. So we free it at the end of main()
#: gconf/gconfd.c:526
#, c-format
msgid "starting (version %s), pid %u user '%s'"
msgstr ""

#: gconf/gconfd.c:567
msgid "Failed to get object reference for ConfigServer"
msgstr ""

#: gconf/gconfd.c:605
#, c-format
msgid "Failed to write byte to pipe fd %d so client program may hang: %s"
msgstr ""

#: gconf/gconfd.c:615
#, c-format
msgid "Failed to get lock for daemon, exiting: %s"
msgstr ""

#: gconf/gconfd.c:657
#, c-format
msgid "Error releasing lockfile: %s"
msgstr ""

#: gconf/gconfd.c:665
msgid "Exiting"
msgstr ""

#: gconf/gconfd.c:690
msgid "GConf server is not in use, shutting down."
msgstr ""

#: gconf/gconfd.c:1049
#, c-format
msgid "Returning exception: %s"
msgstr ""

#: gconf/gconfd.c:1149
#, c-format
msgid ""
"Failed to open gconfd logfile; won't be able to restore listeners after "
"gconfd shutdown (%s)"
msgstr ""

#: gconf/gconfd.c:1184
#, c-format
msgid ""
"Failed to close gconfd logfile; data may not have been properly saved (%s)"
msgstr ""

#: gconf/gconfd.c:1253
#, c-format
msgid "Could not open saved state file '%s' for writing: %s"
msgstr ""

#: gconf/gconfd.c:1267
#, c-format
msgid "Could not write saved state file '%s' fd: %d: %s"
msgstr ""

#: gconf/gconfd.c:1276
#, c-format
msgid "Failed to close new saved state file '%s': %s"
msgstr ""

#: gconf/gconfd.c:1290
#, c-format
msgid "Could not move aside old saved state file '%s': %s"
msgstr ""

#: gconf/gconfd.c:1300
#, c-format
msgid "Failed to move new save state file into place: %s"
msgstr ""

#: gconf/gconfd.c:1309
#, c-format
msgid ""
"Failed to restore original saved state file that had been moved to '%s': %s"
msgstr ""

#: gconf/gconfd.c:1784
#, c-format
msgid ""
"Unable to restore a listener on address '%s', couldn't resolve the database"
msgstr ""

#: gconf/gconfd.c:1830
#, c-format
msgid "Error reading saved state file: %s"
msgstr ""

#: gconf/gconfd.c:1883
#, c-format
msgid "Unable to open saved state file '%s': %s"
msgstr ""

#: gconf/gconfd.c:2000
#, c-format
msgid ""
"Failed to log addition of listener to gconfd logfile; won't be able to re-"
"add the listener if gconfd exits or shuts down (%s)"
msgstr ""

#: gconf/gconfd.c:2005
#, c-format
msgid ""
"Failed to log removal of listener to gconfd logfile; might erroneously re-"
"add the listener if gconfd exits or shuts down (%s)"
msgstr ""

#: gconf/gconfd.c:2028 gconf/gconfd.c:2193
#, c-format
msgid "Failed to get IOR for client: %s"
msgstr ""

#: gconf/gconfd.c:2043
#, c-format
msgid "Failed to open saved state file: %s"
msgstr ""

#: gconf/gconfd.c:2056
#, c-format
msgid "Failed to write client add to saved state file: %s"
msgstr ""

#: gconf/gconfd.c:2064
#, c-format
msgid "Failed to flush client add to saved state file: %s"
msgstr ""

#: gconf/gconfd.c:2154
msgid ""
"Some client removed itself from the GConf server when it hadn't been added."
msgstr ""

#: gconf/gconftool.c:68
msgid "Help options"
msgstr ""

#: gconf/gconftool.c:77
msgid "Set a key to a value and sync. Use with --type."
msgstr ""

#: gconf/gconftool.c:86
msgid "Print the value of a key to standard output."
msgstr ""

#: gconf/gconftool.c:95
msgid ""
"Set a schema and sync. Use with --short-desc, --long-desc, --owner, and --"
"type."
msgstr ""

#: gconf/gconftool.c:105
msgid "Unset the keys on the command line"
msgstr ""

#: gconf/gconftool.c:114
msgid "Print all key/value pairs in a directory."
msgstr ""

#: gconf/gconftool.c:123
msgid "Print all subdirectories in a directory."
msgstr ""

#: gconf/gconftool.c:132
msgid "Print all subdirectories and entries under a dir, recursively."
msgstr ""

#: gconf/gconftool.c:141
msgid "Return 0 if the directory exists, 2 if it does not."
msgstr ""

#: gconf/gconftool.c:150
msgid "Shut down gconfd. DON'T USE THIS OPTION WITHOUT GOOD REASON."
msgstr ""

#: gconf/gconftool.c:159
msgid "Return 0 if gconfd is running, 2 if not."
msgstr ""

#: gconf/gconftool.c:168
msgid ""
"Launch the config server (gconfd). (Normally happens automatically when "
"needed.)"
msgstr ""

#: gconf/gconftool.c:177
msgid ""
"Specify the type of the value being set, or the type of the value a schema "
"describes. Unique abbreviations OK."
msgstr ""

#: gconf/gconftool.c:178
msgid "int|bool|float|string|list|pair"
msgstr ""

#: gconf/gconftool.c:186
msgid ""
"Specify the type of the list value being set, or the type of the value a "
"schema describes. Unique abbreviations OK."
msgstr ""

#: gconf/gconftool.c:187 gconf/gconftool.c:196 gconf/gconftool.c:205
msgid "int|bool|float|string"
msgstr ""

#: gconf/gconftool.c:195
msgid ""
"Specify the type of the car pair value being set, or the type of the value a "
"schema describes. Unique abbreviations OK."
msgstr ""

#: gconf/gconftool.c:204
msgid ""
"Specify the type of the cdr pair value being set, or the type of the value a "
"schema describes. Unique abbreviations OK."
msgstr ""

#: gconf/gconftool.c:213
msgid "Specify a short half-line description to go in a schema."
msgstr ""

#: gconf/gconftool.c:214 gconf/gconftool.c:223
msgid "DESCRIPTION"
msgstr ""

#: gconf/gconftool.c:222
msgid "Specify a several-line description to go in a schema."
msgstr ""

#: gconf/gconftool.c:231
msgid "Specify the owner of a schema"
msgstr ""

#: gconf/gconftool.c:232
msgid "OWNER"
msgstr ""

#: gconf/gconftool.c:240
msgid "Specify a schema file to be installed"
msgstr ""

#: gconf/gconftool.c:241
msgid "FILENAME"
msgstr ""

#: gconf/gconftool.c:249
msgid "Specify a configuration source to use rather than the default path"
msgstr ""

#: gconf/gconftool.c:250
msgid "SOURCE"
msgstr ""

#: gconf/gconftool.c:258
msgid ""
"Access the config database directly, bypassing server. Requires that gconfd "
"is not running."
msgstr ""

#: gconf/gconftool.c:267
msgid ""
"Properly installs schema files on the command line into the database. "
"GCONF_CONFIG_SOURCE environment variable should be set to a non-default "
"config source or set to the empty string to use the default."
msgstr ""

#: gconf/gconftool.c:276
msgid ""
"Torture-test an application by setting and unsetting a bunch of values of "
"different types for keys on the command line."
msgstr ""

#: gconf/gconftool.c:285
msgid ""
"Torture-test an application by setting and unsetting a bunch of keys inside "
"the directories on the command line."
msgstr ""

#: gconf/gconftool.c:294
msgid "Get the short doc string for a key"
msgstr ""

#: gconf/gconftool.c:303
msgid "Get the long doc string for a key"
msgstr ""

#: gconf/gconftool.c:312
msgid "Get the name of the schema applied to this key"
msgstr ""

#: gconf/gconftool.c:321
msgid "Specify the schema name followed by the key to apply the schema name to"
msgstr ""

#: gconf/gconftool.c:330
msgid "Get the name of the default source"
msgstr ""

#: gconf/gconftool.c:382
#, c-format
msgid ""
"Error on option %s: %s.\n"
"Run '%s --help' to see a full list of available command line options.\n"
msgstr ""

#: gconf/gconftool.c:394
msgid "Can't get and set/unset simultaneously\n"
msgstr ""

#: gconf/gconftool.c:401
msgid "Can't set and get/unset simultaneously\n"
msgstr ""

#: gconf/gconftool.c:409
msgid "Can't use --all-entries with --get or --set\n"
msgstr ""

#: gconf/gconftool.c:417
msgid "Can't use --all-dirs with --get or --set\n"
msgstr ""

#: gconf/gconftool.c:427
msgid ""
"--recursive-list should not be used with --get, --set, --unset, --all-"
"entries, or --all-dirs\n"
msgstr ""

#: gconf/gconftool.c:437
msgid ""
"--set_schema should not be used with --get, --set, --unset, --all-entries, --"
"all-dirs\n"
msgstr ""

#: gconf/gconftool.c:443
msgid "Value type is only relevant when setting a value\n"
msgstr ""

#: gconf/gconftool.c:449
msgid "Must specify a type when setting a value\n"
msgstr ""

#: gconf/gconftool.c:459
msgid "Ping option must be used by itself.\n"
msgstr ""

#: gconf/gconftool.c:469
msgid "--dir-exists option must be used by itself.\n"
msgstr ""

#: gconf/gconftool.c:479
msgid "--install-schema-file must be used by itself.\n"
msgstr ""

#: gconf/gconftool.c:490
msgid "--makefile-install-rule must be used by itself.\n"
msgstr ""

#: gconf/gconftool.c:501
msgid "--break-key must be used by itself.\n"
msgstr ""

#: gconf/gconftool.c:512
msgid "--break-directory must be used by itself.\n"
msgstr ""

#: gconf/gconftool.c:519
msgid ""
"You must specify a config source with --config-source when using --direct\n"
msgstr ""

#: gconf/gconftool.c:525
#, c-format
msgid "Failed to init GConf: %s\n"
msgstr ""

#: gconf/gconftool.c:560
msgid "Must set the GCONF_CONFIG_SOURCE environment variable\n"
msgstr ""

#: gconf/gconftool.c:589
#, c-format
msgid "Failed to access configuration source(s): %s\n"
msgstr ""

#: gconf/gconftool.c:790
#, c-format
msgid "Shutdown error: %s\n"
msgstr ""

#: gconf/gconftool.c:833
msgid "Must specify one or more dirs to recursively list.\n"
msgstr ""

#: gconf/gconftool.c:867
#, c-format
msgid "Failure listing entries in `%s': %s\n"
msgstr ""

#: gconf/gconftool.c:885
msgid "(no value set)"
msgstr ""

#: gconf/gconftool.c:940
#, c-format
msgid "Failed to spawn the config server (gconfd): %s\n"
msgstr ""

#: gconf/gconftool.c:954
msgid "Must specify a key or keys to get\n"
msgstr ""

#: gconf/gconftool.c:989
#, c-format
msgid "Type: %s\n"
msgstr ""

#: gconf/gconftool.c:990
#, c-format
msgid "List Type: %s\n"
msgstr ""

#: gconf/gconftool.c:991
#, c-format
msgid "Car Type: %s\n"
msgstr ""

#: gconf/gconftool.c:992
#, c-format
msgid "Cdr Type: %s\n"
msgstr ""

#: gconf/gconftool.c:997
#, c-format
msgid "Default Value: %s\n"
msgstr ""

#: gconf/gconftool.c:997 gconf/gconftool.c:999 gconf/gconftool.c:1000
#: gconf/gconftool.c:1001
msgid "Unset"
msgstr ""

#: gconf/gconftool.c:999
#, c-format
msgid "Owner: %s\n"
msgstr ""

#: gconf/gconftool.c:1000
#, c-format
msgid "Short Desc: %s\n"
msgstr ""

#: gconf/gconftool.c:1001
#, c-format
msgid "Long Desc: %s\n"
msgstr ""

#: gconf/gconftool.c:1010 gconf/gconftool.c:1304
#, c-format
msgid "No value set for `%s'\n"
msgstr ""

#: gconf/gconftool.c:1014 gconf/gconftool.c:1308
#, c-format
msgid "Failed to get value for `%s': %s\n"
msgstr ""

#: gconf/gconftool.c:1057 gconf/gconftool.c:1069
#, c-format
msgid "Don't understand type `%s'\n"
msgstr ""

#: gconf/gconftool.c:1081
msgid "Must specify alternating keys/values as arguments\n"
msgstr ""

#: gconf/gconftool.c:1101
#, c-format
msgid "No value to set for key: `%s'\n"
msgstr ""

#: gconf/gconftool.c:1129
msgid "Cannot set schema as value\n"
msgstr ""

#: gconf/gconftool.c:1139
msgid "When setting a list you must specify a primitive list-type\n"
msgstr ""

#: gconf/gconftool.c:1153
msgid ""
"When setting a pair you must specify a primitive car-type and cdr-type\n"
msgstr ""

#: gconf/gconftool.c:1168
#, c-format
msgid "Error: %s\n"
msgstr ""

#: gconf/gconftool.c:1181
#, c-format
msgid "Error setting value: %s\n"
msgstr ""

#: gconf/gconftool.c:1199
#, c-format
msgid "Error syncing: %s\n"
msgstr ""

#: gconf/gconftool.c:1222
msgid "Must specify a key or keys on the command line\n"
msgstr ""

#: gconf/gconftool.c:1242
#, c-format
msgid "No schema known for `%s'\n"
msgstr ""

#: gconf/gconftool.c:1275
#, c-format
msgid "No doc string stored in schema at '%s'\n"
msgstr ""

#: gconf/gconftool.c:1280
#, c-format
msgid "Error getting schema at '%s': %s\n"
msgstr ""

#: gconf/gconftool.c:1287
#, c-format
msgid "No schema stored at '%s'\n"
msgstr ""

#: gconf/gconftool.c:1290
#, c-format
msgid "Value at '%s' is not a schema\n"
msgstr ""

#: gconf/gconftool.c:1346
msgid "Must specify a schema name followed by the key name to apply it to\n"
msgstr ""

#: gconf/gconftool.c:1353
#, c-format
msgid "Error associating schema name '%s' with key name '%s': %s\n"
msgstr ""

#: gconf/gconftool.c:1373
msgid "Must specify key (schema name) as the only argument\n"
msgstr ""

#: gconf/gconftool.c:1415
msgid "List type must be a primitive type: string, int, float or bool\n"
msgstr ""

#: gconf/gconftool.c:1435
msgid "Pair car type must be a primitive type: string, int, float or bool\n"
msgstr ""

#: gconf/gconftool.c:1455
msgid "Pair cdr type must be a primitive type: string, int, float or bool\n"
msgstr ""

#: gconf/gconftool.c:1470
#, c-format
msgid "Error setting value: %s"
msgstr ""

#: gconf/gconftool.c:1484
#, c-format
msgid "Error syncing: %s"
msgstr ""

#: gconf/gconftool.c:1499
msgid "Must specify one or more dirs to get key/value pairs from.\n"
msgstr ""

#: gconf/gconftool.c:1513
msgid "Must specify one or more keys to unset.\n"
msgstr ""

#: gconf/gconftool.c:1524
#, c-format
msgid "Error unsetting `%s': %s\n"
msgstr ""

#: gconf/gconftool.c:1547
msgid "Must specify one or more dirs to get subdirs from.\n"
msgstr ""

#: gconf/gconftool.c:1581
#, c-format
msgid "Error listing dirs: %s\n"
msgstr ""

#: gconf/gconftool.c:1623
#, c-format
msgid "WARNING: invalid or missing type for schema (%s)\n"
msgstr ""

#: gconf/gconftool.c:1632
#, c-format
msgid "WARNING: invalid or missing list_type for schema (%s)\n"
msgstr ""

#: gconf/gconftool.c:1643 gconf/gconftool.c:1673 gconf/gconftool.c:1702
#, c-format
msgid "WARNING: Failed to parse default value `%s' for schema (%s)\n"
msgstr ""

#: gconf/gconftool.c:1661
#, c-format
msgid "WARNING: invalid or missing car_type or cdr_type for schema (%s)\n"
msgstr ""

#: gconf/gconftool.c:1686
msgid "WARNING: You cannot set a default value for a schema\n"
msgstr ""

#: gconf/gconftool.c:1715
msgid "WARNING: gconftool internal error, unknown GConfValueType\n"
msgstr ""

#: gconf/gconftool.c:1762 gconf/gconftool.c:1783 gconf/gconftool.c:1804
#: gconf/gconftool.c:1825
#, c-format
msgid "WARNING: failed to parse type name `%s'\n"
msgstr ""

#: gconf/gconftool.c:1779
#, c-format
msgid ""
"WARNING: list_type can only be int, float, string or bool and not `%s'\n"
msgstr ""

#: gconf/gconftool.c:1800
#, c-format
msgid "WARNING: car_type can only be int, float, string or bool and not `%s'\n"
msgstr ""

#: gconf/gconftool.c:1821
#, c-format
msgid "WARNING: cdr_type can only be int, float, string or bool and not `%s'\n"
msgstr ""

#: gconf/gconftool.c:1849
msgid "WARNING: empty <applyto> node"
msgstr ""

#: gconf/gconftool.c:1852 gconf/gconftool.c:2115
#, c-format
msgid "WARNING: node <%s> not understood below <schema>\n"
msgstr ""

#: gconf/gconftool.c:1862
msgid "WARNING: no key specified for schema\n"
msgstr ""

#: gconf/gconftool.c:1895
msgid "WARNING: <locale> node has no `name=\"locale\"' attribute, ignoring\n"
msgstr ""

#: gconf/gconftool.c:1901
#, c-format
msgid ""
"WARNING: multiple <locale> nodes for locale `%s', ignoring all past first\n"
msgstr ""

#: gconf/gconftool.c:1982
#, c-format
msgid "WARNING: Invalid node <%s> in a <locale> node\n"
msgstr ""

#: gconf/gconftool.c:2011
#, c-format
msgid "WARNING: failed to install schema `%s' locale `%s': %s\n"
msgstr ""

#: gconf/gconftool.c:2019
#, c-format
msgid "Installed schema `%s' for locale `%s'\n"
msgstr ""

#: gconf/gconftool.c:2041
#, c-format
msgid "WARNING: failed to associate schema `%s' with key `%s': %s\n"
msgstr ""

#: gconf/gconftool.c:2049
#, c-format
msgid "Attached schema `%s' to key `%s'\n"
msgstr ""

#: gconf/gconftool.c:2128
msgid "You must have at least one <locale> entry in a <schema>\n"
msgstr ""

#: gconf/gconftool.c:2157
#, c-format
msgid "WARNING: node <%s> not understood below <schemalist>\n"
msgstr ""

#: gconf/gconftool.c:2179
#, c-format
msgid "Failed to open `%s': %s\n"
msgstr ""

#: gconf/gconftool.c:2186
#, c-format
msgid "Document `%s' is empty?\n"
msgstr ""

#: gconf/gconftool.c:2198
#, c-format
msgid ""
"Document `%s' has the wrong type of root node (<%s>, should be "
"<gconfschemafile>)\n"
msgstr ""

#: gconf/gconftool.c:2211
#, c-format
msgid "Document `%s' has no top level <gconfschemafile> node\n"
msgstr ""

#: gconf/gconftool.c:2225
#, c-format
msgid "WARNING: node <%s> below <gconfschemafile> not understood\n"
msgstr ""

#: gconf/gconftool.c:2236 gconf/gconftool.c:2268
#, c-format
msgid "Error syncing config data: %s"
msgstr ""

#: gconf/gconftool.c:2252
msgid "Must specify some schema files to install\n"
msgstr ""

#: gconf/gconftool.c:2289
#, c-format
msgid ""
"\n"
"%s\n"
msgstr ""

#: gconf/gconftool.c:2309
#, c-format
msgid "Failed to unset breakage key %s: %s\n"
msgstr ""

#: gconf/gconftool.c:2435
msgid "Must specify some keys to break\n"
msgstr ""

#: gconf/gconftool.c:2441
#, c-format
msgid ""
"Trying to break your application by setting bad values for key:\n"
"  %s\n"
msgstr ""

#: gconf/gconftool.c:2459
msgid "Must specify some directories to break\n"
msgstr ""

#: gconf/gconftool.c:2478
#, c-format
msgid ""
"Trying to break your application by setting bad values for keys in "
"directory:\n"
"  %s\n"
msgstr ""

#: wrappers/gtk/gconf-client.c:287 wrappers/gtk/gconf-client.c:304
#, c-format
msgid "GConf Error: %s\n"
msgstr ""

#: wrappers/gtk/gconf-client.c:795
#, c-format
msgid "GConf warning: failure listing pairs in `%s': %s"
msgstr ""

#: wrappers/gtk/gconf-client.c:997
#, c-format
msgid "Expected `%s' got `%s' for key %s"
msgstr ""
