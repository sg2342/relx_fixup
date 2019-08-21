relx_fixup
=====

A rebar3 plugin....

Remove unneeded files from relx generated release

The plugin is supposed to be run after rebar3 release (via provider_hook).

Unless erts is included in the release and dev_mode is false the plugin will
do nothing.

When run the plugin will:

1. remove all files in the erts-* sub-directory of the release that
are not listed in erts_whitelist and then recursively remove all empty
directories under erts-*

2. remove all files in the lib sub directories of the release that
match the wildcards in lib_rm_wildcards and then recursively remove all empty
directories in lib

3. copy releases/$VSN/$NAME.boot to releases/$VSN/start.boot

Config
-----

```
%% enable relx_fixup plugin
{plugins, [{relx_fixup
	   ,{git, "https://github.com/sg2342/relx_fixup.git"
	    ,{branch, "master"}}}]}.
{provider_hooks, [{post, [{release, relx_fixup}]}]}.

%% rlex_fixup config
%% these are the default values for erts_whitelist and lib_rm_wildcards
{relx_fixup
 ,[{erts_whitelist
   ,[  "bin/erlexec"
     , "bin/beam.smp"
     , "bin/erl_child_setup"
     , "bin/heart"]}
   {lib_rm_wildcards
   ,[ "include/*.hrl"
    , "priv/obj/*.o", "priv/obj/Makefile"
    , "priv/lib/otp_test_engine.so"
    , "priv/conf/agent/*.conf"
    , "priv/conf/manager/*.conf" ]}]}.

```

Use
-----

    $ rebar3 release

