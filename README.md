rebar_erl_vsn
=====

defines for erlang versions

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

```erlang
{plugins, [rebar_erl_vsn]}.
{provider_hooks, [{pre, [{compile, erl_vsn}]}]}.
```

Then just call your plugin directly in an existing application:


    $ rebar3 compile
    ===> Fetching rebar_erl_vsn
    ===> Compiling rebar_erl_vsn
    <Plugin Output>

Defines
-------
Each major/minor pair get a define like `17.5` or `18.0` in addition the following 'events' are defined too (in combination with the corresponding version):


* `new_hash` (`16.0`) - new crypto:hash functions are the default and old ones deprecated.
* `maps`, `namespaced_types` (`17.0`) - maps are introduced, types like `dict()` now require a namespace.
* `large_maps` (`18`) - large maps are now feasable, while in R17 they caused performance problems