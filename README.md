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


Examples
--------

Code that runs on R18 and above:


```erlang
-ifdef('18.0').
version() ->
  "18 and above".
-else.
version() ->
  "before 17".
-endif.
```


Targeting R17 (all variants) specially

```erlang
-ifdef('17.0').
-ifndef('18.0').
%% Code only executed in R17.*
-else.
%% Code executed for R18+
-endif.
-else.
%% Code executed < R17
-endif.
```


md5 function that does not throw deprecation warnings:


```erlang
-ifdef(new_hash).
md5(Data) ->
  crypto:hash(md5, Data).
-elseif.
md5(Data) ->
  crypto:md5(Data).
-endif.
```  