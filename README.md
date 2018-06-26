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
When added to your project, this plugin adds a define for each major/minor pair, such as `17.5` or `18.0`.  These are defined whenever the current version detected by the plugin is greater than the major/minor pair, so for example a build done running 19.0 will have '19.0', '18.3', '18.2', '18.1', & so on defined, down to '14.0'.

For clarity, some major breakages have their own special defines.  The following are defined (in addition to their corresponding version defines):

* `new_hash` (`16.0`) - new crypto:hash functions are the default and old ones deprecated.
* `long_schedule` (`16.1`) - `long_schedule` warnings.
* `maps`, `namespaced_types` (`17.0`) - maps are introduced, types like `dict()` now require a namespace.
* `large_maps` (`18`) - large maps are now feasible, while in R17 they caused performance problems
* `rand` (`19`) - introduction of the rand module.
* `ceil_bif`, `high_bit_uniform`, `fsm_deprecated` (`20`) - we get a ceil bif, and fsm is deprecated, uniform for high bits is improved.
* `ssl_accept_deprecated` (`21`) - `ssh:ssl_accept` is deprecated use `ssl:handshake instead` instead .


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
