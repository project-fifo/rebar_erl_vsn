rebar_erl_vsn
=====

defines for erlang versions

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar_erl_vsn, ".*", {git, "git@host:user/rebar_erl_vsn.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar_erl_vsn
    ===> Fetching rebar_erl_vsn
    ===> Compiling rebar_erl_vsn
    <Plugin Output>
