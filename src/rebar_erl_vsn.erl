-module('rebar_erl_vsn').

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar_erl_vsn_prv:init(State),
    {ok, State1}.
