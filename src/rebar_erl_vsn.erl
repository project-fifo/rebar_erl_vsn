-module('rebar_erl_vsn').

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    io:format("~s:init/1\n", [?MODULE]),
    {ok, State1} = 'rebar_erl_vsn_prv':init(State),
    {ok, State1}.
