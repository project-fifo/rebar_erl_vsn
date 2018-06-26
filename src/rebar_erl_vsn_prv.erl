-module('rebar_erl_vsn_prv').

-export([init/1, do/1, format_error/1]).

-behaviour(provider).

-define(PROVIDER, 'erl_vsn').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create(
          [{name, ?PROVIDER},            % The 'user friendly' name of the task
           {module, ?MODULE},            % The module implementation of the task
           {bare, true},
           {deps, ?DEPS}                 % The list of dependencies
           ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Vsns = add_events(enumerate(version())),
    AppInfo = rebar_state:current_app(State),
    ErlOpts = rebar_app_info:get(AppInfo, erl_opts, []),
    AppInfo1 = rebar_app_info:set(AppInfo, erl_opts, Vsns ++ ErlOpts),
    %%io:format("vsns: ~p~n", [Vsns]),
    State1 = rebar_state:current_app(State, AppInfo1),
    {ok, State1}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


version() ->
    parse_vsn(rebar_api:get_arch()).

parse_vsn([$R, Maj1, Maj2, $B, Min1, Min2 | _]) ->
    {list_to_integer([Maj1, Maj2]), list_to_integer([Min1, Min2])};
parse_vsn(Vsn) ->
    [Vsn1 | _] = re:split(Vsn, "-"),
    [Maj, Min | _ ] = re:split(Vsn1, "\\."),
    {binary_to_integer(Maj), binary_to_integer(Min)}.

enumerate(V) ->
    enumerate(V, []).

-define(HEAD_VSN, 21).

-spec enumerate({pos_integer(), non_neg_integer()}, [{d, atom()}]) -> [{d, atom()}].

enumerate({V, VSub}, Acc) when V > ?HEAD_VSN orelse
                               (V =:= ?HEAD_VSN andalso VSub > 0) ->
    rebar_api:warn("The erlang version ~p.~p is newer then the latest version "
                   "known to rebar_erl_vsn (~p). Features introduced between "
                   "after ~p will not have flags.", [V, VSub, ?HEAD_VSN, ?HEAD_VSN]),
    enumerate({?HEAD_VSN, 0}, Acc);
enumerate({?HEAD_VSN, 0}, Acc) ->
    enumerate({20, 0}, [{d, '21.0'} | Acc]);
enumerate({20, 0}, Acc) ->
    enumerate({19, 3}, [{d, '20.0'} | Acc]);
enumerate({19, 0}, Acc) ->
    enumerate({18, 3}, [{d, '19.0'} | Acc]);
enumerate({18, 0}, Acc) ->
    enumerate({17, 5}, [{d, '18.0'} | Acc]);
enumerate({17, 0}, Acc) ->
    enumerate({16, 3}, [{d, '17.0'} | Acc]);
enumerate({16, 0}, Acc) ->
    enumerate({15, 3}, [{d, '16.0'} | Acc]);
enumerate({15, 0}, Acc) ->
    enumerate({14, 4}, [{d, '15.0'} | Acc]);
enumerate({14, 0}, Acc) ->
    [{d, '14.0'} | Acc];
enumerate({Maj, Min}, Acc) when Maj >= 14, Maj =< ?HEAD_VSN, Min > 0 ->
    V = list_to_atom(lists:flatten(io_lib:format("~p.~p", [Maj, Min]))),
    enumerate({Maj, Min - 1}, [{d, V} | Acc]).

add_events([]) ->
    [];
add_events([V = {d, '21.0'} | R]) ->
    [V, {d, 'ssl_accept_deprecated'} | add_events(R)];
add_events([V = {d, '20.0'} | R]) ->
    [V, {d, 'ceil_bif'}, {d, 'high_bit_uniform'}, {d, 'fsm_deprecated'} | add_events(R)];
add_events([V = {d, '19.0'} | R]) ->
    [V, {d, 'rand'} | add_events(R)];
add_events([V = {d, '18.0'} | R]) ->
    [V, {d, 'large_maps'} | add_events(R)];
add_events([V = {d, '17.0'} | R]) ->
    [V, {d, 'maps'}, {d, 'namespaced_types'} | add_events(R)];
add_events([V = {d, '16.1'} | R]) ->
    [V, {d, 'long_schedule'} | add_events(R)];
add_events([V = {d, '16.0'} | R]) ->
    [V, {d, 'new_hash'} | add_events(R)];
add_events([V | R]) ->
    [V | add_events(R)].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
enum_test() ->
    [begin
         ?assert(enumerate({Major, Minor}, []) =/= [])
     end || Major <- lists:seq(14, ?HEAD_VSN + 1), Minor <- [1, 2, 3]].

vsn_test() ->
    ?assert(is_tuple(version())).

parse_test() ->
    ?assert(is_tuple(parse_vsn("21.0-rc0-x86_64-unknown-linux-gnu-64"))),
    ?assert(is_tuple(parse_vsn("19.3.6.4-x86_64-apple-darwin17.3.0-64"))).

-endif.
