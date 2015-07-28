-module('rebar_erl_vsn_prv').

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'erl_vsn').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, false},                % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 rebar_erl_vsn"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {hooks, {[compile], []}},
            {short_desc, "defines for erlang versions"},
            {desc, "defines for erlang versions"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Vsns = enumerate(versions()),
    io:format("Vsns: ~p~n", [Vsns]),
    Opts = rebar_state:get(State, erl_opts),
    io:format("Opts: ~p~n", [Opts]),
    Opts1 = Vsns ++ Opts,
    io:format("Opts1: ~p~n", [Opts1]),
    State1 = rebar_state:set(State, erl_opts, Opts1),
    {ok, State1}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


versions() ->
    Vsn = rebar_api:get_arch(),
    Vsn1 = extract_version(Vsn, []),
    to_vsn(Vsn1, []).

enumerate(V) ->
    io:format("V: ~p~n", [V]),
    enumerate(V, []).

enumerate({18, 0}, Acc) ->
    enumerate({17, 5}, [{d, '18.0'} | Acc]);
enumerate({17, 0}, Acc) ->
    enumerate({16, 3}, [{d, '17.0'} | Acc]);
enumerate({16, 0}, Acc) ->
    enumerate({15, 3}, [{d, '16.0'} | Acc]);
enumerate({15, 0}, Acc) ->
    enumerate({14, 4}, [{d, '15.0'} | Acc]);
enumerate({14, 0}, Acc) ->
    [{d, "14.0"} | Acc];
enumerate({Maj, Min}, Acc) when Maj >= 14, Maj =< 18, Min > 0 ->
    V = list_to_atom(lists:flatten(io_lib:format("~p.~p", [Maj, Min]))),
    enumerate({Maj, Min - 1}, [{d, V} | Acc]).


extract_version([H | T], Acc) when H =/= $- ->
    extract_version(T, [H | Acc]);
extract_version(_, Acc) ->
    lists:reverse(Acc).

to_vsn([H | T], Acc) when H =:= $. ->
    {list_to_integer(Acc), list_to_integer(T)};
to_vsn([H | T], Acc) ->
    to_vsn(T, Acc ++ [H]).
