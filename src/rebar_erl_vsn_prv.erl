-module('rebar_erl_vsn_prv').

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'rebar_erl_vsn').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 rebar_erl_vsn"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "defines for erlang versions"},
            {desc, "defines for erlang versions"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    io:format("Vsn: ~p", [versions()]),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


versions() ->
    Vsn = case erlang:system_info(otp_release) of
              "R" ++ V ->
                  extract_numbers(V, "");
              V ->
                  V
          end,
    list_to_integer(Vsn).

extract_numbers([H | T], Acc) when H >= $0,
                                   H =< $9 ->
    Acc;
extract_numbers(_, Acc) ->
    Acc.
