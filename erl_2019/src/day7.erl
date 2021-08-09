-module(day7).

-export([run/2, permutation/1, get_star/0]).

get_star() ->
    {ok, [Intcode]} = file:consult("priv/day7_input"),

    PhaseSettings = permutation([0, 1, 2, 3, 4]),

    ThrustersIn =
        lists:map(fun(PhaseSetting) -> run(PhaseSetting, Intcode) end, PhaseSettings),

    lists:max(ThrustersIn).

run(PhasesSettings, Intcode) ->
    lists:foldl(fun(Phase, Input) -> day5:intcode([Phase, Input], Intcode) end,
                0,
                PhasesSettings).

permutation([]) ->
    [[]];
permutation(L) ->
    [[H | T] || H <- L, T <- permutation(L -- [H])].
