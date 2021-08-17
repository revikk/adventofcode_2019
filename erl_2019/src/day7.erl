-module(day7).

-export([run1/2, run2/2, permutation/1, get_first_star/0, get_second_star/0]).
-export([start/3]).

get_first_star() ->
    {ok, [Intcode]} = file:consult("priv/day7_input"),

    PhaseSettings = permutation([0, 1, 2, 3, 4]),

    ThrustersIn =
        lists:map(fun(PhaseSetting) -> run1(PhaseSetting, Intcode) end, PhaseSettings),

    lists:max(ThrustersIn).

get_second_star() ->
    {ok, [Intcode]} = file:consult("priv/day7_input"),

    PhaseSettings = permutation([5, 6, 7, 8, 9]),

    ThrustersIn =
        lists:map(fun(PhaseSetting) -> run2(PhaseSetting, Intcode) end, PhaseSettings),

    lists:max(ThrustersIn).

run2([A, B, C, D, E], Intcode) ->
    APid = spawn(?MODULE, start, [A, Intcode, self()]),
    BPid = spawn(?MODULE, start, [B, Intcode, self()]),
    CPid = spawn(?MODULE, start, [C, Intcode, self()]),
    DPid = spawn(?MODULE, start, [D, Intcode, self()]),
    EPid = spawn(?MODULE, start, [E, Intcode, self()]),

    APid ! {neighbor, BPid},
    BPid ! {neighbor, CPid},
    CPid ! {neighbor, DPid},
    DPid ! {neighbor, EPid},
    EPid ! {neighbor, APid},

    APid ! {input, [0]},

    await(EPid).

await(FinalAmp) ->
    receive
        {result, FinalAmp, Result} ->
            Result;
        _ ->
            await(FinalAmp)
    end.

start(Input, Intcode, Parent) ->
    case day5:intcode(Input, Intcode) of
        {waiting_input, InstructionPointer, NewIntcode, Output} ->
            loop(InstructionPointer, NewIntcode, Output, Parent, undefined);
        Result ->
            Parent ! {result, self(), Result}
    end.

loop(InstructionPointer, Intcode, Output, Parent, Neighbor) ->
    receive
        {neighbor, NewNeighbor} ->
            loop(InstructionPointer, Intcode, Output, Parent, NewNeighbor);
        {input, Input} ->
            case day5:do_intcode(Input, InstructionPointer, Intcode, Output) of
                {waiting_input, NewInstructionPointer, NewIntcode, NewOutput} ->
                    Neighbor ! {input, NewOutput},
                    loop(NewInstructionPointer, NewIntcode, [], Parent, Neighbor);
                Result ->
                    case erlang:is_process_alive(Neighbor) of
                        true ->
                            Neighbor ! {input, [Result]};
                        false ->
                            Parent ! {result, self(), Result}
                    end
            end
    end.

run1(PhasesSettings, Intcode) ->
    lists:foldl(fun(Phase, Input) -> day5:intcode([Phase, Input], Intcode) end,
                0,
                PhasesSettings).

permutation([]) ->
    [[]];
permutation(L) ->
    [[H | T] || H <- L, T <- permutation(L -- [H])].
