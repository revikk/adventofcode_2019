-module(day2).

-export([intcode/1]).

intcode(Intcode) ->
    do_intcode(1, Intcode).

do_intcode(Position, Intcode) ->
    Opcode = lists:nth(Position, Intcode),
    do_opcode(Opcode, Position, Intcode).

do_opcode(99, _, Intcode) ->
    Intcode;
do_opcode(1, Position, Intcode) ->
    From1 = lists:nth(Position + 1, Intcode),
    Value1 = lists:nth(From1 + 1, Intcode),
    From2 = lists:nth(Position + 2, Intcode),
    Value2 = lists:nth(From2 + 1, Intcode),
    Sum = Value1 + Value2,

    SumPosition = lists:nth(Position + 3, Intcode),
    {Left, [_ForSumValue | Right]} = lists:split(SumPosition, Intcode),
    NewIntcode = lists:append([Left, [Sum], Right]),
    do_intcode(Position + 4, NewIntcode);
do_opcode(2, Position, Intcode) ->
    From1 = lists:nth(Position + 1, Intcode),
    Value1 = lists:nth(From1 + 1, Intcode),
    From2 = lists:nth(Position + 2, Intcode),
    Value2 = lists:nth(From2 + 1, Intcode),
    Multi = Value1 * Value2,

    MultiPosition = lists:nth(Position + 3, Intcode),
    {Left, [_ForMultiValue | Right]} = lists:split(MultiPosition, Intcode),
    NewIntcode = lists:append([Left, [Multi], Right]),
    do_intcode(Position + 4, NewIntcode).
