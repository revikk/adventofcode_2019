-module(day5).

-export([run/0, intcode/1]).

run() ->
    {ok, [Intcode]} = file:consult("input"),
    intcode(Intcode).

intcode(Intcode) ->
    do_intcode(1, Intcode).

do_intcode(InstructionPointer, Intcode) ->
    Opcode = lists:nth(InstructionPointer, Intcode),
    do_instruction(Opcode, InstructionPointer, Intcode).

do_instruction(99, _, Intcode) ->
    Intcode;
do_instruction(1, InstructionPointer, Intcode) ->
    Address1 = lists:nth(InstructionPointer + 1, Intcode),
    Parameter1 = lists:nth(Address1 + 1, Intcode),
    Address2 = lists:nth(InstructionPointer + 2, Intcode),
    Parameter2 = lists:nth(Address2 + 1, Intcode),
    Sum = Parameter1 + Parameter2,

    SumAddress = lists:nth(InstructionPointer + 3, Intcode),
    {Left, [_ForSumValue | Right]} = lists:split(SumAddress, Intcode),
    NewIntcode = lists:append([Left, [Sum], Right]),
    do_intcode(InstructionPointer + 4, NewIntcode);
do_instruction(2, InstructionPointer, Intcode) ->
    Address1 = lists:nth(InstructionPointer + 1, Intcode),
    Parameter1 = lists:nth(Address1 + 1, Intcode),
    Address2 = lists:nth(InstructionPointer + 2, Intcode),
    Parameter2 = lists:nth(Address2 + 1, Intcode),
    Multi = Parameter1 * Parameter2,

    MultiAddress = lists:nth(InstructionPointer + 3, Intcode),
    {Left, [_ForMultiValue | Right]} = lists:split(MultiAddress, Intcode),
    NewIntcode = lists:append([Left, [Multi], Right]),
    do_intcode(InstructionPointer + 4, NewIntcode).
