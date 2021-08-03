-module(day5).

-export([intcode/1, run/0]).

run() ->
    {ok, [Intcode]} = file:consult("input"),
    intcode(Intcode).

intcode(Intcode) -> do_intcode(1, Intcode).

do_intcode(InstructionPointer, Intcode) ->
    Opcode = lists:nth(InstructionPointer, Intcode),
    do_instruction(Opcode, InstructionPointer, Intcode).

do_instruction(99, _, Intcode) -> Intcode;
do_instruction(1, InstructionPointer, Intcode) ->
    Address1 = lists:nth(InstructionPointer + 1, Intcode),
    Parameter1 = lists:nth(Address1 + 1, Intcode),
    Address2 = lists:nth(InstructionPointer + 2, Intcode),
    Parameter2 = lists:nth(Address2 + 1, Intcode),

    Sum = Parameter1 + Parameter2,

    SumAddress = lists:nth(InstructionPointer + 3, Intcode),
    NewIntcode = update_intcode_with_value_at_address(Sum,
                                                      SumAddress,
                                                      Intcode),
    do_intcode(InstructionPointer + 4, NewIntcode);
do_instruction(2, InstructionPointer, Intcode) ->
    Address1 = lists:nth(InstructionPointer + 1, Intcode),
    Parameter1 = lists:nth(Address1 + 1, Intcode),
    Address2 = lists:nth(InstructionPointer + 2, Intcode),
    Parameter2 = lists:nth(Address2 + 1, Intcode),

    Multi = Parameter1 * Parameter2,

    MultiAddress = lists:nth(InstructionPointer + 3,
                             Intcode),
    NewIntcode = update_intcode_with_value_at_address(Multi,
                                                      MultiAddress,
                                                      Intcode),
    do_intcode(InstructionPointer + 4, NewIntcode);
do_instruction(3, InstructionPointer, Intcode) ->
    InputAddress = lists:nth(InstructionPointer + 1,
                             Intcode),
    NewValue = unit_id(air_conditioner),

    NewIntcode =
        update_intcode_with_value_at_address(NewValue,
                                             InputAddress,
                                             Intcode),
    do_intcode(InstructionPointer + 2, NewIntcode);
do_instruction(4, InstructionPointer, Intcode) ->
    OutputAddress = lists:nth(InstructionPointer + 1,
                              Intcode),
    OutputValue = lists:nth(OutputAddress, Intcode),

    io:format("Output ~p~n", [OutputValue]),

    do_intcode(InstructionPointer + 2, Intcode).

update_intcode_with_value_at_address(NewValue, Address,
                                     Intcode) ->
    {Left, [_OldValue | Right]} = lists:split(Address,
                                              Intcode),
    lists:append([Left, [NewValue], Right]).

unit_id(air_conditioner) -> 1.
