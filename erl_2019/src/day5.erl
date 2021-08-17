-module(day5).

-export([intcode/1, intcode/2, run/0, run/1]).
-export([do_intcode/4]).

run() ->
    run(-1).

run(Input) ->
    {ok, [Intcode]} = file:consult("input"),
    intcode(Input, Intcode).

intcode(Intcode) ->
    intcode([], Intcode).

intcode(Input, Intcode) when is_integer(Input) ->
    intcode([Input], Intcode);
intcode(Input, Intcode) when is_list(Input) ->
    do_intcode(Input, {1, 1}, Intcode, []).

do_intcode(Input, {InstructionPointer, RelativeBase}, Intcode, Output)
    when is_list(Input) ->
    Instruction = lists:nth(InstructionPointer, Intcode),

    OpcodeAndModes = decode_instruction(Instruction),

    do_instruction(Input,
                   OpcodeAndModes,
                   {InstructionPointer, RelativeBase},
                   Intcode,
                   Output).

do_instruction(_Input, #{opcode := 99}, _, Intcode, Output) ->
    case Output of
        [] ->
            Intcode;
        [Digit] ->
            Digit;
        _ ->
            lists:reverse(Output)
    end;
do_instruction(Input,
               #{opcode := Opcode} = OpcodeAndModes,
               {InstructionPointer, RelativeBase},
               Intcode,
               Output)
    when Opcode == 1; Opcode == 2 ->
    #{first_param_mode := FirstParameterMode, second_param_mode := SecondParameterMode} =
        OpcodeAndModes,

    {ParameterValue1, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase}, FirstParameterMode, Intcode),
    {ParameterValue2, ExtendedIntcode2} =
        get_parameter_value({InstructionPointer + 2, RelativeBase},
                            SecondParameterMode,
                            ExtendedIntcode1),

    Result =
        case Opcode of
            1 ->
                ParameterValue1 + ParameterValue2;
            2 ->
                ParameterValue1 * ParameterValue2
        end,

    ResultAddress = lists:nth(InstructionPointer + 3, ExtendedIntcode2),
    NewIntcode =
        update_intcode_with_value_at_address(Result, ResultAddress, ExtendedIntcode2),
    do_intcode(Input, {InstructionPointer + 4, RelativeBase}, NewIntcode, Output);
do_instruction([], #{opcode := 3}, {InstructionPointer, RelativeBase}, Intcode, Output) ->
    {waiting_input, {InstructionPointer, RelativeBase}, Intcode, Output};
do_instruction([In | Tail],
               #{opcode := 3},
               {InstructionPointer, RelativeBase},
               Intcode,
               Output) ->
    InputAddress = lists:nth(InstructionPointer + 1, Intcode),

    NewIntcode = update_intcode_with_value_at_address(In, InputAddress, Intcode),
    do_intcode(Tail, {InstructionPointer + 2, RelativeBase}, NewIntcode, Output);
do_instruction(Input,
               #{opcode := 4} = OpcodeAndModes,
               {InstructionPointer, RelativeBase},
               Intcode,
               Output) ->
    #{first_param_mode := FirstParameterMode} = OpcodeAndModes,

    {FirstParameter, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase}, FirstParameterMode, Intcode),

    NewOutput = [FirstParameter | Output],

    do_intcode(Input, {InstructionPointer + 2, RelativeBase}, ExtendedIntcode1, NewOutput);
do_instruction(Input,
               #{opcode := Opcode} = OpcodeAndModes,
               {InstructionPointer, RelativeBase},
               Intcode,
               Output)
    when Opcode == 5; Opcode == 6 ->
    #{first_param_mode := FirstParameterMode, second_param_mode := SecondParameterMode} =
        OpcodeAndModes,

    {FirstParameter, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase}, FirstParameterMode, Intcode),

    Result =
        case Opcode of
            5 ->
                FirstParameter =/= 0;
            6 ->
                FirstParameter == 0
        end,

    case Result of
        true ->
            {SecondParameter, ExtendedIntcode2} =
                get_parameter_value({InstructionPointer + 2, RelativeBase},
                                    SecondParameterMode,
                                    ExtendedIntcode1),
            do_intcode(Input, {SecondParameter + 1, RelativeBase}, ExtendedIntcode2, Output);
        false ->
            do_intcode(Input, {InstructionPointer + 3, RelativeBase}, ExtendedIntcode1, Output)
    end;
do_instruction(Input,
               #{opcode := Opcode} = OpcodeAndModes,
               {InstructionPointer, RelativeBase},
               Intcode,
               Output)
    when Opcode == 7; Opcode == 8 ->
    #{first_param_mode := FirstParameterMode, second_param_mode := SecondParameterMode} =
        OpcodeAndModes,

    {ParameterValue1, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase}, FirstParameterMode, Intcode),
    {ParameterValue2, ExtendedIntcode2} =
        get_parameter_value({InstructionPointer + 2, RelativeBase},
                            SecondParameterMode,
                            ExtendedIntcode1),

    ResultAddress = lists:nth(InstructionPointer + 3, ExtendedIntcode2),

    Result =
        case Opcode of
            7 ->
                ParameterValue1 < ParameterValue2;
            8 ->
                ParameterValue1 == ParameterValue2
        end,

    NewIntcode =
        case Result of
            true ->
                update_intcode_with_value_at_address(1, ResultAddress, ExtendedIntcode2);
            false ->
                update_intcode_with_value_at_address(0, ResultAddress, ExtendedIntcode2)
        end,
    do_intcode(Input, {InstructionPointer + 4, RelativeBase}, NewIntcode, Output);
do_instruction(Input,
               #{opcode := 9} = OpcodeAndModes,
               {InstructionPointer, RelativeBase},
               Intcode,
               Output) ->
    #{first_param_mode := FirstParameterMode} = OpcodeAndModes,

    {ParameterValue1, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase}, FirstParameterMode, Intcode),

    NewRelativeBase = RelativeBase + ParameterValue1,
    do_intcode(Input, {InstructionPointer + 2, NewRelativeBase}, ExtendedIntcode1, Output).

update_intcode_with_value_at_address(NewValue, Address, Intcode) ->
    case Address >= length(Intcode) of
        true ->
            ExtendedIntcode = append_memory_for_address(Address, Intcode),
            lists:append(ExtendedIntcode, [NewValue]);
        false ->
            {Left, [_OldValue | Right]} = lists:split(Address, Intcode),
            lists:append([Left, [NewValue], Right])
    end.

decode_instruction(Instruction) when is_integer(Instruction) ->
    ThirdParameterDecoder = 10000,
    SecondParameterDecoder = 1000,
    FirstParameterDecoder = 100,

    ThirdParameterMode = Instruction div ThirdParameterDecoder,
    SecondParameterMode = Instruction rem ThirdParameterDecoder div SecondParameterDecoder,
    FirstParameterMode =
        Instruction rem ThirdParameterDecoder rem SecondParameterDecoder
        div FirstParameterDecoder,
    Opcode =
        Instruction
        rem ThirdParameterDecoder
        rem SecondParameterDecoder
        rem FirstParameterDecoder,

    #{opcode => Opcode,
      first_param_mode => FirstParameterMode,
      second_param_mode => SecondParameterMode,
      third_param_mode => ThirdParameterMode}.

get_parameter_value({Pointer, RelativeBase}, ParameterMode, Intcode) ->
    PositionMode = 0,
    ImmediateMode = 1,
    RelativeMode = 2,
    case ParameterMode of
        PositionMode ->
            Address1 = lists:nth(Pointer, Intcode),
            ProgramMemory = length(Intcode),
            case Address1 > ProgramMemory of
                true ->
                    ExtendedProgram = append_memory_for_address(Address1, Intcode),
                    {lists:nth(Address1 + 1, ExtendedProgram), ExtendedProgram};
                false ->
                    {lists:nth(Address1 + 1, Intcode), Intcode}
            end;
        ImmediateMode ->
            {lists:nth(Pointer, Intcode), Intcode};
        RelativeMode ->
            Address1 = lists:nth(Pointer, Intcode),
            {lists:nth(Address1 + RelativeBase, Intcode), Intcode}
    end.

append_memory_for_address(Address, Intcode) ->
    AdditionalMemory = lists:duplicate(Address + 1 - length(Intcode), 0),
    lists:append(Intcode, AdditionalMemory).
