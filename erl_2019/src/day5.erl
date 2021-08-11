-module(day5).

-export([intcode/1, intcode/2, run/0, run/1]).
-export([do_intcode/4]).

run() ->
    run(-1).

run(Input) ->
    {ok, [Intcode]} = file:consult("input"),
    intcode(Input, Intcode).

intcode(Intcode) ->
    intcode(-1, Intcode).

intcode(Input, Intcode) when is_integer(Input) ->
    do_intcode([Input], 1, Intcode, -1);
intcode(Input, Intcode) when is_list(Input) ->
    do_intcode(Input, 1, Intcode, -1).

do_intcode(Input, InstructionPointer, Intcode, Output) when is_list(Input) ->
    Instruction = lists:nth(InstructionPointer, Intcode),

    OpcodeAndModes = decode_instruction(Instruction),

    do_instruction(Input, OpcodeAndModes, InstructionPointer, Intcode, Output).

do_instruction(_Input, #{opcode := 99}, _, Intcode, Output) ->
    case Output of
        -1 ->
            Intcode;
        _ ->
            Output
    end;
do_instruction(Input,
               #{opcode := Opcode} = OpcodeAndModes,
               InstructionPointer,
               Intcode,
               Output)
    when Opcode == 1; Opcode == 2 ->
    #{first_param_mode := FirstParameterMode, second_param_mode := SecondParameterMode} =
        OpcodeAndModes,

    ParameterValue1 =
        get_parameter_value(InstructionPointer + 1, FirstParameterMode, Intcode),
    ParameterValue2 =
        get_parameter_value(InstructionPointer + 2, SecondParameterMode, Intcode),

    Result =
        case Opcode of
            1 ->
                ParameterValue1 + ParameterValue2;
            2 ->
                ParameterValue1 * ParameterValue2
        end,

    ResultAddress = lists:nth(InstructionPointer + 3, Intcode),
    NewIntcode = update_intcode_with_value_at_address(Result, ResultAddress, Intcode),
    do_intcode(Input, InstructionPointer + 4, NewIntcode, Output);
do_instruction([], #{opcode := 3}, InstructionPointer, Intcode, Output) ->
    {waiting_input, InstructionPointer, Intcode, Output};
do_instruction([In | Tail], #{opcode := 3}, InstructionPointer, Intcode, Output) ->
    InputAddress = lists:nth(InstructionPointer + 1, Intcode),

    NewIntcode = update_intcode_with_value_at_address(In, InputAddress, Intcode),
    do_intcode(Tail, InstructionPointer + 2, NewIntcode, Output);
do_instruction(Input,
               #{opcode := 4} = OpcodeAndModes,
               InstructionPointer,
               Intcode,
               _Output) ->
    #{first_param_mode := FirstParameterMode} = OpcodeAndModes,

    FirstParameter = get_parameter_value(InstructionPointer + 1, FirstParameterMode, Intcode),

    do_intcode(Input, InstructionPointer + 2, Intcode, FirstParameter);
do_instruction(Input,
               #{opcode := Opcode} = OpcodeAndModes,
               InstructionPointer,
               Intcode,
               Output)
    when Opcode == 5; Opcode == 6 ->
    #{first_param_mode := FirstParameterMode, second_param_mode := SecondParameterMode} =
        OpcodeAndModes,

    FirstParameter = get_parameter_value(InstructionPointer + 1, FirstParameterMode, Intcode),

    Result =
        case Opcode of
            5 ->
                FirstParameter =/= 0;
            6 ->
                FirstParameter == 0
        end,

    case Result of
        true ->
            SecondParameter =
                get_parameter_value(InstructionPointer + 2, SecondParameterMode, Intcode),
            do_intcode(Input, SecondParameter + 1, Intcode, Output);
        false ->
            do_intcode(Input, InstructionPointer + 3, Intcode, Output)
    end;
do_instruction(Input,
               #{opcode := Opcode} = OpcodeAndModes,
               InstructionPointer,
               Intcode,
               Output)
    when Opcode == 7; Opcode == 8 ->
    #{first_param_mode := FirstParameterMode, second_param_mode := SecondParameterMode} =
        OpcodeAndModes,

    ParameterValue1 =
        get_parameter_value(InstructionPointer + 1, FirstParameterMode, Intcode),
    ParameterValue2 =
        get_parameter_value(InstructionPointer + 2, SecondParameterMode, Intcode),

    ResultAddress = lists:nth(InstructionPointer + 3, Intcode),

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
                update_intcode_with_value_at_address(1, ResultAddress, Intcode);
            false ->
                update_intcode_with_value_at_address(0, ResultAddress, Intcode)
        end,
    do_intcode(Input, InstructionPointer + 4, NewIntcode, Output).

update_intcode_with_value_at_address(NewValue, Address, Intcode) ->
    {Left, [_OldValue | Right]} = lists:split(Address, Intcode),
    lists:append([Left, [NewValue], Right]).

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

get_parameter_value(ParameterAddress, ParameterMode, Intcode) ->
    PositionMode = 0,
    ImmediateMode = 1,
    case ParameterMode of
        PositionMode ->
            Address1 = lists:nth(ParameterAddress, Intcode),
            lists:nth(Address1 + 1, Intcode);
        ImmediateMode ->
            lists:nth(ParameterAddress, Intcode)
    end.
