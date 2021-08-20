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
intcode(Input, Intcode) when is_list(Input), is_list(Intcode) ->
    Positions = lists:seq(0, length(Intcode) - 1),
    Intcode2 =
        maps:from_list(
            lists:zip(Positions, Intcode)),
    do_intcode(Input, {0, 0}, Intcode2, []).

do_intcode(Input, {InstructionPointer, RelativeBase}, Intcode, Output)
    when is_list(Input), is_map(Intcode) ->
    Instruction = maps:get(InstructionPointer, Intcode),

    OpcodeAndModes = decode_instruction(Instruction),

    do_instruction(Input,
                   OpcodeAndModes,
                   {InstructionPointer, RelativeBase},
                   Intcode,
                   Output).

do_instruction(_Input, #{opcode := 99}, _, Intcode, Output) ->
    case Output of
        [] ->
            {_, Values} =
                lists:unzip(
                    lists:keysort(1, maps:to_list(Intcode))),
            Values;
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
    #{first_param_mode := FirstParameterMode,
      second_param_mode := SecondParameterMode,
      third_param_mode := ThirdParamertMode} =
        OpcodeAndModes,

    {ParameterValue1, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase},
                            FirstParameterMode,
                            read,
                            Intcode),
    {ParameterValue2, ExtendedIntcode2} =
        get_parameter_value({InstructionPointer + 2, RelativeBase},
                            SecondParameterMode,
                            read,
                            ExtendedIntcode1),

    Result =
        case Opcode of
            1 ->
                ParameterValue1 + ParameterValue2;
            2 ->
                ParameterValue1 * ParameterValue2
        end,

    {ResultAddress, ExtendedIntcode3} =
        get_parameter_value({InstructionPointer + 3, RelativeBase},
                            ThirdParamertMode,
                            write,
                            ExtendedIntcode2),
    NewIntcode =
        update_intcode_with_value_at_address(Result, ResultAddress, ExtendedIntcode3),
    do_intcode(Input, {InstructionPointer + 4, RelativeBase}, NewIntcode, Output);
do_instruction([], #{opcode := 3}, {InstructionPointer, RelativeBase}, Intcode, Output) ->
    {waiting_input, {InstructionPointer, RelativeBase}, Intcode, Output};
do_instruction([In | Tail],
               #{opcode := 3} = OpcodeAndModes,
               {InstructionPointer, RelativeBase},
               Intcode,
               Output) ->
    #{first_param_mode := FirstParameterMode} = OpcodeAndModes,
    {InputAddress, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase},
                            FirstParameterMode,
                            write,
                            Intcode),

    NewIntcode = update_intcode_with_value_at_address(In, InputAddress, ExtendedIntcode1),
    do_intcode(Tail, {InstructionPointer + 2, RelativeBase}, NewIntcode, Output);
do_instruction(Input,
               #{opcode := 4} = OpcodeAndModes,
               {InstructionPointer, RelativeBase},
               Intcode,
               Output) ->
    #{first_param_mode := FirstParameterMode} = OpcodeAndModes,

    {FirstParameter, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase},
                            FirstParameterMode,
                            read,
                            Intcode),

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
        get_parameter_value({InstructionPointer + 1, RelativeBase},
                            FirstParameterMode,
                            read,
                            Intcode),

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
                                    read,
                                    ExtendedIntcode1),
            do_intcode(Input, {SecondParameter, RelativeBase}, ExtendedIntcode2, Output);
        false ->
            do_intcode(Input, {InstructionPointer + 3, RelativeBase}, ExtendedIntcode1, Output)
    end;
do_instruction(Input,
               #{opcode := Opcode} = OpcodeAndModes,
               {InstructionPointer, RelativeBase},
               Intcode,
               Output)
    when Opcode == 7; Opcode == 8 ->
    #{first_param_mode := FirstParameterMode,
      second_param_mode := SecondParameterMode,
      third_param_mode := ThirdParamertMode} =
        OpcodeAndModes,

    {ParameterValue1, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase},
                            FirstParameterMode,
                            read,
                            Intcode),
    {ParameterValue2, ExtendedIntcode2} =
        get_parameter_value({InstructionPointer + 2, RelativeBase},
                            SecondParameterMode,
                            read,
                            ExtendedIntcode1),
    {ResultAddress, ExtendedIntcode3} =
        get_parameter_value({InstructionPointer + 3, RelativeBase},
                            ThirdParamertMode,
                            write,
                            ExtendedIntcode2),

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
                update_intcode_with_value_at_address(1, ResultAddress, ExtendedIntcode3);
            false ->
                update_intcode_with_value_at_address(0, ResultAddress, ExtendedIntcode3)
        end,
    do_intcode(Input, {InstructionPointer + 4, RelativeBase}, NewIntcode, Output);
do_instruction(Input,
               #{opcode := 9} = OpcodeAndModes,
               {InstructionPointer, RelativeBase},
               Intcode,
               Output) ->
    #{first_param_mode := FirstParameterMode} = OpcodeAndModes,

    {ParameterValue1, ExtendedIntcode1} =
        get_parameter_value({InstructionPointer + 1, RelativeBase},
                            FirstParameterMode,
                            read,
                            Intcode),

    NewRelativeBase = RelativeBase + ParameterValue1,
    do_intcode(Input, {InstructionPointer + 2, NewRelativeBase}, ExtendedIntcode1, Output).

update_intcode_with_value_at_address(NewValue, Address, Intcode) ->
    maps:put(Address, NewValue, Intcode).

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

get_parameter_value({Pointer, RelativeBase}, ParameterMode, OperationMode, Intcode) ->
    PositionMode = 0,
    ImmediateMode = 1,
    RelativeMode = 2,
    case ParameterMode of
        PositionMode ->
            Address1 = maps:get(Pointer, Intcode),
            case OperationMode of
                read ->
                    get_value_at_address(Address1, Intcode);
                write ->
                    {Address1, Intcode}
            end;
        ImmediateMode ->
            {maps:get(Pointer, Intcode), Intcode};
        RelativeMode ->
            Offset = maps:get(Pointer, Intcode),
            Address1 = Offset + RelativeBase,
            case OperationMode of
                read ->
                    get_value_at_address(Address1, Intcode);
                write ->
                    {Address1, Intcode}
            end
    end.

get_value_at_address(Address, Intcode) ->
    case maps:is_key(Address, Intcode) of
        true ->
            {maps:get(Address, Intcode), Intcode};
        false ->
            {0, maps:put(Address, 0, Intcode)}
    end.
