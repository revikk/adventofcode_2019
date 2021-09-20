-module(intcode).

-export([init/0, set/2, run/0]).
-export([loop/1]).

-define(INTCODE_MACHINE, intcode_machine).

-type input() :: [] | [integer()].
-type output() :: [] | [integer()].
-type intcode() :: #{} | #{non_neg_integer() := integer()}.
-type instructionPointer() :: non_neg_integer().
-type relativeBase() :: non_neg_integer().
-type state_keys() ::
    input | output | intcode | instructionPointer | relativeBase | replyTo.
-type state() ::
    #{atom() := input() | output() | intcode() | instructionPointer() | relativeBase()}.

-spec init() -> true.
init() ->
    InitState = new_state(),
    MachineId = spawn(?MODULE, loop, [InitState]),
    register(?INTCODE_MACHINE, MachineId).

-spec run() -> output().
run() ->
    ?INTCODE_MACHINE ! {run, self()},
    MachineId = whereis(?INTCODE_MACHINE),
    receive
        {MachineId, Output} ->
            Output
    end.

-spec set(Key :: state_keys(), Value :: any()) -> {state_keys(), any()}.
set(intcode, IntcodePath) ->
    {ok, [Intcode]} = file:consult(IntcodePath),

    Indexes = lists:seq(0, length(Intcode) - 1),
    IndexedIntcode =
        maps:from_list(
            lists:zip(Indexes, Intcode)),

    ?INTCODE_MACHINE ! {intcode, IndexedIntcode};
set(Key, Value) ->
    ?INTCODE_MACHINE ! {Key, Value}.

-spec loop(State :: state()) -> done | function().
loop(State) ->
    receive
        {intcode, Intcode} ->
            NewState = State#{intcode => Intcode},
            loop(NewState);
        {run, From} ->
            #{instructionPointer := InstructionPointer, intcode := Intcode} = State,
            {NewInstructionPointer, NewIntcode} = do_instruction(InstructionPointer, Intcode),
            NewState =
                State#{replyTo => From,
                       intcode => NewIntcode,
                       instructionPointer => NewInstructionPointer},
            loop(NewState);
        halt ->
            #{replyTo := ReplyTo, intcode := Intcode} = State,
            {_Indexes, Reply} =
                lists:unzip(
                    lists:keysort(1, maps:to_list(Intcode))),
            ReplyTo ! {self(), Reply},
            done
    end.

-spec do_instruction(InstructionPointer :: instructionPointer(), Intcode :: intcode()) ->
                        {instructionPointer(), intcode()}.
do_instruction(InstructionPointer, Intcode) ->
    #{InstructionPointer := Opcode} = Intcode,

    case Opcode of
        OneOrTwo when OneOrTwo == 1; OneOrTwo == 2 ->
            #{InstructionPointer + 1 := Address1,
              InstructionPointer + 2 := Address2,
              InstructionPointer + 3 := ResultAddress} =
                Intcode,
            #{Address1 := Value1, Address2 := Value2} = Intcode,

            Result =
                case OneOrTwo of
                    1 ->
                        Value1 + Value2;
                    2 ->
                        Value1 * Value2
                end,

            do_instruction(InstructionPointer + 4, Intcode#{ResultAddress => Result});
        99 ->
            self() ! halt,
            {InstructionPointer, Intcode}
    end.

-spec new_state() -> state().
new_state() ->
    maps:from_list(init_values()).

-spec init_values() -> [{state_keys(), any()}].
init_values() ->
    [{input, []}, {output, []}, {intcode, #{}}, {instructionPointer, 0}, {relativeBase, 0}].
