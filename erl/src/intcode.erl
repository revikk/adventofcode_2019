-module(intcode).

-export([init/0, set/3, send/2]).
-export([loop/1]).

-type input() :: [] | [integer()].
-type output() :: [] | [integer()].
-type intcode() :: #{} | #{non_neg_integer() := integer()}.
-type instructionPointer() :: non_neg_integer().
-type relativeBase() :: non_neg_integer().
-type state_keys() ::
    input | output | intcode | instructionPointer | relativeBase | replyTo.
-type state() ::
    #{atom() := input() | output() | intcode() | instructionPointer() | relativeBase()}.
-type commands() :: run | get_state | terminate.
-type opcode() :: '+' | '*' | in | out | exit.

-spec init() -> pid().
init() ->
    spawn(?MODULE, loop, [new_state()]).

-spec send(MachineId :: pid(), Command :: commands()) -> any().
send(MachineId, Command) ->
    MachineId ! {Command, self()},
    receive
        {MachineId, Output} ->
            Output
    end.

-spec set(MachineId :: pid(), Key :: state_keys(), Value :: any()) ->
             {state_keys(), any()}.
set(MachineId, intcode, {file, IntcodePath}) ->
    {ok, [Intcode]} = file:consult(IntcodePath),
    set(MachineId, intcode, {list, Intcode});
set(MachineId, intcode, {list, Intcode}) ->
    Indexes = lists:seq(0, length(Intcode) - 1),
    IndexedIntcode =
        maps:from_list(
            lists:zip(Indexes, Intcode)),

    MachineId ! {intcode, IndexedIntcode}.

-spec loop(State :: state()) -> function() | {pid(), ok}.
loop(State) ->
    receive
        {intcode, Intcode} ->
            loop(new_state(Intcode));
        {run, From} ->
            NewState = do_instruction(State),
            loop(NewState#{replyTo => From});
        done ->
            #{replyTo := ReplyTo, intcode := Intcode} = State,
            {_Indexes, Reply} =
                lists:unzip(
                    lists:keysort(1, maps:to_list(Intcode))),
            ReplyTo ! {self(), Reply},
            loop(new_state());
        {get_state, From} ->
            From ! {self(), State},
            loop(State);
        {terminate, From} ->
            From ! {self(), ok}
    end.

-spec do_instruction(State :: state()) -> state().
do_instruction(State) ->
    #{instructionPointer := InstructionPointer, intcode := Intcode} = State,
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

            NewState =
                State#{instructionPointer => InstructionPointer + 4,
                       intcode => Intcode#{ResultAddress => Result}},

            do_instruction(NewState);
        3 ->
            #{InstructionPointer + 1 := Address} = Intcode,
            #{input := [I | Input]} = State,
            NewIntcode = Intcode#{Address => I},

            do_instruction(State#{input => Input,
                                  instructionPointer => InstructionPointer + 2,
                                  intcode => NewIntcode});
        4 ->
            #{InstructionPointer + 1 := Address} = Intcode,
            #{Address := Value} = Intcode,
            #{output := Output} = State,
            do_instruction(State#{instructionPointer => InstructionPointer + 2,
                                  output => [Value | Output]});
        99 ->
            self() ! done,
            State
    end.

-spec new_state() -> state().
new_state() ->
    new_state(#{}).

-spec new_state(Intcode :: intcode()) -> state().
new_state(Intcode) ->
    #{input => [],
      output => [],
      intcode => Intcode,
      instructionPointer => 0,
      relativeBase => 0}.
