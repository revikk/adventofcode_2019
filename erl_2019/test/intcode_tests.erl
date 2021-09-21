-module(intcode_tests).

-include_lib("eunit/include/eunit.hrl").

intcode_test_() ->
    {setup, fun start/0, fun stop/1, fun examples/1}.

start() ->
    intcode:init().

stop(MachineId) ->
    intcode:send(MachineId, terminate).

examples(MachineId) ->
    Day2Tests =
        [{[3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50],
          [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]},
         {[2, 0, 0, 0, 99], [1, 0, 0, 0, 99]},
         {[2, 3, 0, 6, 99], [2, 3, 0, 3, 99]},
         {[2, 4, 4, 5, 99, 9801], [2, 4, 4, 5, 99, 0]},
         {[30, 1, 1, 4, 2, 5, 6, 0, 99], [1, 1, 1, 4, 99, 5, 6, 0, 99]}],
    lists:foldl(fun({Output, Input}, Acc) ->
                   [?_assertMatch({intcode, _Intcode},
                                  intcode:set(MachineId, intcode, {list, Input})),
                    ?_assertEqual(Output, intcode:send(MachineId, run))
                    | Acc]
                end,
                [],
                Day2Tests).
