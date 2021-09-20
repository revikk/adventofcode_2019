-module(intcode_tests).

-include_lib("eunit/include/eunit.hrl").

intcode_test_() ->
    {setup, fun start/0, fun examples/1}.

start() ->
    intcode:init().

examples(true) ->
    % day2
    intcode:set(intcode, "priv/day2_test1_input"),
    [?_assertEqual([3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50], intcode:run())].
