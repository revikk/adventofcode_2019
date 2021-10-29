-module(day10_tests).

-include_lib("eunit/include/eunit.hrl").

run_test_() ->
    [?_assertMatch({{3, 4}, _, 8}, day10:run("priv/day10_test1_input")),
     ?_assertMatch({{5, 8}, _, 33}, day10:run("priv/day10_test2_input")),
     ?_assertMatch({{1, 2}, _, 35}, day10:run("priv/day10_test3_input")),
     ?_assertMatch({{6, 3}, _, 41}, day10:run("priv/day10_test4_input")),
     ?_assertMatch({{11, 13}, _, 210}, day10:run("priv/day10_test5_input"))].
