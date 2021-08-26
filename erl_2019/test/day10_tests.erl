-module(day10_tests).

-include_lib("eunit/include/eunit.hrl").

run_test_() ->
    [?_assertEqual({{3, 4}, 8}, day10:run("priv/day10_test1_input")),
     ?_assertEqual({{5, 8}, 33}, day10:run("priv/day10_test2_input")),
     ?_assertEqual({{1, 2}, 35}, day10:run("priv/day10_test3_input")),
     ?_assertEqual({{6, 3}, 41}, day10:run("priv/day10_test4_input")),
     ?_assertEqual({{11, 13}, 210}, day10:run("priv/day10_test5_input"))].
