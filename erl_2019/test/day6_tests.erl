-module(day6_tests).

-include_lib("eunit/include/eunit.hrl").

run_test_() ->
    [?_assertEqual(42, day6:run("priv/day6_test_input"))].
