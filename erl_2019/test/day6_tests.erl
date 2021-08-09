-module(day6_tests).

-include_lib("eunit/include/eunit.hrl").

run_test_() ->
    [?_assertEqual(42,
                   day6:get_number_of_paths(
                       day6:build_map("priv/day6_test1_input"))),
     ?_assertEqual(4,
                   day6:get_number_of_transfers(
                       day6:build_map("priv/day6_test2_input")))].
