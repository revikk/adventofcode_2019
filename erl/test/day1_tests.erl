-module(day1_tests).

-include_lib("eunit/include/eunit.hrl").

fuel_for_mass_test_() ->
    [?_assertEqual(2, day1:fuel_for_mass(12)),
     ?_assertEqual(2, day1:fuel_for_mass(14)),
     ?_assertEqual(654, day1:fuel_for_mass(1969)),
     ?_assertEqual(33583, day1:fuel_for_mass(100756))].

fuel_for_fuel_test_() ->
    [?_assertEqual(2, day1:fuel_for_fuel(2)),
     ?_assertEqual(966, day1:fuel_for_fuel(654)),
     ?_assertEqual(50346, day1:fuel_for_fuel(33583))].
