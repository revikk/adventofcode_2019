-module(day2_tests).

-include_lib("eunit/include/eunit.hrl").

intcode_test_() ->
    [
        ?_assertEqual([3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50], day2:intcode([1,9,10,3,2,3,11,0,99,30,40,50])),
        ?_assertEqual([2, 0, 0, 0, 99], day2:intcode([1, 0, 0, 0, 99])),
        ?_assertEqual([2, 3, 0, 6, 99], day2:intcode([2, 3, 0, 3, 99])),
        ?_assertEqual([2, 4, 4, 5, 99, 9801], day2:intcode([2, 4, 4, 5, 99, 0])),
        ?_assertEqual([30,1,1,4,2,5,6,0,99], day2:intcode([1,1,1,4,99,5,6,0,99]))
    ].