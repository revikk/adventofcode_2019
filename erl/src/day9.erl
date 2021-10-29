-module(day9).

-export([run/1, get_first_star/0, get_second_star/0]).

get_first_star() ->
    {ok, [Intcode]} = file:consult("priv/day9_input"),
    day5:intcode(1, Intcode).

get_second_star() ->
    {ok, [Intcode]} = file:consult("priv/day9_input"),
    day5:intcode(2, Intcode).

run(Intcode) ->
    day5:intcode(Intcode).
