-module(day1).

-export([fuel_for_mass/1, fuel_for_fuel/1]).

fuel_for_mass(Mass) ->
    round(Mass div 3) - 2.

fuel_for_fuel(InitFuel) ->
    Fuel = fuel_for_mass(InitFuel),
    do_fuel_for_fuel(Fuel, InitFuel).

do_fuel_for_fuel(Fuel, Total) when Fuel =< 0 ->
    Total;
do_fuel_for_fuel(Fuel, Total) ->
    RequiredFuel = fuel_for_mass(Fuel),
    do_fuel_for_fuel(RequiredFuel, Total + Fuel).
