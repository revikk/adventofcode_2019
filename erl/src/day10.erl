-module(day10).

-export([run/1, get_first_star/0, get_second_star/0]).

get_first_star() ->
    run("priv/day10_input").

get_second_star() ->
    {{XPoint, YPoint}, Offs, _LenOffs} = run("priv/day10_input"),
    [_As, _Bs, _Cs, Ds] =
        lists:foldl(fun ({X, Y} = P, [A, B, C, D]) when (X >= 0) and (Y < 0) ->
                            [[P | A], B, C, D];
                        ({X, Y} = P, [A, B, C, D]) when (X >= 0) and (Y >= 0) ->
                            [A, [P | B], C, D];
                        ({X, Y} = P, [A, B, C, D]) when (X < 0) and (Y >= 0) ->
                            [A, B, [P | C], D];
                        ({X, Y} = P, [A, B, C, D]) when (X < 0) and (Y < 0) ->
                            [A, B, C, [{P, Y / X} | D]]
                    end,
                    [[], [], [], []],
                    Offs),
    % the length of nearest asteroids in each qudrant is {61,12,41,189}
    % the 200th is in the 4th one. 61 + 12 + 41 = 114; 200 - 114 = 86
    % so need to take the 86th from the 4th quadrant.
    {{XOff, YOff}, _} = lists:nth(86, lists:keysort(2, Ds)),
    (XPoint + XOff) * 100 + YPoint + YOff.

run(MapPath) ->
    Coordinates = decode_map(MapPath),
    Of = lists:map(fun(C) -> offsets(C, Coordinates -- [C]) end, Coordinates),
    A = lists:map(fun({P, Of1}) ->
                     {NegH, PosH} = same_horizontale(Of1),
                     {NegV, PosV} = same_vertical(Of1 -- NegH ++ PosH),
                     Ds = same_diagonal(Of1 -- NegH ++ PosH ++ NegV ++ PosV),
                     {P,
                      lists:flatten(nearest_in_vertical(sort(NegV))
                                    ++ nearest_in_horizontale(sort(PosH))
                                    ++ nearest_in_vertical(sort(PosV))
                                    ++ nearest_in_horizontale(sort(NegH))
                                    ++ remove_duplicates(lists:flatmap(fun(D) ->
                                                                          nearest_in_diagonale(sort(D))
                                                                       end,
                                                                       Ds)))}
                  end,
                  Of),

    {P, S} =
        lists:foldl(fun({_, Sight1} = Q, {_, Sight2} = Acc) ->
                       case length(Sight1) > length(Sight2) of
                           true -> Q;
                           false -> Acc
                       end
                    end,
                    hd(A),
                    tl(A)),

    {P, S, length(S)}.

remove_duplicates(Line) ->
    lists:foldl(fun(Point, Acc) ->
                   case lists:member(Point, Acc) of
                       true -> Acc;
                       false -> [Point | Acc]
                   end
                end,
                [],
                Line).

nearest_in_vertical(Line) ->
    MaxFromNegative =
        lists:foldl(fun ({0, Y1} = P, []) when Y1 < 0 ->
                            [P];
                        ({0, Y1} = P, [{_X2, Y2}] = Acc) when Y1 < 0 ->
                            if Y1 > Y2 ->
                                   [P];
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    Line),
    MinFromPositive =
        lists:foldl(fun ({0, Y1} = P, []) when Y1 > 0 ->
                            [P];
                        ({0, Y1} = P, [{_X2, Y2}] = Acc) when Y1 > 0 ->
                            if Y1 < Y2 ->
                                   [P];
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    Line),

    [MaxFromNegative, MinFromPositive].

nearest_in_horizontale(Line) ->
    MaxFromNegative =
        lists:foldl(fun ({X1, 0} = P, []) when X1 < 0 ->
                            [P];
                        ({X1, 0} = P, [{X2, _}] = Acc) when X1 < 0 ->
                            if X1 > X2 ->
                                   [P];
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    Line),
    MinFromPositive =
        lists:foldl(fun ({X1, 0} = P, []) when X1 > 0 ->
                            [P];
                        ({X1, 0} = P, [{X2, _Y2}] = Acc) when X1 > 0 ->
                            if X1 < X2 ->
                                   [P];
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    Line),
    [MaxFromNegative, MinFromPositive].

nearest_in_diagonale(Line) ->
    %  First   | Third
    % --------+-------
    % Second | Fourth
    First =
        lists:foldl(fun ({X1, Y1} = P, []) when (X1 < 0) and (Y1 < 0) ->
                            [P];
                        ({X1, Y1} = P, [{X2, Y2}] = Acc) when (X1 < 0) and (Y1 < 0) ->
                            if (X1 > X2) and (Y1 > Y2) ->
                                   [P];
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    Line),
    Second =
        lists:foldl(fun ({X1, Y1} = P, []) when (X1 < 0) and (Y1 > 0) ->
                            [P];
                        ({X1, Y1} = P, [{X2, Y2}] = Acc) when (X1 < 0) and (Y1 > 0) ->
                            if (X1 > X2) and (Y1 < Y2) ->
                                   [P];
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    Line),

    Third =
        lists:foldl(fun ({X1, Y1} = P, []) when (X1 > 0) and (Y1 < 0) ->
                            [P];
                        ({X1, Y1} = P, [{X2, Y2}] = Acc) when (X1 > 0) and (Y1 < 0) ->
                            if (X1 < X2) and (Y1 > Y2) ->
                                   [P];
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    Line),

    Fourth =
        lists:foldl(fun ({X1, Y1} = P, []) when (X1 > 0) and (Y1 > 0) ->
                            [P];
                        ({X1, Y1} = P, [{X2, Y2}] = Acc) when (X1 > 0) and (Y1 > 0) ->
                            if (X1 < X2) and (Y1 < Y2) ->
                                   [P];
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    Line),

    [Third, Fourth, Second, First].

sort(Line) ->
    lists:sort(fun({X1, Y1}, {X2, Y2}) ->
                  if (X1 > X2) and (Y1 > Y2) -> false;
                     (X1 > X2) and (Y1 == Y2) -> false;
                     (X1 == X2) and (Y1 > Y2) -> false;
                     true -> true
                  end
               end,
               Line).

offsets({X1, Y1} = Point, Coordinates) ->
    {Point, lists:map(fun({X, Y}) -> {X - X1, Y - Y1} end, Coordinates)}.

same_horizontale(Of) ->
    Neg = lists:filter(fun ({X, 0}) when X < 0 ->
                               true;
                           (_) ->
                               false
                       end,
                       Of),
    Pos = lists:filter(fun ({X, 0}) when X > 0 ->
                               true;
                           (_) ->
                               false
                       end,
                       Of),
    {Neg, Pos}.

same_vertical(Of) ->
    Neg = lists:filter(fun ({0, Y}) when Y < 0 ->
                               true;
                           (_) ->
                               false
                       end,
                       Of),
    Pos = lists:filter(fun ({0, Y}) when Y > 0 ->
                               true;
                           (_) ->
                               false
                       end,
                       Of),
    {Neg, Pos}.

same_diagonal(Of) ->
    lists:map(fun({X1, Y1} = O) ->
                 lists:foldl(fun({X2, Y2} = O1, Acc) ->
                                case X1 / X2 == Y1 / Y2 of
                                    true -> [O1 | Acc];
                                    false -> Acc
                                end
                             end,
                             [O],
                             Of -- [O])
              end,
              Of).

decode_map(MapPath) ->
    {ok, BinData} = file:read_file(MapPath),
    BinLines = binary:split(BinData, <<"\n">>, [global]),
    lines_to_coordinates(BinLines, 0, []).

lines_to_coordinates([], _LineNumber, Coordinates) ->
    Coordinates;
lines_to_coordinates([BinLine | Lines], LineNumber, Coordinates) ->
    Matches = binary:matches(BinLine, <<"#">>),
    NewCoordinates =
        lists:foldl(fun({Position, 1}, Acc) -> [{Position, LineNumber} | Acc] end,
                    Coordinates,
                    Matches),
    lines_to_coordinates(Lines, LineNumber + 1, NewCoordinates).
