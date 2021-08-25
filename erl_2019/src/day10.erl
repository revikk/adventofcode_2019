-module(day10).

-export([run/0]).

run() ->
    Coordinates = decode_map("priv/day10_test3_input"),
    Of = lists:map(fun(C) -> offsets(C, Coordinates -- [C]) end, Coordinates),
    A = lists:map(fun({P, Of1}) ->
                     H = same_horizontale(Of1),
                     V = same_vertical(Of1 -- H),
                     Ds = same_diagonal(Of1 -- H ++ V),
                     {P,
                      remove_empty(nearest_in_horizontale(sort(H)))
                      ++ remove_empty(nearest_in_vertical(sort(V)))
                      ++ remove_duplicates(lists:flatmap(fun(D) ->
                                                            remove_empty(nearest_in_diagonale(sort(D)))
                                                         end,
                                                         Ds))}
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

    {P, length(S)}.

remove_duplicates(Line) ->
    lists:foldl(fun(Point, Acc) ->
                   case lists:member(Point, Acc) of
                       true -> Acc;
                       false -> [Point | Acc]
                   end
                end,
                [],
                Line).

remove_empty(Line) ->
    lists:filter(fun ({}) ->
                         false;
                     (_) ->
                         true
                 end,
                 Line).

nearest_in_vertical(Line) ->
    MaxFromNegative =
        lists:foldl(fun ({0, Y1} = P, {}) when Y1 < 0 ->
                            P;
                        ({0, Y1} = P, {_X2, Y2} = Acc) when Y1 < 0 ->
                            if Y1 > Y2 ->
                                   P;
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    {},
                    Line),
    MinFromPositive =
        lists:foldl(fun ({0, Y1} = P, {}) when Y1 > 0 ->
                            P;
                        ({0, Y1} = P, {_X2, Y2} = Acc) when Y1 > 0 ->
                            if Y1 < Y2 ->
                                   P;
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    {},
                    Line),

    [MaxFromNegative, MinFromPositive].

nearest_in_horizontale(Line) ->
    MaxFromNegative =
        lists:foldl(fun ({X1, 0} = P, {}) when X1 < 0 ->
                            P;
                        ({X1, 0} = P, {X2, _} = Acc) when X1 < 0 ->
                            if X1 > X2 ->
                                   P;
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    {},
                    Line),
    MinFromPositive =
        lists:foldl(fun ({X1, 0} = P, {}) when X1 > 0 ->
                            P;
                        ({X1, 0} = P, {X2, _Y2} = Acc) when X1 > 0 ->
                            if X1 < X2 ->
                                   P;
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    {},
                    Line),
    [MaxFromNegative, MinFromPositive].

nearest_in_diagonale(Line) ->
    %  First   | Third
    % --------+-------
    % Second | Fourth
    First =
        lists:foldl(fun ({X1, Y1} = P, {}) when (X1 < 0) and (Y1 < 0) ->
                            P;
                        ({X1, Y1} = P, {X2, Y2} = Acc) when (X1 < 0) and (Y1 < 0) ->
                            if (X1 > X2) and (Y1 > Y2) ->
                                   P;
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    {},
                    Line),
    Second =
        lists:foldl(fun ({X1, Y1} = P, {}) when (X1 < 0) and (Y1 > 0) ->
                            P;
                        ({X1, Y1} = P, {X2, Y2} = Acc) when (X1 < 0) and (Y1 > 0) ->
                            if (X1 > X2) and (Y1 < Y2) ->
                                   P;
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    {},
                    Line),

    Third =
        lists:foldl(fun ({X1, Y1} = P, {}) when (X1 > 0) and (Y1 < 0) ->
                            P;
                        ({X1, Y1} = P, {X2, Y2} = Acc) when (X1 > 0) and (Y1 < 0) ->
                            if (X1 < X2) and (Y1 > Y2) ->
                                   P;
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    {},
                    Line),

    Fourth =
        lists:foldl(fun ({X1, Y1} = P, {}) when (X1 > 0) and (Y1 > 0) ->
                            P;
                        ({X1, Y1} = P, {X2, Y2} = Acc) when (X1 > 0) and (Y1 > 0) ->
                            if (X1 < X2) and (Y1 < Y2) ->
                                   P;
                               true ->
                                   Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    {},
                    Line),

    [First, Second, Third, Fourth].

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
    lists:filter(fun ({_, 0}) ->
                         true;
                     (_) ->
                         false
                 end,
                 Of).

same_vertical(Of) ->
    lists:filter(fun ({0, _}) ->
                         true;
                     (_) ->
                         false
                 end,
                 Of).

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
