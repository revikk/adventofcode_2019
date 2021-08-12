-module(day8).

-export([get_first_star/0, get_second_star/0]).

get_first_star() ->
    {ok, BinImage} = file:read_file("priv/day8_input"),

    Wide = 25,
    Tall = 6,

    Layers = split_on_layers(Wide * Tall, BinImage, 0, []),

    LayersStat =
        lists:map(fun(Layer) when is_binary(Layer) ->
                     lists:foldl(fun ($0, {NumZero, NumOne, NumTwo}) ->
                                         {NumZero + 1, NumOne, NumTwo};
                                     ($1, {NumZero, NumOne, NumTwo}) ->
                                         {NumZero, NumOne + 1, NumTwo};
                                     ($2, {NumZero, NumOne, NumTwo}) ->
                                         {NumZero, NumOne, NumTwo + 1}
                                 end,
                                 {0, 0, 0},
                                 binary_to_list(Layer))
                  end,
                  Layers),

    {_Zeros, Ones, Twos} =
        lists:foldl(fun ({NumZero, _, _} = Layer, {MinZero, _, _}) when NumZero < MinZero ->
                            Layer;
                        (_, Acc) ->
                            Acc
                    end,
                    hd(LayersStat),
                    tl(LayersStat)),

    Ones * Twos.

get_second_star() ->
    {ok, BinImage} = file:read_file("priv/day8_input"),

    Wide = 25,
    Tall = 6,

    Layers = split_on_layers(Wide * Tall, BinImage, 0, []),

    Message =
        lists:foldl(fun(Layer, Message) -> merge_layers(binary_to_list(Layer), Message, []) end,
                    binary_to_list(hd(Layers)),
                    tl(Layers)),

    print_message(Message, Wide).

print_message([], _Wide) ->
    ok;
print_message(Message, Wide) ->
    {Row, Rest} = lists:split(Wide, Message),
    io:format("~p~n", [Row]),
    print_message(Rest, Wide).

merge_layers([], [], Result) ->
    lists:reverse(Result);
merge_layers([$0 | DownTail], [$2 | UpTail], Result) ->
    merge_layers(DownTail, UpTail, [$0 | Result]);
merge_layers([$1 | DownTail], [$2 | UpTail], Result) ->
    merge_layers(DownTail, UpTail, [$1 | Result]);
merge_layers([$2 | DownTail], [$2 | UpTail], Result) ->
    merge_layers(DownTail, UpTail, [$2 | Result]);
merge_layers([_ | DownTail], [$0 | UpTail], Result) ->
    merge_layers(DownTail, UpTail, [$0 | Result]);
merge_layers([_ | DownTail], [$1 | UpTail], Result) ->
    merge_layers(DownTail, UpTail, [$1 | Result]).

split_on_layers(_Size, Image, StartPosition, Layers)
    when StartPosition >= byte_size(Image) ->
    lists:reverse(Layers);
split_on_layers(LayerSize, Image, StartPosition, Layers) when is_binary(Image) ->
    Layer = binary:part(Image, {StartPosition, LayerSize}),

    split_on_layers(LayerSize, Image, StartPosition + LayerSize, [Layer | Layers]).
