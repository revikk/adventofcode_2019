-module(day6).

-export([build_map/1, get_number_of_paths/1, get_number_of_transfers/1]).

build_map(OrbitMap) ->
    {ok, BinData} = file:read_file(OrbitMap),
    Orbits = binary:split(BinData, <<"\n">>, [global]),

    Map = digraph:new([acyclic]),
    lists:foreach(fun(Orbit) ->
                     [Center, Satellite] = binary:split(Orbit, <<")">>),
                     digraph:add_vertex(Map, Center),
                     digraph:add_vertex(Map, Satellite),
                     digraph:add_edge(Map, Satellite, Center)
                  end,
                  Orbits),
    Map.

get_number_of_paths(Map) ->
    Planets = digraph:vertices(Map),
    CenterOfMass = <<"COM">>,

    lists:foldl(fun(Planet, NumOrbits) ->
                   case digraph:get_short_path(Map, Planet, CenterOfMass) of
                       false -> NumOrbits;
                       Path -> length(Path) - 1 + NumOrbits
                   end
                end,
                0,
                Planets).

get_number_of_transfers(Map) ->
    You = <<"YOU">>,
    San = <<"SAN">>,
    CenterOfMass = <<"COM">>,

    [YouCenter] = digraph:out_neighbours(Map, You),
    [SanCenter] = digraph:out_neighbours(Map, San),

    YouPath = digraph:get_path(Map, YouCenter, CenterOfMass),
    SanPath = digraph:get_path(Map, SanCenter, CenterOfMass),

    YouBranch = YouPath -- SanPath,
    SanBranch = SanPath -- YouPath,

    length(YouBranch ++ SanBranch).
