-module(day6).

-export([run/1]).

run(OrbitMap) ->
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
