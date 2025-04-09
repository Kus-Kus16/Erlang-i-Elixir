-module(sensor_dist).

-export([get_rand_locations/1, dist/2, find_for_person/2, find_closest/2, find_for_person/3, find_closest_parallel/2]).

get_rand_locations(N) ->
    [{rand:uniform(10_000), rand:uniform(10_000)} || _ <- lists:seq(1, N)].

dist({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

find_for_person(PersonLocation, SensorsLocations) ->
    lists:min([{dist(PersonLocation, SenLoc), {PersonLocation, SenLoc}} || SenLoc <- SensorsLocations]).

find_closest(PeopleLocations, SensorsLocations) ->
    lists:min([find_for_person(PerLoc, SensorsLocations) || PerLoc <- PeopleLocations]).

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
    ParentPID ! find_for_person(PersonLocation, SensorsLocations).

find_closest_parallel(PeopleLocations, SensorsLocations) ->
    [ spawn(?MODULE, find_for_person, [PerLoc, SensorsLocations, self()]) || PerLoc <- PeopleLocations ],
    lists:min([ receive Data -> Data end || _ <- PeopleLocations]).
