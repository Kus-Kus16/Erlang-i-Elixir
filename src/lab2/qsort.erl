-module(qsort).

-export([less_than/2, grt_eq_than/2, qs/1, random_elems/3, compare_speeds/3]).

less_than(List, Arg) ->
    [X || X <- List, X < Arg].

grt_eq_than(List, Arg) ->
    [X || X <- List, X >= Arg].

qs([Pivot|Tail]) ->
    qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) );
qs([]) -> [].

random_elems(N,Min,Max) ->
    [rand:uniform(Max - Min + 1) - 1 + Min || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
    {Time1, _} = timer:tc(Fun1, [List]),
    {Time2, _} = timer:tc(Fun2, [List]),
    io:format("Funkcja 1: ~w, Funkcja 2: ~w~n", [Time1, Time2]).
