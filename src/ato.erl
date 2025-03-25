
-module(ato).

-export([f/0, factorial/1, contains/2, power/2, duplicateElements/1, sumFloats/1, sumFloats/2]).

f() ->
  io:format("Hello"),
  42.

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

contains([], _) -> false;
contains([H | _T], H) -> true;
contains([_ | T], E) -> contains(T, E).

power(_, 0) -> 1;
power(X, Y) when Y < 0 -> 1 / power(X, -Y);
power(X, Y) -> X * power(X, Y - 1).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H] ++ duplicateElements(T).

sumFloats([]) -> 0.0;
sumFloats([H | T]) when is_float(H) -> H + sumFloats(T);
sumFloats([_ | T]) -> sumFloats(T).

sumFloats([], Acc) -> Acc;
sumFloats([H | T], Acc) when is_float(H) -> sumFloats(T, Acc + H);
sumFloats([_ | T], Acc) -> sumFloats(T, Acc).