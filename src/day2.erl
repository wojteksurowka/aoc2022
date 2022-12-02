-module(day2).
-export([part1/0, part2/0]).

input() ->
    [{binary_to_atom(string:lowercase(O)), binary_to_atom(string:lowercase(M))} || {O, M} <- aoc_input:read(?MODULE, "^(.) (.)$", [binary, binary])].

part1() ->
    lists:foldl(fun ({O, M}, Acc) ->
        Acc + compare(O, M) + shape(M)
    end, 0, input()).

part2() ->
    lists:foldl(fun ({O, D}, Acc) ->
        M = decrypt(O, D),
        Acc + compare(O, M) + shape(M)
    end, 0, input()).

compare(a, y) -> 6;
compare(b, z) -> 6;
compare(c, x) -> 6;
compare(a, x) -> 3;
compare(b, y) -> 3;
compare(c, z) -> 3;
compare(_, _) -> 0.

shape(x) -> 1;
shape(y) -> 2;
shape(z) -> 3.

decrypt(a, x) -> z;
decrypt(b, x) -> x;
decrypt(c, x) -> y;
decrypt(a, y) -> x;
decrypt(b, y) -> y;
decrypt(c, y) -> z;
decrypt(a, z) -> y;
decrypt(b, z) -> z;
decrypt(c, z) -> x.
