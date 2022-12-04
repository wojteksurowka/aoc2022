-module(day4).
-export([part1/0, part2/0]).

input() ->
    FTs = aoc_input:read(?MODULE, "^(\\d+)-(\\d+),(\\d+)-(\\d+)$", [integer, integer, integer, integer]),
    [{sets:from_list(lists:seq(F1, T1)), sets:from_list(lists:seq(F2, T2))} || {F1, T1, F2, T2} <- FTs].

part1() ->
    lists:foldl(fun ({S1, S2}, Acc) ->
        case sets:is_subset(S1, S2) orelse sets:is_subset(S2, S1) of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, input()).

part2() ->
    lists:foldl(fun ({S1, S2}, Acc) ->
        case sets:is_disjoint(S1, S2) of
            true -> Acc;
            false -> Acc + 1
        end
    end, 0, input()).