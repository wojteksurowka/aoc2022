-module(day9).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "^(.) (\\d+)$", [atom, integer]).

moveH('R', {X, Y}) -> {X + 1, Y};
moveH('L', {X, Y}) -> {X - 1, Y};
moveH('U', {X, Y}) -> {X, Y + 1};
moveH('D', {X, Y}) -> {X, Y - 1}.

followT({X, HY}, {X, Y}) when abs(HY - Y) == 2 -> {X, round((HY + Y) / 2)};
followT({HX, Y}, {X, Y}) when abs(HX - X) == 2 -> {round((HX + X) / 2), Y};
followT({HX, HY}, {X, Y}) when abs(HX - X) =< 1, abs(HY - Y) =< 1 -> {X, Y};
followT({X, _HY}, {X, Y}) -> {X, Y};
followT({_HX, Y}, {X, Y}) -> {X, Y};
followT({HX, HY}, {X, Y}) ->
    case {HX < X, HY < Y} of
        {true, true} -> {X - 1, Y - 1};
        {true, false} -> {X - 1, Y + 1};
        {false, true} -> {X + 1, Y - 1};
        {false, false} -> {X + 1, Y + 1}
    end.

follow(NTails) ->
    Singles = lists:flatten([lists:duplicate(S, D) || {D, S} <- input()]),
    {_, _, Visited} = lists:foldl(fun (Move, {H, Tails, Acc}) ->
        NewH = moveH(Move, H),
        {NewTails, LastT} = lists:mapfoldl(fun (T, Follow) ->
            NewT = followT(Follow, T),
            {NewT, NewT}
        end, NewH, Tails),
        {NewH, NewTails, [LastT | Acc]}
    end, {{1, 1}, lists:duplicate(NTails, {1, 1}), []}, Singles),
    length(lists:usort(Visited)).

part1() ->
    follow(1).

part2() ->
    follow(9).