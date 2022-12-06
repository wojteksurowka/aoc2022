-module(day6).
-export([part1/0, part2/0]).

input() ->
    hd(aoc_input:read(?MODULE, "^(.+)$", [binary])).

part1() ->
    find_marker(input(), 1, [], 4).

part2() ->
    find_marker(input(), 1, [], 14).

find_marker(<<C:8, Rest/binary>>, Index, Previous, DistinctCount) ->
    case sets:size(sets:from_list([C | Previous])) of
        DistinctCount ->
            Index;
        _ ->
            find_marker(Rest, Index + 1, [C | lists:sublist(Previous, DistinctCount - 2)], DistinctCount)
    end.