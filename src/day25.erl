-module(day25).
-export([part1/0]).

input() ->
    aoc_input:read(?MODULE, "^(.+)$", [list]).

to_snafu(I) ->
    Convert = fun
        Convert(0, "") -> "0";
        Convert(0, Acc) -> Acc;
        Convert(X, Acc) when X rem 5 == 3 -> Convert(X div 5 + 1, [$= | Acc]);
        Convert(X, Acc) when X rem 5 == 4 -> Convert(X div 5 + 1, [$- | Acc]);
        Convert(X, Acc) -> Convert(X div 5, [$0 + X rem 5 | Acc])
    end,
    Convert(I, "").

from_snafu(S) ->
    lists:foldl(fun
        ($=, Acc) -> Acc * 5 - 2;
        ($-, Acc) -> Acc * 5 - 1;
        (D, Acc) -> Acc * 5 + D - $0
    end, 0, S).

part1() ->
    to_snafu(lists:sum([from_snafu(S) || S <- input()])).