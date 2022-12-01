-module(day1).
-export([part1/0, part2/0]).

input() ->
    Day = [$1],
    {ok, Input} = file:read_file("input/day" ++ Day ++ ".in"),
    lists:foldr(fun
        (<<>>, Acc) ->
            [[] | Acc];
        (Binary, [H | T]) ->
            [[binary_to_integer(Binary) | H] | T]
    end, [], binary:split(Input, <<"\n">>, [global])).

part1() ->
    lists:foldl(fun (Numbers, Acc) ->
        max(Acc, lists:sum(Numbers))
    end, -1, input()).

part2() ->
    lists:sum(lists:sublist(lists:reverse(lists:sort([lists:sum(Numbers) || Numbers <- input()])), 3)).