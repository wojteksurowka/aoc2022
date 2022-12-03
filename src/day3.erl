-module(day3).
-export([part1/0, part2/0]).

input1() ->
    lists:map(fun (Line) ->
        FirstHalf = binary:part(Line, 0, byte_size(Line) div 2),
        SecondHalf = binary:part(Line, byte_size(Line) div 2, byte_size(Line) div 2),
        {to_set(FirstHalf), to_set(SecondHalf)}
    end, aoc_input:read(?MODULE, "^(.+)$", [binary])).

input2() ->
    Splitter = fun
        F([Third, Second, First | Tail], Acc) -> [{to_set(First), to_set(Second), to_set(Third)} | F(Tail, Acc)];
        F([], Acc) -> Acc
    end,
    lists:reverse(Splitter(lists:reverse(aoc_input:read(?MODULE, "^(.+)$", [binary])), [])).

to_set(Binary) ->
    Priority = fun
        (X) when X >= $a, X =< $z -> X - $a + 1;
        (X) when X >= $A, X =< $Z -> X - $A + 27
    end,
    sets:from_list([Priority(X) || X <- binary_to_list(Binary)]).

part1() ->
    lists:foldl(fun ({First, Second}, Acc) ->
        Acc + lists:sum(sets:to_list(sets:intersection(First, Second)))
    end, 0, input1()).

part2() ->
    lists:foldl(fun ({First, Second, Third}, Acc) ->
        Acc + lists:sum(sets:to_list(sets:intersection([First, Second, Third])))
    end, 0, input2()).