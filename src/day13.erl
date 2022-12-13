-module(day13).
-export([part1/0, part2/0]).

input() ->
    convert(aoc_input:read(?MODULE, "^(.+)$", [list])).

convert([]) ->
    [];
convert([First, Second | T]) ->
    [{parse(First), parse(Second)} | convert(T)].

parse(String) ->
    {ok, Tokens, _Loc} = erl_scan:string(String ++ "."),
    {ok, Parsed} = erl_parse:parse_term(Tokens),
    Parsed.

compare(L, R) when is_integer(L), is_integer(R), L < R ->
    less;
compare(L, R) when is_integer(L), is_integer(R), L > R ->
    greater;
compare(L, R) when is_integer(L), is_integer(R) ->
    equal;
compare([], []) ->
    equal;
compare([], R) when is_list(R) ->
    less;
compare(L, []) when is_list(L) ->
    greater;
compare([LH | LT], [RH | RT]) ->
    case compare(LH, RH) of
        equal -> compare(LT, RT);
        Diff -> Diff
    end;
compare(L, R) when is_integer(L) ->
    compare([L], R);
compare(L, R) when is_integer(R) ->
    compare(L, [R]).

part1() ->
    Input = input(),
    {_, Indices} = lists:unzip(lists:filter(fun ({{First, Second}, _Index}) ->
        compare(First, Second) =:= less
    end, lists:zip(Input, lists:seq(1, length(Input))))),
    lists:sum(Indices).

part2() ->
    Flattened = lists:foldl(fun ({First, Second}, Acc) ->
        [First, Second | Acc]
    end, [[[2]], [[6]]], input()),
    Sorted = lists:sort(fun (L, R) ->
        Result = compare(L, R),
        Result =:= less orelse Result =:= equal
    end, Flattened),
    lists:foldl(fun
        ({[[2]], Index}, Acc) -> Acc * Index;
        ({[[6]], Index}, Acc) -> Acc * Index;
        ({_List, _Index}, Acc) -> Acc
    end, 1, lists:zip(Sorted, lists:seq(1, length(Sorted)))).
