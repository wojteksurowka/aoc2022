-module(day20).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "^(.+)$", [integer]).

move(OrgIndex, List, Len) ->
    {Index, BeforeReversed, [{Element, OrgIndex, _Rank} | T]} = pop(OrgIndex, List, 0, []),
    Shift = case (Index + Element) rem (Len - 1) of
        NI when NI =< 0 -> NI + Len - 1 - Index;
        NI -> NI - Index
    end,
    if
        Shift < 0 ->
            case lists:nthtail(-Shift - 1, BeforeReversed) of
                [{_, _, FirstRank}] ->
                    lists:keysort(3, lists:append(lists:reverse(BeforeReversed), [{Element, OrgIndex, FirstRank - Len} | T]));
                [{_, _, Rank1}, {_, _, Rank2} | _] ->
                    lists:keysort(3, lists:append(lists:reverse(BeforeReversed), [{Element, OrgIndex, (Rank1 + Rank2) / 2} | T]))
            end;
        Shift > 0 ->
            case lists:nthtail(Shift - 1, T) of
                [{_, _, RankLast}] ->
                    lists:keysort(3, lists:append(lists:reverse(BeforeReversed), [{Element, OrgIndex, RankLast + Len} | T]));
                [{_, _, Rank1}, {_, _, Rank2} | _] ->
                    lists:keysort(3, lists:append(lists:reverse(BeforeReversed), [{Element, OrgIndex, (Rank1 + Rank2) / 2} | T]))
            end;
        true ->
            List
    end.

pop(Index, [{_Element, Index, _Rank} | _] = Rest, LAcc, Acc) ->
    {LAcc, Acc, Rest};
pop(Index, [H | T], LAcc, Acc) ->
    pop(Index, T, LAcc + 1, [H | Acc]).

find_index(Element, [Element | _], Acc) ->
    Acc;
find_index(Element, [_ | T], Acc) ->
    find_index(Element, T, Acc + 1).

part1() ->
    Input = input(),
    Len = length(Input),
    Ranked = lists:zip3(Input, lists:seq(1, Len), lists:seq(1, Len)),
    Result = element(1, lists:unzip3(lists:foldl(fun (OrgIndex, Acc) ->
        move(OrgIndex, Acc, Len)
    end, Ranked, lists:seq(1, Len)))),
    ZeroIndex = find_index(0, Result, 0),
    Nth = fun (I) -> lists:nth((ZeroIndex + I) rem Len + 1, Result) end,
    Nth(1000) + Nth(2000) + Nth(3000).

part2() ->
    Input = [E * 811589153 || E <- input()],
    Len = length(Input),
    Ranked = lists:zip3(Input, lists:seq(1, Len), lists:seq(1, Len)),
    Result = element(1, lists:unzip3(lists:foldl(fun (_, Acc10) ->
        lists:foldl(fun (OrgIndex, Acc) ->
            move(OrgIndex, Acc, Len)
        end, Acc10, lists:seq(1, Len))
    end, Ranked, lists:seq(1, 10)))),
    ZeroIndex = find_index(0, Result, 0),
    Nth = fun (I) -> lists:nth((ZeroIndex + I) rem Len + 1, Result) end,
    Nth(1000) + Nth(2000) + Nth(3000).
