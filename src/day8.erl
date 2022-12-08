-module(day8).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "^(.+)$", [list]).

transpose(Map) ->
    lists:map(fun (X) ->
        lists:map(fun (Line) ->
            element(X, Line)
        end, [list_to_tuple(Line) || Line <- Map])
    end, lists:seq(1, length(hd(Map)))).

visible_from_left(Line) ->
    {XCoords, _Max} = lists:foldl(fun
        ({X, Tree}, {Acc, MaxBefore}) when Tree > MaxBefore -> {[X | Acc], Tree};
        (_, {Acc, MaxBefore}) -> {Acc, MaxBefore}
    end, {[], -1}, lists:zip(lists:seq(1, length(Line)), Line)),
    XCoords.

visible_from_left_or_right(Line) ->
    FromLeft = visible_from_left(Line),
    LineLength = length(Line),
    FromRight = [LineLength - X + 1 || X <- visible_from_left(lists:reverse(Line))],
    lists:usort(lists:flatten([FromLeft, FromRight])).

visible_from_left_or_right_map(Map) ->
    lists:flatten(lists:map(fun ({Y, Line}) ->
        [{X, Y} || X <- visible_from_left_or_right(Line)]
    end, lists:zip(lists:seq(1, length(Map)), Map))).

part1() ->
    Map = input(),
    HorVisible = visible_from_left_or_right_map(Map),
    VertVisible = [{Y, X} || {X, Y} <- visible_from_left_or_right_map(transpose(Map))],
    length(lists:usort(lists:flatten([HorVisible, VertVisible]))).

left_score(Line) ->
    {Scores, _} = lists:mapfoldl(fun (Tree, Before) ->
        {left_score(Tree, Before, 0), [Tree | Before]}
    end, [], Line),
    Scores.

left_score(_Tree, [], Acc) ->
    Acc;
left_score(Tree, [Next | T], Acc) when Tree > Next ->
    left_score(Tree, T, Acc + 1);
left_score(_Tree, _Before, Acc) ->
    Acc + 1.

mul(M1, M2) ->
    lists:map(fun ({Line1, Line2}) ->
        lists:map(fun ({E1, E2}) ->
            E1 * E2
        end, lists:zip(Line1, Line2))
    end, lists:zip(M1, M2)).

part2() ->
    Map = input(),
    Transposed = transpose(Map),
    LeftScores = [left_score(Line) || Line <- Map],
    RightScores = [lists:reverse(left_score(lists:reverse(Line))) || Line <- Map],
    TopScores = transpose([left_score(Line) || Line <- Transposed]),
    BottomScores = transpose([lists:reverse(left_score(lists:reverse(Line))) || Line <- Transposed]),
    lists:max(lists:flatten(mul(mul(LeftScores, RightScores), mul(TopScores, BottomScores)))).