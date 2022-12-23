-module(day23).
-export([part1/0, part2/0]).

input() ->
    Lines = aoc_input:read(?MODULE, "^(.+)$", [binary]),
    lists:foldl(fun ({Line, Y}, YAcc) ->
        lists:foldl(fun (X, XAcc) ->
            case binary:at(Line, X - 1) of
                $# -> sets:add_element({X, Y}, XAcc);
                _ -> XAcc
            end
        end, YAcc, lists:seq(1, byte_size(Line)))
    end, sets:new(), lists:zip(Lines, lists:seq(1, length(Lines)))).

adjacent({X, Y}, all) ->
    [
        {X - 1, Y - 1}, {X, Y - 1}, {X + 1, Y - 1},
        {X - 1, Y}, {X + 1, Y},
        {X - 1, Y + 1}, {X, Y + 1}, {X + 1, Y + 1}
    ];
adjacent({X, Y}, n) ->
    [{X - 1, Y - 1}, {X, Y - 1}, {X + 1, Y - 1}];
adjacent({X, Y}, s) ->
    [{X - 1, Y + 1}, {X, Y + 1}, {X + 1, Y + 1}];
adjacent({X, Y}, w) ->
    [{X - 1, Y - 1}, {X - 1, Y}, {X - 1, Y + 1}];
adjacent({X, Y}, e) ->
    [{X + 1, Y - 1}, {X + 1, Y}, {X + 1, Y + 1}].

any_adjacent(XY, Dir, XYs) ->
    lists:any(fun (AdjXY) -> sets:is_element(AdjXY, XYs) end, adjacent(XY, Dir)).

move({X, Y}, n) -> {X, Y - 1};
move({X, Y}, s) -> {X, Y + 1};
move({X, Y}, w) -> {X - 1, Y};
move({X, Y}, e) -> {X + 1, Y}.

step(XYs, Dirs) ->
    Moves = sets:fold(fun (XY, Acc) ->
        case any_adjacent(XY, all, XYs) of
            true ->
                {MoveTo, _} = lists:foldl(fun
                    (Dir, {DAcc, false}) ->
                        case any_adjacent(XY, Dir, XYs) of
                            false -> {move(XY, Dir), true};
                            true -> {DAcc, false}
                        end;
                    (_Dir, {DAcc, true}) ->
                        {DAcc, true}
                end, {XY, false}, Dirs),
                Acc#{MoveTo => [XY | maps:get(MoveTo, Acc, [])]};
            false ->
                Acc#{XY => [XY | maps:get(XY, Acc, [])]}
        end
    end, #{}, XYs),
    {Result, AnyMoved} = maps:fold(fun
        (XYTo, [XYFrom], {Acc, AnyMovedAcc}) ->
            {[XYTo | Acc], AnyMovedAcc orelse XYTo =/= XYFrom};
        (_XYTo, XYFroms, {Acc, AnyMovedAcc}) ->
            {[XYFroms | Acc], AnyMovedAcc}
    end, {[], false}, Moves),
    {sets:from_list(lists:flatten(Result)), AnyMoved}.

next_dirs([Dir1, Dir2, Dir3, Dir4]) ->
    [Dir2, Dir3, Dir4, Dir1].

part1() ->
    {After10, _} = lists:foldl(fun (_I, {XYAcc, DirsAcc}) ->
        {element(1, step(XYAcc, DirsAcc)), next_dirs(DirsAcc)}
    end, {input(), [n, s, w, e]}, lists:seq(1, 10)),
    {Xs, Ys} = lists:unzip(sets:to_list(After10)),
    (lists:max(Xs) - lists:min(Xs) + 1) * (lists:max(Ys) - lists:min(Ys) + 1) - sets:size(After10).

till_not_moving(XYs, Dirs, Round) ->
    case step(XYs, Dirs) of
        {_, false} -> Round;
        {NewXYs, true} -> till_not_moving(NewXYs, next_dirs(Dirs), Round + 1)
    end.

part2() ->
    till_not_moving(input(), [n, s, w, e], 1).