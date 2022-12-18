-module(day17).
-export([part1/0, part2/0]).

input() ->
    hd(aoc_input:read(?MODULE, "^(.+)$", [binary])).

shape(line) ->
    [[true, true, true, true]];
shape(cross) ->
    [
        [false, true, false],
        [true, true, true],
        [false, true, false]
    ];
shape(invl) ->
    [
        [false, false, true],
        [false, false, true],
        [true, true, true]
    ];
shape(vline) ->
    [[true], [true], [true], [true]];
shape(square) ->
    [[true, true], [true, true]].

new_well() ->
    {sets:new(), 0}.

occupied(X, Y, {WellSet, _Max}) ->
    case X < 1 orelse X > 7 orelse Y == 0 of
        true -> true;
        false -> sets:is_element({X, Y}, WellSet)
    end.

set(X, Y, {WellSet, Max}) ->
    {sets:add_element({X, Y}, WellSet), max(Y, Max)}.

getmax({_WellSet, Max}) ->
    Max.

add({XOffset, YOffset, Shape}, Well) ->
    lists:foldl(fun ({Line, Y}, YAcc) ->
        lists:foldl(fun
            ({true, X}, XAcc) -> set(XOffset + X, YOffset + Y, XAcc);
            ({false, _X}, XAcc) -> XAcc
        end, YAcc, lists:zip(Line, lists:seq(0, length(Line) - 1)))
    end, Well, lists:zip(lists:reverse(Shape), lists:seq(0, length(Shape) - 1))).

blocked({XOffset, YOffset, Shape}, Well) ->
    lists:foldl(fun ({Line, Y}, YAcc) ->
        lists:foldl(fun
            (_, true) -> true;
            ({true, X}, false) -> occupied(XOffset + X, YOffset + Y, Well);
            ({false, _X}, false) -> false
        end, YAcc, lists:zip(Line, lists:seq(0, length(Line) - 1)))
    end, false, lists:zip(lists:reverse(Shape), lists:seq(0, length(Shape) - 1))).

move({XOffset, YOffset, Shape}, JetIndex, Jet, Well) when JetIndex == byte_size(Jet) ->
    move({XOffset, YOffset, Shape}, 0, Jet, Well);
move({XOffset, YOffset, Shape}, JetIndex, Jet, Well) ->
    MovedXYShape = case binary:at(Jet, JetIndex) of
        $> -> {XOffset + 1, YOffset, Shape};
        $< -> {XOffset - 1, YOffset, Shape}
    end,
    case blocked(MovedXYShape, Well) of
        true -> drop({XOffset, YOffset, Shape}, JetIndex + 1, Jet, Well);
        false -> drop(MovedXYShape, JetIndex + 1, Jet, Well)
    end.

drop({XOffset, YOffset, Shape}, JetIndex, Jet, Well) ->
    MovedXYShape = {XOffset, YOffset - 1, Shape},
    case blocked(MovedXYShape, Well) of
        true -> {{XOffset, YOffset, Shape}, JetIndex};
        false -> move(MovedXYShape, JetIndex, Jet, Well)
    end.

do_shape(ShapeName, JetIndex, Jet, Well) ->
    {XYShape, UpdatedJetIndex} = move({3, getmax(Well) + 4, shape(ShapeName)}, JetIndex, Jet, Well),
    {add(XYShape, Well), UpdatedJetIndex}.

max_steps(Steps) ->
    Jet = input(),
    Shapes = {line, cross, invl, vline, square},
    ShapesLen = tuple_size(Shapes),
    getmax(element(1, lists:foldl(fun (Index, {Well, JetIndex}) ->
        do_shape(element(Index rem ShapesLen + 1, Shapes), JetIndex, Jet, Well)
    end, {new_well(), 0}, lists:seq(0, Steps - 1)))).

part1() ->
    max_steps(2022).

part2() ->
    Jet = input(),
    Shapes = {line, cross, invl, vline, square},
    ShapesLen = tuple_size(Shapes),
    {_, _, DiffMax, Index, DiffIndex} = hd(lists:reverse(lists:keysort(1, maps:values(element(3, lists:foldl(fun (Index, {Well, JetIndex, Map}) ->
        {UpdatedWell, UpdatedJetIndex} = do_shape(element(Index rem ShapesLen + 1, Shapes), JetIndex, Jet, Well),
        Key = {Index rem ShapesLen + 1, UpdatedJetIndex},
        UpdatedMap = case Map of
            #{Key := {1, PreviousMax, _, PreviousIndex, _}} ->
                Map#{Key := {2, getmax(UpdatedWell), getmax(UpdatedWell) - PreviousMax, Index, Index - PreviousIndex}};
            #{Key := {Count, PreviousMax, DiffMax, PreviousIndex, IndexDiff}} ->
                case getmax(UpdatedWell) of
                    Max when Max == PreviousMax + DiffMax ->
                        case PreviousIndex + IndexDiff == Index of
                            true -> Map#{Key := {Count + 1, PreviousMax + DiffMax, DiffMax, Index, IndexDiff}};
                            false -> Map#{Key := {1, Max, 0, Index, 0}}
                        end;
                    Max ->
                        Map#{Key := {1, Max, 0, Index, 0}}
                end;
            _ ->
                Map#{Key => {1, getmax(UpdatedWell), 0, Index, 0}}
        end,
        {UpdatedWell, UpdatedJetIndex, UpdatedMap}
    end, {new_well(), 0, #{}}, lists:seq(0, 100000))))))),
    Steps = Index + (1000000000000 - Index) rem DiffIndex,
    max_steps(Steps) + ((1000000000000 - Index) div DiffIndex) * DiffMax.