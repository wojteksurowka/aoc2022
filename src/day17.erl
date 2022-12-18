-module(day17).
-export([part1/0]).

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

part1() ->
    Jet = input(),
    Shapes = {line, cross, invl, vline, square},
    ShapesLen = tuple_size(Shapes),
    getmax(element(1, lists:foldl(fun (Index, {Well, JetIndex}) ->
        do_shape(element(Index rem ShapesLen + 1, Shapes), JetIndex, Jet, Well)
    end, {new_well(), 0}, lists:seq(0, 2022 - 1)))).