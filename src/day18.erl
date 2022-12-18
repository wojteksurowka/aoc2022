-module(day18).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "^(\\d+),(\\d+),(\\d+)$", [integer, integer, integer]).

neighbours({X, Y, Z}) ->
    [
        {X - 1, Y, Z}, {X + 1, Y, Z},
        {X, Y - 1, Z}, {X, Y + 1, Z},
        {X, Y, Z - 1}, {X, Y, Z + 1}
    ].

sides(Cube, Cubes) ->
    length(lists:filter(fun (N) -> not sets:is_element(N, Cubes) end, neighbours(Cube))).

part1() ->
    Input = input(),
    Cubes = sets:from_list(Input),
    lists:sum([sides(C, Cubes) || C <- Input]).

limits(Input) ->
    {Xs, Ys, Zs} = lists:unzip3(Input),
    {lists:min(Xs) - 1, lists:max(Xs) + 1, lists:min(Ys) - 1, lists:max(Ys) + 1, lists:min(Zs) - 1, lists:max(Zs) + 1}.

visible_from_side(Side, Dir, Cube, Cubes, Limit) ->
    NewCoord = element(Side, Cube) + Dir,
    NextCube = setelement(Side, Cube, NewCoord),
    case (Dir > 0 andalso NewCoord > Limit) orelse (Dir < 0 andalso NewCoord < Limit) of
        true ->
            true;
        false ->
            case sets:is_element(NextCube, Cubes) of
                true -> false;
                false -> visible_from_side(Side, Dir, NextCube, Cubes, Limit)
            end
    end.

visible(Cube, Cubes, {MinX, MaxX, MinY, MaxY, MinZ, MaxZ}) ->
    visible_from_side(1, -1, Cube, Cubes, MinX) orelse
    visible_from_side(1, 1, Cube, Cubes, MaxX) orelse
    visible_from_side(2, -1, Cube, Cubes, MinY) orelse
    visible_from_side(2, 1, Cube, Cubes, MaxY) orelse
    visible_from_side(3, -1, Cube, Cubes, MinZ) orelse
    visible_from_side(3, 1, Cube, Cubes, MaxZ).

filter(Fun, {MinX, MaxX, MinY, MaxY, MinZ, MaxZ}) ->
    lists:foldl(fun (X, XAcc) ->
        lists:foldl(fun (Y, YAcc) ->
            lists:foldl(fun (Z, ZAcc) ->
                case Fun({X, Y, Z}) of
                    true -> [{X, Y, Z} | ZAcc];
                    false -> ZAcc
                end
            end, YAcc, lists:seq(MinZ, MaxZ))
        end, XAcc, lists:seq(MinY, MaxY))
    end, [], lists:seq(MinX, MaxX)).

all_visible(VisibleTillNow, Cubes, Limits) ->
    VisibleNeighbours = filter(fun (Cube) ->
        case sets:is_element(Cube, Cubes) orelse sets:is_element(Cube, VisibleTillNow) of
            false ->
                lists:any(fun (N) -> sets:is_element(N, VisibleTillNow) end, neighbours(Cube));
            true ->
                false
        end
    end, Limits),
    case VisibleNeighbours of
        [] ->
            VisibleTillNow;
        _ ->
            all_visible(sets:union(VisibleTillNow, sets:from_list(VisibleNeighbours)), Cubes, Limits)
    end.

visible_sides(Cube, Cubes, AllVisible) ->
    length(lists:filter(fun (N) -> not sets:is_element(N, Cubes) andalso sets:is_element(N, AllVisible) end, neighbours(Cube))).

part2() ->
    Input = input(),
    Limits = limits(Input),
    Cubes = sets:from_list(Input),
    DirectlyVisible = sets:from_list(filter(fun (Cube) ->
        not sets:is_element(Cube, Cubes) andalso visible(Cube, Cubes, Limits)
    end, Limits)),
    AllVisible = all_visible(DirectlyVisible, Cubes, Limits),
    lists:sum([visible_sides(C, Cubes, AllVisible) || C <- Input]).