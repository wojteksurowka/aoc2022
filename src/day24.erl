-module(day24).
-export([part1/0, part2/0]).

input() ->
    Input = aoc_input:read(?MODULE, "^(.+)$", [list]),
    Width = length(hd(Input)),
    Height = length(Input),
    Map = lists:foldl(fun ({Line, Y}, YAcc) ->
        lists:foldl(fun
            ({$., _X}, XAcc) -> XAcc;
            ({$#, _X}, XAcc) -> XAcc;
            ({B, X}, XAcc) -> XAcc#{{X, Y} => [list_to_atom([B]) | maps:get({X, Y}, XAcc, [])]}
        end, YAcc, lists:zip(Line, lists:seq(1, Width)))
    end, #{}, lists:zip(Input, lists:seq(1, Height))),
    {Map, Width, Height}.

move({2, Y}, '<', Width, _Height) ->
    {Width - 1, Y};
move({X, Y}, '<', _Width, _Height) ->
    {X - 1, Y};
move({X, Y}, '>', Width, _Height) when X == Width - 1 ->
    {2, Y};
move({X, Y}, '>', _Width, _Height) ->
    {X + 1, Y};
move({X, 2}, '^', _Width, Height) ->
    {X, Height - 1};
move({X, Y}, '^', _Width, _Height) ->
    {X, Y - 1};
move({X, Y}, 'v', _Width, Height) when Y == Height - 1 ->
    {X, 2};
move({X, Y}, 'v', _Width, _Height) ->
    {X, Y + 1}.

next_map(Map, Width, Height) ->
    maps:fold(fun (XY, Dirs, MAcc) ->
        lists:foldl(fun (Dir, Acc) ->
            Moved = move(XY, Dir, Width, Height),
            Acc#{Moved => [Dir | maps:get(Moved, Acc, [])]}
        end, MAcc, Dirs)
    end, #{}, Map).

next_positions({X, Y}, Map, Width, Height) ->
    All = [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}, {X, Y}],
    lists:filter(fun
        ({2, 0}) -> false;
        ({2, 1}) -> true;
        ({NX, NY}) when NX == Width - 1, NY == Height + 1 -> false;
        ({NX, NY}) when NX == Width - 1, NY == Height -> true;
        ({1, _}) -> false;
        ({NX, _}) when NX == Width -> false;
        ({_, 1}) -> false;
        ({_, NY}) when NY == Height -> false;
        (XY) -> not maps:is_key(XY, Map)
    end, All).

dijkstra(InitialNode, NeighbourGetter, IsEnd) ->
    Loop = fun Loop(Current, Unvisited, Visited) ->
        DistanceGetter = fun (N) ->
            case {Visited, Unvisited} of
                {#{N := Distance}, _} -> Distance;
                {_, #{N := Distance}} -> Distance;
                _ -> infinity
            end
        end,
        Neighbours = NeighbourGetter(Current),
        UnvisitedNeighbours = lists:filter(fun (N) -> not maps:is_key(N, Visited) end, Neighbours),
        CurrentDistance = DistanceGetter(Current),
        UpdatedUnvisited = lists:foldl(fun (N, Acc) ->
            Acc#{N => min(CurrentDistance + 1, DistanceGetter(N))}
        end, maps:remove(Current, Unvisited), UnvisitedNeighbours),
        UpdatedVisited = Visited#{Current => CurrentDistance},
        {Next, _} = maps:fold(fun
            (N, D, {_Acc, DistanceAcc}) when D < DistanceAcc -> {N, D};
            (_N, _D, {Acc, DistanceAcc}) -> {Acc, DistanceAcc}
        end, {undefined, infinity}, UpdatedUnvisited),
        case IsEnd(Current) of
            false -> Loop(Next, UpdatedUnvisited, UpdatedVisited);
            true -> {Current, CurrentDistance}
        end
    end,
    Loop(InitialNode, #{InitialNode => 0}, #{}).

from_to(From, To, Offset, Maps, Width, Height) ->
    Cycle = (Width - 2) * (Height - 2),
    NeighbourGetter = fun ({XY, Step}) ->
        StepMap = maps:get((Step + Offset) rem Cycle, Maps),
        [{NXY, Step + 1} || NXY <- next_positions(XY, StepMap, Width, Height)]
    end,
    IsEnd = fun ({XY, _Step}) -> XY =:= To end,
    element(2, dijkstra({From, 1}, NeighbourGetter, IsEnd)).

maps(Map, Width, Height) ->
    {_, Maps} = lists:foldl(fun (I, {MapAcc, MapsAcc}) ->
        NextMap = next_map(MapAcc, Width, Height),
        {NextMap, MapsAcc#{I => MapAcc}}
    end, {Map, #{}}, lists:seq(0, (Width - 2) * (Height - 2) - 1)),
    Maps.

part1() ->
    {Map, Width, Height} = input(),
    from_to({2, 1}, {Width - 1, Height}, 0, maps(Map, Width, Height), Width, Height).

part2() ->
    {Map, Width, Height} = input(),
    Maps = maps(Map, Width, Height),
    Start = {2, 1},
    End = {Width - 1, Height},
    First = from_to(Start, End, 0, Maps, Width, Height),
    Second = from_to(End, Start, First, Maps, Width, Height),
    Third = from_to(Start, End, First + Second, Maps, Width, Height),
    First + Second + Third.