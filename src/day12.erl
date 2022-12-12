-module(day12).
-export([part1/0, part2/0]).

input() ->
    list_to_tuple(aoc_input:read(?MODULE, "^(.+)$", [tuple])).

bimap_new() ->
    {#{}, #{}}.

bimap_get(Key, {K2V, _V2K}) ->
    maps:get(Key, K2V, undefined).

bimap_set(Key, Value, {K2V, V2K}) ->
    V2KWithout = case K2V of
        #{Key := PrevValue} ->
            case V2K of
                #{PrevValue := [Key]} -> maps:remove(PrevValue, V2K);
                #{PrevValue := PrevKeys} -> V2K#{PrevValue => lists:delete(Key, PrevKeys)}
            end;
        _ ->
            V2K
    end,
    V2KWithNew = case V2KWithout of
        #{Value := Keys} -> V2KWithout#{Value := [Key | Keys]};
        _ -> V2KWithout#{Value => [Key]}
    end,
    {K2V#{Key => Value}, V2KWithNew}.

bimap_remove(Key, {K2V, V2K}) ->
    case maps:take(Key, K2V) of
        {Value, K2VWithout} ->
            V2kWithout = case V2K of
                #{Value := [Key]} -> maps:remove(Value, V2K);
                #{Value := Keys} -> V2K#{Value => lists:delete(Key, Keys)}
            end,
            {K2VWithout, V2kWithout};
        error ->
            {K2V, V2K}
    end.

bimap_key_with_min_value({_K2V, V2K}) ->
    case lists:sort(maps:keys(V2K)) of
        [] -> undefined;
        [Value | _] -> hd(maps:get(Value, V2K))
    end.

width(Grid) -> tuple_size(element(1, Grid)).

height(Grid) -> tuple_size(Grid).

el({X, Y}, Grid) -> element(X, element(Y, Grid)).

neighbours({X, Y}, Grid) ->
    Width = width(Grid),
    Height = height(Grid),
    All = [{X - 1, Y}, {X + 1, Y}, {X, Y - 1}, {X, Y + 1}],
    Current = case el({X, Y}, Grid) of
        $S -> $a;
        C -> C
    end,
    lists:filter(fun ({NX, NY}) ->
        case NX >= 1 andalso NX =< Width andalso NY >= 1 andalso NY =< Height of
            true ->
                Neighbour = case el({NX, NY}, Grid) of
                    $E -> $z;
                    N -> N
                end,
                Neighbour == $E orelse Neighbour =< Current + 1;
            false ->
                false
        end
    end, All).

distance(XY, Unvisited, Visited) ->
    case Visited of
        #{XY := D} -> D;
        _ -> bimap_get(XY, Unvisited)
    end.

dijkstra(Current, Unvisited, Visited, Grid) ->
    CurrentDistance = distance(Current, Unvisited, Visited),
    UpdatedUnvisited = lists:foldl(fun (Neighbour, Acc) ->
        case maps:is_key(Neighbour, Visited) of
            false ->
                case bimap_get(Neighbour, Acc) of
                    undefined ->
                        bimap_set(Neighbour, CurrentDistance + 1, Acc);
                    NeighbourDistance when NeighbourDistance > CurrentDistance + 1->
                        bimap_set(Neighbour, CurrentDistance + 1, Acc);
                    _NeighbourDistance ->
                        Acc
                end;
            true ->
                Acc
        end
    end, Unvisited, neighbours(Current, Grid)),
    UnvisitedWithout = bimap_remove(Current, UpdatedUnvisited),
    VisitedWith = Visited#{Current => CurrentDistance},
    case el(Current, Grid) of
        $E ->
            CurrentDistance;
        _ ->
            case bimap_key_with_min_value(UnvisitedWithout) of
                undefined ->
                    undefined;
                NextCurrent ->
                    dijkstra(NextCurrent, UnvisitedWithout, VisitedWith, Grid)
            end
    end.

part1() ->
    Grid = input(),
    [Start] = [{X, Y} || X <- lists:seq(1, width(Grid)), Y <- lists:seq(1, height(Grid)), el({X, Y}, Grid) == $S],
    dijkstra(Start, bimap_set(Start, 0, bimap_new()), #{}, Grid).

part2() ->
    Grid = input(),
    Starts = [{X, Y} || X <- lists:seq(1, width(Grid)), Y <- lists:seq(1, height(Grid)), el({X, Y}, Grid) == $a orelse el({X, Y}, Grid) == $S],
    lists:min([dijkstra(Start, bimap_set(Start, 0, bimap_new()), #{}, Grid) || Start <- Starts]).
