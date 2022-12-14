-module(day14).
-export([part1/0, part2/0]).

input() ->
    lists:map(fun (P) ->
        lists:map(fun (C) ->
            [X, Y] = string:split(C, ","),
            {binary_to_integer(X), binary_to_integer(Y)}
        end, string:split(P, " -> ", all))
    end, aoc_input:read(?MODULE, "^(.+)$", [binary])).

expand_paths(Paths) ->
    sets:from_list(lists:flatten([expand_path(P, []) || P <- Paths])).

next(A, B) when A < B -> A + 1;
next(A, B) when A > B -> A - 1.

expand_path([], Acc) ->
    Acc;
expand_path([XY], Acc) ->
    [XY | Acc];
expand_path([{X, Y}, {X, Y} | T], Acc) ->
    expand_path([{X, Y} | T], [{X, Y} | Acc]);
expand_path([{X, Y1}, {X, Y2} | T], Acc) ->
    expand_path([{X, next(Y1, Y2)}, {X, Y2} | T], [{X, Y1} | Acc]);
expand_path([{X1, Y}, {X2, Y} | T], Acc) ->
    expand_path([{next(X1, X2), Y}, {X2, Y} | T], [{X1, Y} | Acc]).

drop({_X, Y}, _Grid, MaxY, _Blocked) when Y >= MaxY ->
    abyss;
drop({X, Y}, Grid, MaxY, Blocked) ->
    case Blocked({X, Y}, Grid) of
        false ->
            case Blocked({X, Y + 1}, Grid) of
                false ->
                    drop({X, Y + 1}, Grid, MaxY, Blocked);
                true ->
                    case Blocked({X - 1, Y + 1}, Grid) of
                        false ->
                            drop({X - 1, Y + 1}, Grid, MaxY, Blocked);
                        true ->
                            case Blocked({X + 1, Y + 1}, Grid) of
                                false -> drop({X + 1, Y + 1}, Grid, MaxY, Blocked);
                                true -> sets:add_element({X, Y}, Grid)
                            end
                    end
            end;
        true ->
            abyss
    end.

units_till_abyss(Start, Grid, MaxY, Blocked, Acc) ->
    case drop(Start, Grid, MaxY, Blocked) of
        abyss -> Acc;
        NextGrid -> units_till_abyss(Start, NextGrid, MaxY, Blocked, Acc + 1)
    end.

part1() ->
    Rocks = expand_paths(input()),
    MaxY = lists:max(element(2, lists:unzip(sets:to_list(Rocks)))),
    units_till_abyss({500, 0}, Rocks, MaxY, fun sets:is_element/2, 0).

part2() ->
    Rocks = expand_paths(input()),
    MaxY = lists:max(element(2, lists:unzip(sets:to_list(Rocks)))),
    Blocked = fun
        ({_X, Y}, _Grid) when Y == MaxY + 2 -> true;
        ({X, Y}, Grid) -> sets:is_element({X, Y}, Grid)
    end,
    units_till_abyss({500, 0}, Rocks, MaxY + 3, Blocked, 0).
