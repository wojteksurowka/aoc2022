-module(day22).
-export([part1/0, part2/0]).

input() ->
    MapList = aoc_input:read(?MODULE, "^([ \\.#]+)$", [tuple]),
    Map = list_to_tuple(MapList),
    Path = lists:reverse(lists:foldl(fun
        (Next, [Steps | T]) when $0 =< Next, Next =< $9, is_number(Steps) ->
            [(Next - $0) + Steps * 10 | T];
        (Next, Acc) when $0 =< Next, Next =< $9 ->
            [Next - $0 | Acc];
        (Next, Acc) when $A =< Next, Next =< $Z ->
            [list_to_atom([Next]) | Acc]
    end, [], hd(aoc_input:read(?MODULE, "^([A-Z0-9]+)$", [list])))),
    Width = lists:max([tuple_size(Row) || Row <- MapList]),
    Height = tuple_size(Map),
    {Map, Width, Height, Path}.

at({X, Y}, Map) ->
    case Y > 0 andalso Y =< tuple_size(Map) of
        true ->
            Row = element(Y, Map),
            case X > 0 andalso X =< tuple_size(Row) of
                true -> element(X, Row);
                false -> $ 
            end;
        false ->
            $ 
    end.

find_begin(Map, Width) ->
    {lists:foldl(fun
        (X, undefined) ->
            case at({X, 1}, Map) of
                $. -> X;
                _ -> undefined
            end;
        (_X, Found) ->
            Found
    end, undefined, lists:seq(1, Width)), 1}.

dir({X, Y}, n) -> {X, Y - 1};
dir({X, Y}, s) -> {X, Y + 1};
dir({X, Y}, e) -> {X + 1, Y};
dir({X, Y}, w) -> {X - 1, Y}.

wrap({X, _Y}, n, _Width, Height) -> {{X, Height}, n};
wrap({X, _Y}, s, _Width, _Height) -> {{X, 1}, s};
wrap({_X, Y}, e, _Width, _Height) -> {{1, Y}, e};
wrap({_X, Y}, w, Width, _Height) -> {{Width, Y}, w}.

reach_map({XY, Dir}, Map) ->
    case at(XY, Map) of
        $  -> reach_map({dir(XY, Dir), Dir}, Map);
        _ -> XY
    end.

next(XY, Dir, Map, Width, Height, Wrap) ->
    Next1 = dir(XY, Dir),
    case at(Next1, Map) of
        $  ->
            reach_map(Wrap(XY, Dir, Width, Height), Map);
        _ ->
            Next1
    end.

move1(XY, Dir, Map, Width, Height, Wrap) ->
    Next = next(XY, Dir, Map, Width, Height, Wrap),
    case at(Next, Map) of
        $# -> XY;
        $. -> Next
    end.

turn(n, 'L') -> w;
turn(s, 'L') -> e;
turn(e, 'L') -> n;
turn(w, 'L') -> s;
turn(n, 'R') -> e;
turn(s, 'R') -> w;
turn(e, 'R') -> s;
turn(w, 'R') -> n.

facing(n) -> 3;
facing(s) -> 1;
facing(e) -> 0;
facing(w) -> 2.

move(XY, Dir, [], _Map, _Width, _Height, _Wrap) ->
    {XY, Dir};
move(XY, Dir, [Steps | T], Map, Width, Height, Wrap) when is_integer(Steps), Steps > 1 ->
    case move1(XY, Dir, Map, Width, Height, Wrap) of
        XY -> move(XY, Dir, T, Map, Width, Height, Wrap);
        Moved -> move(Moved, Dir, [Steps - 1 | T], Map, Width, Height, Wrap)
    end;
move(XY, Dir, [1 | T], Map, Width, Height, Wrap) ->
    move(move1(XY, Dir, Map, Width, Height, Wrap), Dir, T, Map, Width, Height, Wrap);
move(XY, Dir, [Turn | T], Map, Width, Height, Wrap) ->
    move(XY, turn(Dir, Turn), T, Map, Width, Height, Wrap).

part1() ->
    {Map, Width, Height, Path} = input(),
    {{FX, FY}, FDir} = move(find_begin(Map, Width), e, Path, Map, Width, Height, fun wrap/4),
    FY * 1000 + FX * 4 + facing(FDir).

wrap2({X, 1}, n, __Width, _Height) when X >= 51, X =< 100 ->
    {{1, X + 100}, e};
wrap2({51, Y}, w, _Width, _Height) when Y >= 1, Y =< 50 ->
    {{1, 150 - Y + 1}, e};
wrap2({X, 1}, n, _Width, _Height) when X >= 101, X =< 150 ->
    {{X - 100, 200}, n};
wrap2({150, Y}, e, _Width, _Height) when Y >= 1, Y =< 50 ->
    {{100, Y + 100}, w};
wrap2({X, 50}, s, _Width, _Height) when X >= 101, X =< 150 ->
    {{100, X - 50}, w};
wrap2({51, Y}, w, _Width, _Height) when Y >= 51, Y =< 100 ->
    {{Y - 50, 101}, s};
wrap2({100, Y}, e, _Width, _Height) when Y >= 51, Y =< 100 ->
    {{Y + 50, 50}, n};
wrap2({X, 101}, n, _Width, _Height) when X >= 1, X =< 50 ->
    {{51, X + 50}, e};
wrap2({1, Y}, w, _Width, _Height) when Y >= 101, Y =< 150 ->
    {{51, 150 - Y + 1}, e};
wrap2({100, Y}, e, _Width, _Height) when Y >= 101, Y =< 150 ->
    {{150, 150 - Y + 1}, w};
wrap2({X, 150}, s, _Width, _Height) when X >= 51, X =< 100 ->
    {{50, X + 100}, w};
wrap2({1, Y}, w, _Width, _Height) when Y >= 151, Y =< 200 ->
    {{Y - 100, 1}, s};
wrap2({50, Y}, e, _Width, _Height) when Y >= 151, Y =< 200 ->
    {{Y - 100, 150}, n};
wrap2({X, 200}, s, _Width, _Height) when X >= 1, X =< 50 ->
    {{X + 100, 1}, s}.

part2() ->
    {Map, Width, Height, Path} = input(),
    {{FX, FY}, FDir} = move(find_begin(Map, Width), e, Path, Map, Width, Height, fun wrap2/4),
    FY * 1000 + FX * 4 + facing(FDir).