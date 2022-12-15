-module(day15).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "^Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)", [integer, integer, integer, integer]).

single_blocks(Input, TargetRow) ->
    lists:foldl(fun
        ({SX, SY, BX, BY}, Acc) ->
            Distance = abs(SX - BX) + abs(SY - BY),
            Left = Distance - abs(TargetRow - SY),
            case Left >= 0 of
                true ->
                    case TargetRow of
                        BY -> add_ranges(SX - Left, SX + Left, BX, Acc);
                        _ -> add_ranges(SX - Left, SX + Left, SX - Left - 1, Acc)
                    end;
                false ->
                    Acc
            end
    end, [], Input).

blocks(Input, TargetRow) ->
    SingleBlocks = single_blocks(Input, TargetRow),
    merge(lists:usort(SingleBlocks)).

merge([{From1, To1}, {From2, To2} | T]) when To1 >= From2 ->
    merge([{From1, max(To1, To2)} | T]);
merge([{From, To} | T]) ->
    [{From, To} | merge(T)];
merge([]) ->
    [].

add_ranges(From, From, From, Acc) ->
    Acc;
add_ranges(From, To, Except, Acc) when Except =< From ->
    [{max(From, Except + 1), To} | Acc];
add_ranges(From, To, Except, Acc) when Except >= To ->
    [{From, min(To, Except - 1)} | Acc];
add_ranges(From, To, Except, Acc) ->
    [{From, Except - 1}, {Except + 1, To} | Acc].

part1() ->
    TargetRow = 2000000,
    lists:sum([To - From + 1 || {From, To} <- blocks(input(), TargetRow)]).

single_gap([{_From, To} | T], Max) when To < 0 ->
    single_gap(T, Max);
single_gap([{_From1, To1}, {From2, _To2} | _T], Max) when To1 >= -1, To1 < Max, From2 - To1 == 2 ->
    To1 + 1;
single_gap(_Blocks, _Max) ->
    undefined.

part2() ->
    Input = input(),
    Max = 4000000,
    Beacons = sets:from_list([{BX, BY} || {_SX, _SY, BX, BY} <- Input]),
    {value, FoundY} = lists:search(fun (Y) ->
        case single_gap(blocks(Input, Y), Max) of
            undefined -> false;
            X -> not sets:is_element({X, Y}, Beacons)
        end
    end, lists:seq(0, Max)),
    FoundX = single_gap(blocks(Input, FoundY), Max),
    FoundX * 4000000 + FoundY.