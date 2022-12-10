-module(day10).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "^([a-z]+) ?(.*)?$", [atom, list]).

calc({addx, Op}, Cycle, X, Cycles, Sum) ->
    {Cycle + 2, X + list_to_integer(Op), update_sum(Cycle, X, Cycles, Sum, true)};
calc({noop, []}, Cycle, X, Cycles, Sum) ->
    {Cycle + 1, X, update_sum(Cycle, X, Cycles, Sum, false)}.

update_sum(Cycle, X, Cycles, Sum, Two) ->
    UpdatedSum = case lists:member(Cycle, Cycles) of
        true -> Sum + Cycle * X;
        false -> Sum
    end,
    case Two andalso lists:member(Cycle + 1, Cycles) of
        true -> UpdatedSum + (Cycle + 1) * X;
        false -> UpdatedSum
    end.

calc2({addx, Op}, Cycle, X, Pixels) ->
    {Cycle + 2, X + list_to_integer(Op), update_pixels(Cycle, X, Pixels, true)};
calc2({noop, []}, Cycle, X, Pixels) ->
    {Cycle + 1, X, update_pixels(Cycle, X, Pixels, false)}.

update_pixels(Cycle, X, Pixels, Two) ->
    XCoord = (Cycle - 1) rem 40,
    YCoord = (Cycle - 1) div 40,
    UpdatedPixels = case X - 1 =< XCoord andalso X + 1 >= XCoord of
        true -> [{XCoord, YCoord} | Pixels];
        false -> Pixels
    end,
    XCoord2 = Cycle rem 40,
    YCoord2 = Cycle div 40,
    case Two andalso X - 1 =< XCoord2 andalso X + 1 >= XCoord2 of
        true -> [{XCoord2, YCoord2} | UpdatedPixels];
        false -> UpdatedPixels
    end.

part1() ->
    Cycles = [20, 60, 100, 140, 180, 220],
    lists:foldl(fun (Instr, {CycleAcc, XAcc, SumAcc}) ->
        calc(Instr, CycleAcc, XAcc, Cycles, SumAcc)
    end, {1, 1, 0}, input()).

part2() ->
    {_, _, Pixels} = lists:foldl(fun (Instr, {CycleAcc, XAcc, PixelsAcc}) ->
        calc2(Instr, CycleAcc, XAcc, PixelsAcc)
    end, {1, 1, []}, input()),
    SPixels = sets:from_list(Pixels),
    lists:map(fun (Y) ->
        lists:foldr(fun (X, XAcc) ->
            case sets:is_element({X, Y}, SPixels) of
                true -> [$# | XAcc];
                false -> [$. | XAcc]
            end
        end, [], lists:seq(0, 39))
    end, lists:seq(0, 5)).
