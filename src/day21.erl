-module(day21).
-export([part1/0, part2/0]).

input() ->
    Numbers = maps:from_list(aoc_input:read(?MODULE, "^(....): (\\d+)$", [atom, integer])),
    Expressions = maps:from_list([{T, {L, O, R}} || {T, L, O, R} <- aoc_input:read(?MODULE, "^(....): (....) (.) (....)$", [atom, atom, atom, atom])]),
    {Numbers, Expressions}.

calculate(LValue, '+', RValue) when is_number(LValue), is_number(RValue) -> LValue + RValue;
calculate(LValue, '-', RValue) when is_number(LValue), is_number(RValue) -> LValue - RValue;
calculate(LValue, '*', RValue) when is_number(LValue), is_number(RValue) -> LValue * RValue;
calculate(LValue, '/', RValue) when is_number(LValue), is_number(RValue) -> LValue / RValue;
calculate(LValue, Op, RValue) -> {LValue, Op, RValue}.

reduce({Numbers, Expressions}) when map_size(Expressions) =:= 0 ->
    Numbers;
reduce({Numbers, Expressions}) ->
    reduce(maps:fold(fun (T, {L, O, R}, {NAcc, EAcc}) ->
        case NAcc of
            #{L := LValue, R := RValue} ->
                {NAcc#{T => calculate(LValue, O, RValue)}, EAcc};
            _ ->
                {NAcc, EAcc#{T => {L, O, R}}}
        end
    end, {Numbers, #{}}, Expressions)).

part1() ->
    maps:get(root, reduce(input())).

calculateL(H, L, Numbers, ExpressionsWithoutRoot) ->
    maps:get(L, reduce({Numbers#{humn => H}, ExpressionsWithoutRoot})).

find_initial_range(N, Calculate, Constant) ->
    ValueL = Calculate(-N),
    ValueR = Calculate(N),
    case ValueL < Constant andalso ValueR > Constant orelse ValueL > Constant andalso ValueR < Constant of
        true -> N;
        false -> find_initial_range(N * 10, Calculate, Constant)
    end.

bisect(From, To, _Calculate, _Constant) when To - From == 2 ->
    From + 1;
bisect(From, To, Calculate, Constant) ->
    FromValue = Calculate(From),
    ToValue = Calculate(To),
    Mid = From + (To - From) div 2,
    MidValue = Calculate(Mid),
    case {FromValue == Constant, ToValue == Constant, FromValue < ToValue} of
        {true, _, _} -> From;
        {false, true, _} -> To;
        {false, false, true} ->
            if
                MidValue > Constant -> bisect(From, Mid, Calculate, Constant);
                MidValue < Constant -> bisect(Mid, To, Calculate, Constant);
                true -> Mid
            end;
        {false, false, false} ->
            if
                MidValue > Constant -> bisect(Mid, To, Calculate, Constant);
                MidValue < Constant -> bisect(From, Mid, Calculate, Constant);
                true -> Mid
            end
    end.

part2() ->
    {Numbers, Expressions} = input(),
    {{L, _, R}, ExpressionsWithoutRoot} = maps:take(root, Expressions),
    Constant = maps:get(R, reduce({Numbers#{humn => humn}, ExpressionsWithoutRoot})),
    Calculate = fun (H) -> calculateL(H, L, Numbers, ExpressionsWithoutRoot) end,
    Range = find_initial_range(1, Calculate, Constant),
    bisect(-Range, Range, Calculate, Constant).
