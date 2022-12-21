-module(day21).
-export([part1/0, part2/0]).

input() ->
    Numbers = maps:from_list(aoc_input:read(?MODULE, "^(....): (\\d+)$", [atom, integer])),
    Expressions = maps:from_list([{T, {L, O, R}} || {T, L, O, R} <- aoc_input:read(?MODULE, "^(....): (....) (.) (....)$", [atom, atom, atom, atom])]),
    {Numbers, Expressions}.

calculate(LValue, '+', RValue) -> LValue + RValue;
calculate(LValue, '-', RValue) -> LValue - RValue;
calculate(LValue, '*', RValue) -> LValue * RValue;
calculate(LValue, '/', RValue) -> round(LValue / RValue).

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

find_initial_range(N, Calculate) ->
    ValueL = Calculate(-N),
    ValueR = Calculate(N),
    case ValueL < 0 andalso ValueR > 0 orelse ValueL > 0 andalso ValueR < 0 of
        true -> {-N, N};
        false -> find_initial_range(N * 10, Calculate)
    end.

bisect({From, To}, Calculate) ->
    FromValue = Calculate(From),
    ToValue = Calculate(To),
    Mid = From + (To - From) div 2,
    case Calculate(Mid) of
        0 ->
            Mid;
        MidValue ->
            case (FromValue < ToValue) =:= (MidValue > 0) of
                true -> bisect({From, Mid}, Calculate);
                false -> bisect({Mid, To}, Calculate)
            end
    end.

part2() ->
    {Numbers, Expressions} = input(),
    {{L, _, R}, ExpressionsWithoutRoot} = maps:take(root, Expressions),
    Calculate = fun (H) ->
        Reduced = reduce({Numbers#{humn => H}, ExpressionsWithoutRoot}),
        maps:get(L, Reduced) - maps:get(R, Reduced)
    end,
    bisect(find_initial_range(1, Calculate), Calculate).