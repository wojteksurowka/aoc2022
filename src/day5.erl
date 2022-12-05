-module(day5).
-export([part1/0, part2/0]).

input() ->
    MaybeShorter = [take(1, Line) || Line <- aoc_input:read(?MODULE, "^(.*\\[[A-Z]\\].*)$", [binary])],
    MaxLen = lists:max([length(Line) || Line <- MaybeShorter]),
    Canonical = lists:map(fun (Line) ->
        LineLen = length(Line),
        case LineLen < MaxLen of
            true -> iolist_to_binary(Line ++ lists:duplicate(MaxLen - LineLen, <<" ">>));
            false -> iolist_to_binary(Line)
        end
    end, MaybeShorter),
    Stacks = list_to_tuple(lists:map(fun (Index) ->
        lists:foldr(fun (Line, Acc) ->
            case binary:part(Line, Index, 1) of
                <<" ">> -> Acc;
                <<C:8>> -> [C | Acc]
            end
        end, [], Canonical)
    end, lists:seq(0, MaxLen - 1))),
    Moves = aoc_input:read(?MODULE, "move (\\d+) from (\\d+) to (\\d+)", [integer, integer, integer]),
    {Stacks, Moves}.

take(Index, Line) ->
    case Index < byte_size(Line) of
        true -> [binary:part(Line, Index, 1) | take(Index + 4, Line)];
        false -> []
    end.

move({Count, From, To}, Stacks) when Count > 1 ->
    move({Count - 1, From, To}, move({1, From, To}, Stacks));
move({1, From, To}, Stacks) ->
    Top = hd(element(From, Stacks)),
    MovedFrom = setelement(From, Stacks, tl(element(From, Stacks))),
    setelement(To, MovedFrom, [Top | element(To, MovedFrom)]).

move2({Count, From, To}, Stacks) ->
    Tops = lists:sublist(element(From, Stacks), Count),
    MovedFrom = setelement(From, Stacks, lists:nthtail(Count, element(From, Stacks))),
    setelement(To, MovedFrom, Tops ++ element(To, MovedFrom)).

part1() ->
    {Stacks, Moves} = input(),
    [Top || [Top | _] <- tuple_to_list(lists:foldl(fun move/2, Stacks, Moves))].

part2() ->
    {Stacks, Moves} = input(),
    [Top || [Top | _] <- tuple_to_list(lists:foldl(fun move2/2, Stacks, Moves))].