-module(day7).
-export([part1/0, part2/0]).

input() ->
    lists:foldr(fun
        ("$ ls", [Output | T]) when is_list(Output) ->
            [{"ls", Output} | T];
        ([$$, $  | Command], Acc) ->
            [{Command, []} | Acc];
        (OutputLine, [Output | T]) when is_list(Output) ->
            [[OutputLine | Output] | T];
        (OutputLine, Acc) ->
            [[OutputLine] | Acc]
    end, [], aoc_input:read(?MODULE, "^(.+)$", [list])).

add_size(Dir, Size, Acc) ->
    UpdatedAcc = Acc#{Dir => Size + maps:get(Dir, Acc, 0)},
    case Dir of
        [] -> UpdatedAcc;
        [_Head | T] -> add_size(T, Size, UpdatedAcc)
    end.

total_sizes() ->
    {_LastDir, Sizes} = lists:foldl(fun
        ({"cd /", _}, {_CurDir, Sizes}) ->
            {[], Sizes};
        ({"cd ..", _}, {[_Head | T], Sizes}) ->
            {T, Sizes};
        ({[$c, $d, $  | Dir], _}, {CurDir, Sizes}) ->
            {[Dir | CurDir], Sizes};
        ({"ls", Output}, {CurDir, Sizes}) ->
            DirSize = lists:foldl(fun (SizeName, Acc) ->
                case string:split(SizeName, " ") of
                    ["dir", _Name] -> Acc;
                    [Size, _Name] -> Acc + list_to_integer(Size)
                end
            end, 0, Output),
            {CurDir, Sizes#{CurDir => DirSize}}
    end, {[], #{}}, input()),
    maps:fold(fun add_size/3, #{}, Sizes).

part1() ->
    lists:sum(lists:filter(fun (Size) -> Size =< 100000 end, maps:values(total_sizes()))).

part2() ->
    TotalSizes = total_sizes(),
    Need = 30000000 - (70000000 - maps:get([], TotalSizes)),
    lists:search(fun (Size) -> Size >= Need end, lists:sort(maps:values(TotalSizes))).