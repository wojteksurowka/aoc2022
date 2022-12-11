-module(day11).
-export([part1/0, part2/0]).

input() ->
    Items = lists:map(fun (List) ->
        lists:reverse([list_to_integer(string:trim(El)) || El <- string:split(List, ",", all)])
    end, aoc_input:read(?MODULE, "Starting items: +(.+)", [list])),
    Test = aoc_input:read(?MODULE, "Test: divisible by +(\\d+)", [integer]),
    True = aoc_input:read(?MODULE, "If true: throw to monkey +(\\d+)", [integer]),
    False = aoc_input:read(?MODULE, "If false: throw to monkey +(\\d+)", [integer]),
    Tests = lists:foldl(fun (X, Acc) -> X * Acc end, 1, Test),
    Operation = lists:map(fun (List) ->
        [ArgL, Op, ArgR] = string:split(List, " ", all),
        operation(ArgL, Op, ArgR, Tests)
    end, aoc_input:read(?MODULE, "Operation: new = +(.+)", [list])),
    list_to_tuple([{I, O, T, Tr, Fa, 0} || {{I, O, T}, {Tr, Fa}} <- lists:zip(lists:zip3(Items, Operation, Test), lists:zip(True, False))]).

operation("old", "+", "old", Tests) ->
    fun (Input) -> (2 * Input) rem Tests end;
operation("old", "*", "old", Tests) ->
    fun (Input) -> (Input * Input) rem Tests end;
operation("old", "+", ArgR, Tests) ->
    IArgR = list_to_integer(ArgR),
    fun (Input) -> (Input + IArgR) rem Tests end;
operation("old", "*", ArgR, Tests) ->
    IArgR = list_to_integer(ArgR),
    fun (Input) -> (Input * IArgR) rem Tests end;
operation(ArgL, "+", "old", Tests) ->
    IArgL = list_to_integer(ArgL),
    fun (Input) -> (IArgL + Input) rem Tests end;
operation(ArgL, "*", "old", Tests) ->
    IArgL = list_to_integer(ArgL),
    fun (Input) -> (IArgL * Input) rem Tests end;
operation(ArgL, "+", ArgR, Tests) ->
    IArgL = list_to_integer(ArgL),
    IArgR = list_to_integer(ArgR),
    Result = (IArgL + IArgR) rem Tests,
    fun (_Input) -> Result end;
operation(ArgL, "*", ArgR, Tests) ->
    IArgL = list_to_integer(ArgL),
    IArgR = list_to_integer(ArgR),
    Result = (IArgL * IArgR) rem Tests,
    fun (_Input) -> Result end.

monkey(Index, Monkeys, DivBy) ->
    {Items, Operation, Test, True, False, Count} = element(Index, Monkeys),
    Len = length(Items),
    MonkeysWithCurrentUpdated = setelement(Index, Monkeys, {[], Operation, Test, True, False, Count + Len}),
    lists:foldr(fun (Item, Acc) ->
        NewItem = case DivBy of
            1 -> Operation(Item);
            _ -> floor(Operation(Item) / DivBy)
        end,
        ThrowTo = case NewItem rem Test == 0 of
            true -> True;
            false -> False
        end,
        TargetMonkey = element(ThrowTo + 1, Acc),
        TargetMonkeyItems = element(1, TargetMonkey),
        UpdatedTargetMonkey = setelement(1, TargetMonkey, [NewItem | TargetMonkeyItems]),
        setelement(ThrowTo + 1, Acc, UpdatedTargetMonkey)
    end, MonkeysWithCurrentUpdated, Items).

monkeys_round(Monkeys, DivBy) ->
    lists:foldl(fun (Index, Acc) ->
        monkey(Index, Acc, DivBy)
    end, Monkeys, lists:seq(1, tuple_size(Monkeys))).

part1() ->
    MonkeysAfter = lists:foldl(fun (_Index, Acc) ->
        monkeys_round(Acc, 3)
    end, input(), lists:seq(1, 20)),
    Counts = [Count || {_, _, _, _, _, Count} <- tuple_to_list(MonkeysAfter)],
    [First, Second | _] = lists:reverse(lists:sort(Counts)),
    First * Second.

part2() ->
    MonkeysAfter = lists:foldl(fun (_Index, Acc) ->
        monkeys_round(Acc, 1)
    end, input(), lists:seq(1, 10000)),
    Counts = [Count || {_, _, _, _, _, Count} <- tuple_to_list(MonkeysAfter)],
    [First, Second | _] = lists:reverse(lists:sort(Counts)),
    First * Second.
