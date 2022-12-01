-module(aoc2022_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aoc2022_sup:start_link().

stop(_State) ->
    ok.

start() ->
    Module = find_last_day(1),
    {module, _} = code:ensure_loaded(Module),
    Exports = Module:module_info(exports),
    Function = case {lists:member({part1, 0}, Exports), lists:member({part2, 0}, Exports)} of
        {_, true} ->
            part2;
        {true, _} ->
            part1;
        _ ->
            throw("Part function not exported")
    end,
    Result = Module:Function(),
    Day = lists:nthtail(3, atom_to_list(Module)),
    Part = lists:nthtail(4, atom_to_list(Function)),
    io:format("~nDay ~s Part ~s: ~p~n~n", [Day, Part, Result]),
    halt().

find_last_day(N) ->
    case code:which(list_to_atom("day" ++ integer_to_list(N + 1))) of
        non_existing -> list_to_atom("day" ++ integer_to_list(N));
        _ -> find_last_day(N + 1)
    end.