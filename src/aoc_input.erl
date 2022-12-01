-module(aoc_input).
-export([read/3]).

read(Module, RE, Types) ->
    [$d, $a, $y | Day] = atom_to_list(Module),
    {ok, Input} = file:read_file("input/day" ++ Day ++ ".in"),
    Lines = binary:split(Input, <<"\n">>, [global, trim_all]),
    {ok, CompiledRE} = re:compile(RE),
    lists:filtermap(fun (Line) ->
        case re:run(Line, CompiledRE) of
            {match, [_All | Groups]} ->
                Converted = lists:map(fun
                    ({Group, atom}) -> binary_to_atom(binary:part(Line, Group));
                    ({Group, integer}) -> binary_to_integer(binary:part(Line, Group));
                    ({Group, character}) -> hd(binary_to_list(binary:part(Line, Group)));
                    ({Group, binary}) -> binary:part(Line, Group);
                    ({Group, list}) -> binary_to_list(binary:part(Line, Group))
                end, lists:zip(Groups, Types)),
                case Types of
                    [_Single] -> {true, hd(Converted)};
                    _ -> {true, list_to_tuple(Converted)}
                end;
            _ ->
                false
        end
    end, Lines).
