-module(day16).
-export([part1/0, part2/0]).

-record(valve, {flow, tunnels, open = false}).
-record(opener, {location, visited_since_open = sets:new()}).
-record(state, {valves, opened_flow = 0, left_to_open}).

input() ->
    VFL = aoc_input:read(?MODULE, "^Valve (..) has flow rate=(\\d+); tunnels? leads? to valves? (.+)$", [atom, integer, binary]),
    maps:from_list(lists:map(fun ({Valve, Flow, Tunnels}) ->
        {
            Valve,
            #valve{
                flow = Flow,
                tunnels = maps:from_list([{binary_to_atom(V), 1} || V <- string:split(Tunnels, ", ", all)])
            }
        }
    end, VFL)).

remove_zero_valves_except(Input, Except) ->
    case lists:search(fun ({N, V}) -> V#valve.flow == 0 andalso N =/= Except end, maps:to_list(Input)) of
        {value, {Name, _Valve}} ->
            remove_zero_valves_except(remove_valve(Name, Input), Except);
        false ->
            Input
    end.

non_zero_valves_and_starting_points(Input, Start) ->
    AllButOne = remove_zero_valves_except(Input, Start),
    StartValve = maps:get(Start, AllButOne),
    case StartValve#valve.flow of
        0 ->
            {tunnels_to_list(remove_valve(Start, AllButOne)), maps:to_list(StartValve#valve.tunnels)};
        _ ->
            {tunnels_to_list(AllButOne), [Start]}
    end.

tunnels_to_list(Valves) ->
    maps:map(fun (_N, V) -> V#valve{tunnels = maps:to_list(V#valve.tunnels)} end, Valves).

remove_valve(Name, Valves) ->
    maps:fold(fun (N, V, Acc) ->
        UpdatedTunnels = case maps:take(Name, V#valve.tunnels) of
            {Distance, TunnelsWithout} ->
                Valve = maps:get(Name, Valves),
                maps:fold(fun
                    (T, _D, TAcc) when T =:= N ->
                        TAcc;
                    (T, D, TAcc) ->
                        case TAcc of
                            #{T := PD} -> TAcc#{T => min(PD, Distance + D)};
                            _ -> TAcc#{T => Distance + D}
                        end
                end, TunnelsWithout, Valve#valve.tunnels);
            error ->
                V#valve.tunnels
        end,
        Acc#{N => V#valve{tunnels = UpdatedTunnels}}
    end, #{}, maps:without([Name], Valves)).

next_steps(#opener{location = {Target, 1}} = Opener, State) ->
    UpdatedVSO = sets:add_element(Target, Opener#opener.visited_since_open),
    [{Opener#opener{location = Target, visited_since_open = UpdatedVSO}, State}];
next_steps(#opener{location = {Target, Distance}} = Opener, State) ->
    [{Opener#opener{location = {Target, Distance - 1}}, State}];
next_steps(Opener, #state{left_to_open = 0} = State) ->
    [{Opener, State}];
next_steps(Opener, State) ->
    Valve = maps:get(Opener#opener.location, State#state.valves),
    Tunnels = lists:filtermap(fun ({V, D}) ->
        case {sets:is_element(V, Opener#opener.visited_since_open), D} of
            {true, _} ->
                false;
            {false, 1} ->
                UpdatedVSO = sets:add_element(V, Opener#opener.visited_since_open),
                {true, {Opener#opener{location = V, visited_since_open = UpdatedVSO}, State}};
            {false, _} ->
                {true, {Opener#opener{location = {V, D - 1}}, State}}
        end
    end, Valve#valve.tunnels),
    OpenAndTunnels = case Valve#valve.open of
        false ->
            [{Opener#opener{visited_since_open = sets:new()}, open(Opener#opener.location, State)} | Tunnels];
        true ->
            Tunnels
    end,
    case OpenAndTunnels of
        [] -> [{Opener, State}];
        _ -> OpenAndTunnels
    end.

open(Name, State) ->
    Valves = State#state.valves,
    Valve = maps:get(Name, Valves),
    UpdatedValves = Valves#{Name := Valve#valve{open = true}},
    State#state{
        valves = UpdatedValves,
        opened_flow = State#state.opened_flow + Valve#valve.flow,
        left_to_open = State#state.left_to_open - 1
    }.

find1(_Opener, _State, 0, Total) ->
    Total;
find1(Opener, State, Remaining, Total) ->
    Total + lists:max(lists:map(fun ({O, S}) ->
        find1(O, S, Remaining - 1, State#state.opened_flow)
    end, next_steps(Opener, State))).

find2(_Opener1, _Opener2, _State, 0, Total) ->
    Total;
find2(Opener1, Opener2, State, Remaining, Total) ->
    Total + lists:max(lists:map(fun ({O1, S1}) ->
        lists:max(lists:map(fun ({O2, S2}) ->
            find2(O1, O2, S2, Remaining - 1, S1#state.opened_flow)
        end, next_steps(Opener2, S1)))
    end, next_steps(Opener1, State))).

find2_wrapper(Opener1, Opener2, State, Remaining, Total) ->
    io:format("~p ~p~n", [Opener1#opener.location, Opener2#opener.location]),
    find2(Opener1, Opener2, State, Remaining, Total).

part1() ->
    {Valves, Starts} = non_zero_valves_and_starting_points(input(), 'AA'),
    lists:max([find1(#opener{location = S}, #state{valves = Valves, left_to_open = maps:size(Valves)}, 30, 0) || S <- Starts]).

part2() ->
    {Valves, Starts} = non_zero_valves_and_starting_points(input(), 'AA'),
    State = #state{valves = Valves, left_to_open = maps:size(Valves)},
    lists:max([find2_wrapper(#opener{location = Start1}, #opener{location = Start2}, State, 26, 0) || Start1 <- Starts, Start2 <- Starts]).
