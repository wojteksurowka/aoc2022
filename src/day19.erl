-module(day19).
-export([part1/0, part2/0]).

-record(blueprint, {id, ore, clay, obsidian, geode}).
-record(cost, {ore, clay = 0, obsidian = 0}).
-record(robots, {ore = 1, clay = 0, obsidian = 0, geode = 0}).
-record(state, {robots = #robots{}, ore = 0, clay = 0, obsidian = 0, geode = 0}).

input() ->
    Lines = aoc_input:read(?MODULE, "^Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.$", [integer, integer, integer, integer, integer, integer, integer]),
    [#blueprint{id = ID, ore = #cost{ore = OreOre}, clay = #cost{ore = ClayOre}, obsidian = #cost{ore = ObsidianOre, clay = ObsidianClay}, geode = #cost{ore = GeodeOre, obsidian = GeodeObsidian}} || {ID, OreOre, ClayOre, ObsidianOre, ObsidianClay, GeodeOre, GeodeObsidian} <- Lines].

buy_cost_count(_Cost, 0, State) ->
    State;
buy_cost_count(Cost, 1, State) ->
    State#state{
        ore = State#state.ore - Cost#cost.ore,
        clay = State#state.clay - Cost#cost.clay,
        obsidian = State#state.obsidian - Cost#cost.obsidian
    }.

buy(#robots{ore = 0, clay = 0, obsidian = 0, geode = 0}, _Blueprint, State) ->
    State;
buy(Robots, Blueprint, State) ->
    Ore = buy_cost_count(Blueprint#blueprint.ore, Robots#robots.ore, State),
    Clay = buy_cost_count(Blueprint#blueprint.clay, Robots#robots.clay, Ore),
    Obsidian = buy_cost_count(Blueprint#blueprint.obsidian, Robots#robots.obsidian, Clay),
    buy_cost_count(Blueprint#blueprint.geode, Robots#robots.geode, Obsidian).

collect(State) ->
    State#state{
        ore = State#state.ore + State#state.robots#robots.ore,
        clay = State#state.clay + State#state.robots#robots.clay,
        obsidian = State#state.obsidian + State#state.robots#robots.obsidian,
        geode = State#state.geode + State#state.robots#robots.geode
    }.

add(Robots, State) ->
    State#state{
        robots = #robots{
            ore = Robots#robots.ore + State#state.robots#robots.ore,
            clay = Robots#robots.clay + State#state.robots#robots.clay,
            obsidian = Robots#robots.obsidian + State#state.robots#robots.obsidian,
            geode = Robots#robots.geode + State#state.robots#robots.geode
        }
    }.

minute(Robots, Blueprint, State) ->
    add(Robots, collect(buy(Robots, Blueprint, State))).

can_buy_one(Blueprint, BlueprintIndex, CostIndex, RobotsIndex, State) ->
    MakeSense = case CostIndex of
        undefined ->
            true;
        _ ->
            MaxCost = lists:max([
                element(CostIndex, Blueprint#blueprint.geode),
                element(CostIndex, Blueprint#blueprint.obsidian),
                element(CostIndex, Blueprint#blueprint.clay),
                element(CostIndex, Blueprint#blueprint.ore)
            ]),
            element(RobotsIndex, State#state.robots) < MaxCost
    end,
    case MakeSense of
        true ->
            Cost = element(BlueprintIndex, Blueprint),
            EnoughOre = State#state.ore >= Cost#cost.ore,
            EnoughOreAndClay = EnoughOre andalso State#state.clay >= Cost#cost.clay,
            EnoughOreAndClay andalso State#state.obsidian >= Cost#cost.obsidian;
        false ->
            false
    end.

can_buy(_Blueprint, 1, _State) ->
    [#robots{ore = 0, clay = 0, obsidian = 0, geode = 0}];
can_buy(Blueprint, 2, State) ->
    case can_buy_one(Blueprint, #blueprint.geode, undefined, undefined, State) of
        false -> [#robots{ore = 0, clay = 0, obsidian = 0, geode = 0}];
        true -> [#robots{ore = 0, clay = 0, obsidian = 0, geode = 0}, #robots{ore = 0, clay = 0, obsidian = 0, geode = 1}]
    end;
can_buy(Blueprint, Remaining, State) ->
    Init = [#robots{ore = 0, clay = 0, obsidian = 0, geode = 0}],
    WithGeode = case can_buy_one(Blueprint, #blueprint.geode, undefined, undefined, State) of
        false -> Init;
        true -> [#robots{ore = 0, clay = 0, obsidian = 0, geode = 1} | Init]
    end,
    WithObsidian = case can_buy_one(Blueprint, #blueprint.obsidian, #cost.obsidian, #robots.obsidian, State) of
        false -> WithGeode;
        true -> [#robots{ore = 0, clay = 0, obsidian = 1, geode = 0} | WithGeode]
    end,
    WithClay = case Remaining > 3 andalso can_buy_one(Blueprint, #blueprint.clay, #cost.clay, #robots.clay, State) of
        false -> WithObsidian;
        true -> [#robots{ore = 0, clay = 1, obsidian = 0, geode = 0} | WithObsidian]
    end,
    case can_buy_one(Blueprint, #blueprint.ore, #cost.ore, #robots.ore, State) of
        false -> WithClay;
        true -> [#robots{ore = 1, clay = 0, obsidian = 0, geode = 0} | WithClay]
    end.

next(_Blueprint, 0, State, _CBBD, Cache) ->
    {State#state.geode, Cache};
next(Blueprint, Remaining, State, CBBD, Cache) ->
    Key = {Remaining, State#state{geode = 0}},
    case maps:get(Key, Cache, undefined) of
        undefined ->
            CanBuy = can_buy(Blueprint, Remaining, State),
            CouldBuy = case CanBuy of
                [_] -> false;
                _ -> true
            end,
            FinalCanBuy = case CBBD of
                true -> lists:delete(#robots{ore = 0, clay = 0, obsidian = 0, geode = 0}, CanBuy);
                false -> CanBuy
            end,
            {Geode, CacheAfter} = lists:foldl(fun (Robots, {GeodeAcc, CacheAcc}) ->
                ButDidnt = Robots =:= #robots{ore = 0, clay = 0, obsidian = 0, geode = 0},
                NextMinute = minute(Robots, Blueprint, State#state{geode = 0}),
                {NextGeode, NextCache} = next(Blueprint, Remaining - 1, NextMinute, CouldBuy andalso ButDidnt, CacheAcc),
                {max(NextGeode, GeodeAcc), NextCache}
            end, {0, Cache}, FinalCanBuy),
            {Geode + State#state.geode, CacheAfter#{Key => Geode}};
        Geode ->
            {Geode + State#state.geode, Cache}
    end.

part1() ->
    lists:sum([element(1, next(Blueprint, 24, #state{}, false, #{})) * Blueprint#blueprint.id || Blueprint <- input()]).

part2() ->
    [X, Y, Z] = [element(1, next(Blueprint, 32, #state{}, false, #{})) || Blueprint <- lists:sublist(input(), 3)],
    X * Y * Z.
