-module(day19).
-export([part1/0]).

-record(blueprint, {id, ore, clay, obsidian, geode}).
-record(cost, {ore, clay = 0, obsidian = 0}).
-record(robots, {ore = 1, clay = 0, obsidian = 0, geode = 0}).
-record(state, {robots = #robots{}, ore = 0, clay = 0, obsidian = 0, geode = 0}).

input() ->
    Lines = aoc_input:read(?MODULE, "^Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.$", [integer, integer, integer, integer, integer, integer, integer]),
    [#blueprint{id = ID, ore = #cost{ore = OreOre}, clay = #cost{ore = ClayOre}, obsidian = #cost{ore = ObsidianOre, clay = ObsidianClay}, geode = #cost{ore = GeodeOre, obsidian = GeodeObsidian}} || {ID, OreOre, ClayOre, ObsidianOre, ObsidianClay, GeodeOre, GeodeObsidian} <- Lines].

buy_cost_count(Cost, Count, State) ->
    State#state{
        ore = State#state.ore - Count * Cost#cost.ore,
        clay = State#state.clay - Count * Cost#cost.clay,
        obsidian = State#state.obsidian - Count * Cost#cost.obsidian
    }.

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

can_buy_at_most(Cost, State) ->
    OreAtMost = State#state.ore div Cost#cost.ore,
    ClayAtMost = case Cost#cost.clay of
        0 -> OreAtMost;
        _ -> min(OreAtMost, State#state.clay div Cost#cost.clay)
    end,
    ObsidianAtMost = case Cost#cost.obsidian of
        0 -> ClayAtMost;
        _ -> min(ClayAtMost, State#state.obsidian div Cost#cost.obsidian)
    end,
    max(0, ObsidianAtMost).

can_buy(Blueprint, State) ->
    Geode = can_buy_at_most(Blueprint#blueprint.geode, State),
    StateAfterBuyingGeode = buy_cost_count(Blueprint#blueprint.geode, Geode, State),
    CanBuyObsidian = case Geode of
        0 -> [can_buy_at_most(Blueprint#blueprint.obsidian, StateAfterBuyingGeode)];
        _ -> lists:seq(0, can_buy_at_most(Blueprint#blueprint.obsidian, StateAfterBuyingGeode))
    end,
    lists:foldl(fun (Obsidian, ObsidianAcc) ->
        StateAfterBuyingObsidian = buy_cost_count(Blueprint#blueprint.obsidian, Obsidian, StateAfterBuyingGeode),
        lists:foldl(fun (Clay, ClayAcc) ->
            StateAfterBuyingClay = buy_cost_count(Blueprint#blueprint.clay, Clay, StateAfterBuyingObsidian),
            lists:foldl(fun (Ore, OreAcc) ->
                [#robots{ore = Ore, clay = Clay, obsidian = Obsidian, geode = Geode} | OreAcc]
            end, ClayAcc, lists:seq(0, can_buy_at_most(Blueprint#blueprint.ore, StateAfterBuyingClay)))
        end, ObsidianAcc, lists:seq(0, can_buy_at_most(Blueprint#blueprint.clay, StateAfterBuyingObsidian)))
    end, [], CanBuyObsidian).

next(_Blueprint, 0, State, Cache) ->
    {State#state.geode, Cache};
next(Blueprint, Remaining, State, Cache) ->
    Key = {Blueprint#blueprint.id, Remaining, State#state{geode = 0}},
    case maps:get(Key, Cache, undefined) of
        undefined ->
            {Geode, CacheAfter} = lists:foldl(fun (Robots, {GeodeAcc, CacheAcc}) ->
                {NextGeode, NextCache} = next(Blueprint, Remaining - 1, minute(Robots, Blueprint, State#state{geode = 0}), CacheAcc),
                {max(NextGeode, GeodeAcc), NextCache}
            end, {0, Cache}, can_buy(Blueprint, State)),
            {Geode + State#state.geode, CacheAfter#{Key => Geode}};
        Geode ->
            {Geode + State#state.geode, Cache}
    end.

part1() ->
    lists:sum([element(1, next(Blueprint, 24, #state{}, #{})) || Blueprint <- input()]).
