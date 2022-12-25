-module(aoc_2022_day_11).
-export([start/0, monkey_loop/2]).

monkey_loop({Me, Items, Operation, Test, TrueMonkey, FalseMonkey}, InspectionCount) ->
    receive 
        {display} -> io:format("~p : ~w : has inspected ~w items ~n", [Me, Items, InspectionCount]),
            monkey_loop({Me, Items, Operation, Test, TrueMonkey, FalseMonkey}, InspectionCount);
        {catch_item, WorryLevel} -> 
            monkey_loop({Me, Items ++ [WorryLevel], Operation, Test, TrueMonkey, FalseMonkey}, InspectionCount);
        {inspect, MonkeyChain} -> 
            lists:foreach(fun(Item) ->  
                % WorryLevel = Operation(Item) div 3, %Part 1
                % WorryLevel = Operation(Item) rem 96577, %Part 2 - sample
                WorryLevel = Operation(Item) rem 9699690, %Part 2 - Actual
                Result = Test(WorryLevel),
                case Result of 
                    true -> TrueMonkey ! {catch_item, WorryLevel};
                    false -> FalseMonkey ! {catch_item, WorryLevel}
                end
                end, Items),
            NewInspectionCount = length(Items) + InspectionCount,
            {[Next], Rest} = lists:split(1, MonkeyChain),
            Next ! {inspect, Rest},
            monkey_loop({Me, [], Operation, Test, TrueMonkey, FalseMonkey}, NewInspectionCount)
    end.

spawn_monkey(Definition) ->
    {Me, _, _, _, _, _} = Definition,
    Spawned = spawn(day_11, monkey_loop, [Definition, 0]),
    register(Me, Spawned),
    Spawned.

start() ->
    % MonkeyDefinitions = [
    %     {monkey_0, [79, 98], fun(Old) -> Old * 19 end, fun(V) -> V rem 23 == 0 end, monkey_2, monkey_3},
    %     {monkey_1, [54, 65, 75, 74], fun(Old) -> Old + 6 end, fun(V) -> V rem 19 == 0 end, monkey_2, monkey_0},
    %     {monkey_2, [79, 60, 97], fun(Old) -> Old * Old end, fun(V) -> V rem 13 == 0 end, monkey_1, monkey_3},
    %     {monkey_3, [74], fun(Old) -> Old + 3 end, fun(V) -> V rem 17 == 0 end, monkey_0, monkey_1}
    %     ],
    MonkeyDefinitions = [
        {monkey_0, [66, 71, 94], fun(Old) -> Old * 5 end, fun(V) -> V rem 3 == 0 end, monkey_7, monkey_4},
        {monkey_1, [70], fun(Old) -> Old + 6 end, fun(V) -> V rem 17 == 0 end, monkey_3, monkey_0},
        {monkey_2, [62, 68, 56, 65, 94, 78], fun(Old) -> Old + 5 end, fun(V) -> V rem 2 == 0 end, monkey_3, monkey_1},
        {monkey_3, [89, 94, 94, 67], fun(Old) -> Old + 2 end, fun(V) -> V rem 19 == 0 end, monkey_7, monkey_0},
        {monkey_4, [71, 61, 73, 65, 98, 98, 63], fun(Old) -> Old * 7 end, fun(V) -> V rem 11 == 0 end, monkey_5, monkey_6},
        {monkey_5, [55, 62, 68, 61, 60], fun(Old) -> Old + 7 end, fun(V) -> V rem 5 == 0 end, monkey_2, monkey_1},
        {monkey_6, [93, 91, 69, 64, 72, 89, 50, 71], fun(Old) -> Old + 1 end, fun(V) -> V rem 13 == 0 end, monkey_5, monkey_2},
        {monkey_7, [76, 50], fun(Old) -> Old * Old end, fun(V) -> V rem 7 == 0 end, monkey_4, monkey_6}
        ],
    Monkeys = [spawn_monkey(D) || D <- MonkeyDefinitions],

    lists:foreach(fun (Round) -> 
        % monkey_0 ! {inspect, [monkey_1, monkey_2, monkey_3, self()]},
        monkey_0 ! {inspect, [monkey_1, monkey_2, monkey_3, monkey_4, monkey_5, monkey_6, monkey_7, self()]},
        receive
            {inspect, _} -> io:format("Round ~p over ~n", [Round])
        end
        
        % end, lists:seq(1,20)),
        end, lists:seq(1,10000)), %Part 2
        lists:foreach(fun(Monkey) -> Monkey ! {display} end, Monkeys).

