-module(aoc_2022_day_5).
-export([start/0]).

parse_to_list([], _, _, Stack) -> Stack;
parse_to_list([String | Rest], CaptureData, N, Stack) ->
    [{Start, Length}] = lists:nth(N, CaptureData),
    StackValue = string:substr(String, Start+1, Length),
    NewStack = case string:trim(StackValue) of
        [] -> Stack;
        C -> [C] ++ Stack
    end,
    parse_to_list(Rest, CaptureData, N, NewStack).


parse_stacks(StartingStacks) ->
    [H | T] = lists:reverse(lists:droplast(StartingStacks)),
    {match, Groups} = re:run(H, "([0-9]+)", [global, {capture, all_but_first}]),
    StackMap = maps:from_list([{K, parse_to_list(T, Groups, K, [])} || K <- lists:seq(1, length(Groups))]),
    StackMap.

parse_instructions(RearrangementProcedure) -> 
    Parser = fun(X) ->
        {match, Groups} = re:run(X, "([0-9]+)", [global, {capture, all_but_first}]),
        [math_functions:parse_integer(string:substr(X, Start + 1, Length)) || [{Start, Length}] <- Groups]
    end,
    [Parser(L) || L <- RearrangementProcedure].


action_instruction_p1(StackMap, [0, _, _]) -> StackMap;
action_instruction_p1(StackMap, [Quantity, From, To]) ->
    {H, T} = lists:split(1, maps:get(From, StackMap)),
    ToStack = maps:get(To, StackMap),
    NewToStack = H ++ ToStack,
    NewMap = maps:put(To, NewToStack, maps:put(From, T, StackMap)),
    action_instruction_p1(NewMap, [Quantity - 1, From, To]).



action_instruction_p2(StackMap, [Quantity, From, To]) ->
    {H, T} = lists:split(Quantity, maps:get(From, StackMap)),
    ToStack = maps:get(To, StackMap),
    NewToStack = H ++ ToStack,
    maps:put(To, NewToStack, maps:put(From, T, StackMap)).


start() ->
    Lines = file_functions:read_lines("input.txt"),
    {StartingStacks, RearrangementProcedure} = lists:splitwith(fun(L) -> string:str(L, "move") =:= 0 end, Lines),
    ParsedStacks = parse_stacks(StartingStacks),
    ParsedInstructions = parse_instructions(RearrangementProcedure),
    Actioned = lists:foldl(fun(Instruction, StackMap) -> action_instruction_p2(StackMap, Instruction) end, ParsedStacks, ParsedInstructions),
    
    Answer = [H || {_, [H|_]} <- maps:to_list(Actioned)],
    io:format("Answer ~p~n", [Answer]).