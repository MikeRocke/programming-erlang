-module(day_10).
-export([start/0]).
-compile(export_all).

solution_1(Execution) ->
    SignalStrengths = [lists:nth(N, Execution) * N || N <-  lists:seq(20, length(Execution), 40)],
    Sum = lists:sum(SignalStrengths),
    io:format("Solution 1: ~p ~n", [Sum]),
    Sum.

solution_2(Execution) ->
    Width = 40,

    Rows = lists:seq(0,5),
    Columns = lists:seq(0,Width-1),

    lists:foreach(fun(R) -> 
        NStart = Width * R,
        io:format("~n"),
        lists:foreach(fun(C) -> 
            N = NStart + C,
            Cycle = N + 1,
            Position = Cycle - 1 - NStart,
            X = lists:nth(Cycle, Execution),
            D = abs(Position - X),
            % io:format("~n ~p ~p ~p", [Cycle, X, D])
            Pixel = case D of 
                0 -> "#";
                1 -> "#";
                _ -> "."
            end,
            io:format(Pixel)
            end, Columns)
        end, Rows).

start() ->
    X = 1,
    Lines = file_functions:read_lines("input.txt"),
    {_, Execution} = loop(Lines, X, []),
    solution_2(Execution).

parse_instruction("noop") -> {noop};
parse_instruction("addx " ++ T) -> 
    {Int, _} = string:to_integer(T),
    {addx, Int}.

loop([], X, Cycles) -> {X, Cycles};
loop([H | T], X, Cycles) -> 
    Line = string:trim(H),
    Instruction = parse_instruction(Line),
    {NewX, Executed} = execute(X, Instruction),
    loop(T, NewX, Cycles ++ Executed).

execute(X, {noop}) -> {X, [X]};
execute(X, {addx, Value}) -> {X + Value, [X, X]}.