-module(aoc_2022_day_4).
-export([start/0, to_tuple/1, map_line/1]).

parse_integer(String) ->
    {Int, _} = string:to_integer(String),
    Int.

to_tuple(Text) ->
    [C, D] = string:split(Text, "-"),
    {parse_integer(C), parse_integer(D)}.

map_line(Line) ->
    [A, B] = string:split(Line, ","),
    FirstInterval = to_tuple(A),
    SecondInterval = to_tuple(B),
    {FirstInterval, SecondInterval}.

contains({{X1, Y1}, {X2, Y2}}) ->
    S1 = sets:from_list(lists:seq(X1, Y1)),
    S2 = sets:from_list(lists:seq(X2, Y2)),
    sets:is_subset(S1, S2) or sets:is_subset(S2, S1).

overlap({{X1, Y1}, {X2, Y2}}) ->
    max(X1, X2) =< min(Y1, Y2).


start() ->
    Lines = file_functions:read_lines("input.txt"),
    Mapped = [map_line(L) || L <- Lines],
    Filtered = [L || L <- Mapped, overlap(L)],
    Answer = length(Filtered),
    io:format("Answer ~p~n", [Answer]).

