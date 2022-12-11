-module(aoc_2022_day_6).
-export([start/0]).



start() ->
    Lines = file_functions:read_lines("input.txt"),
    Input = lists:nth(1, Lines),
    NGramSize = 14,
    NGram = fun(Start, Size, Text) -> 
         lists:sublist(Text, Start, Size)
    end,
    Parsed = [{L, NGram(L, NGramSize, Input)} || L <- lists:seq(1, length(Input) - NGramSize)],
    lists:nth(1, [{C + NGramSize - 1, X} || {C, X} <- Parsed, sets:size(sets:from_list(X)) =:= NGramSize]).