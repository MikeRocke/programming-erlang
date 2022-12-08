-module(aoc_2022_day_1).
-export([start/0]).

elf_partition(Lines) ->
    lists:foldl(fun(X, [SubList|Rest]) ->
        case string:to_integer(X) of
            {error, _} -> [[] | [SubList|Rest]];
            {Int, _} -> [lists:append([Int], SubList) | Rest] 
        end
        end, [[]], Lines).

start() ->
    Lines = file_functions:read_lines("input.txt"),
    Partitioned = elf_partition(Lines),
    Summed = [lists:sum(L) || L <- Partitioned],
    Sorted = lists:sort(Summed),
    Answer = lists:sum(lists:sublist(lists:reverse(Sorted), 3)),
    io:format("Answer ~p~n", [Answer]).

