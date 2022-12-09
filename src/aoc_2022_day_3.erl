-module(aoc_2022_day_3).
-export([start/0, partition/2]).

map_value(Value) when Value >= 97, Value < 123 -> Value - 96;
map_value(Value) when Value >= 65, Value < 97 -> (Value - 64) + 26.

process_line_p1(Line) ->
    LineLength = string:length(Line),
    HalfLength = LineLength div 2,
    FirstHalf = string:substr(Line, 1, HalfLength), 
    LastHalf = string:substr(Line, HalfLength + 1),
    [ItemType] = sets:to_list(sets:intersection(sets:from_list(FirstHalf), sets:from_list(LastHalf))),
    map_value(ItemType).


partition([], Acc) -> Acc;
partition(Lines, Acc) ->
    {Chunk, Rest} = lists:split(3, Lines),
    partition(Rest, Acc ++ [Chunk]).

intersection([H]) -> sets:from_list(H);
intersection([H|T]) ->
    sets:intersection(sets:from_list(H), intersection(T)).



start() ->
    Lines = file_functions:read_lines("input.txt"),
    Chunked = partition(Lines, []),
    Mapped = [map_value(lists:nth(1, sets:to_list(intersection(L)))) || L <- Chunked],
    %Mapped = [process_line_p1(L) || L <- Lines],
    Answer = lists:sum(Mapped),
    io:format("Answer ~p~n", [Answer]).

