-module(aoc_2022_day_2).
-export([start/0, score_line_p2/1]).



process_line_p1(Line) ->
    InputMap = #{"A" => rock, "B" => paper, "C" => scissors, "X" => rock, "Y" => paper, "Z" => scissors},
    Split = string:split(Line, " "),
    Mapped = [maps:get(K, InputMap) || K <- Split],
    list_to_tuple(Mapped).

process_line_p2(Line) ->
    InputMap = #{"A" => rock, "B" => paper, "C" => scissors, "X" => lose, "Y" => draw, "Z" => win},
    Split = string:split(Line, " "),
    Mapped = [maps:get(K, InputMap) || K <- Split],
    list_to_tuple(Mapped).


score_line_p1({OpponentSelection, MySelection}) ->
    ScoreMap = #{rock => 1, paper => 2, scissors => 3},
    Outcome = case {OpponentSelection , MySelection} of
        {rock, scissors} -> 0;
        {scissors, paper} -> 0;
        {paper, rock} -> 0;
        {X, X} -> 3; %Draw
        {_, _} -> 6
    end,
    Outcome + maps:get(MySelection, ScoreMap).


score_line_p2({OpponentSelection, WinCondition}) ->
    ScoreMap = #{win => 6, draw => 3, lose => 0, rock => 1, paper => 2, scissors => 3},
    MySelection = case {OpponentSelection , WinCondition} of
        {rock, win} -> paper;
        {rock, lose} -> scissors;
        {paper, win} -> scissors;
        {paper, lose} -> rock;
        {scissors, win} -> rock;
        {scissors, lose} -> paper;
        {_, draw} -> OpponentSelection
    end,
    maps:get(MySelection, ScoreMap) + maps:get(WinCondition, ScoreMap).

start() ->
    Lines = file_functions:read_lines("input.txt"),
    Mapped = [process_line_p2(L) || L <- Lines],
    Scored = [score_line_p2(L) || L <- Mapped],
    Answer = lists:sum(Scored),
    io:format("Answer ~p~n", [Answer]).

