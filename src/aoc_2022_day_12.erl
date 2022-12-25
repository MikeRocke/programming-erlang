-module(aoc_2022_day_12).
-export([start/0]).
-compile(export_all).

start() ->
    Lines = file_functions:read_lines("input.txt"),
    Grid = [parse_line(Line) || Line <- Lines],
    NumberOfRows = length(Grid),
    NumberOfColumns = length(lists:nth(1, Grid)),
    AllPositions = [{R,C} || R <- lists:seq(1, NumberOfRows), C <- lists:seq(1, NumberOfColumns)],
    [StartingPosition] = lists:filter(fun(P) -> get_grid_value(Grid, P) =:= 0 end, AllPositions),
    [TargetPosition] = lists:filter(fun(P) -> get_grid_value(Grid, P) =:= 27 end, AllPositions),
    Graph = digraph:new([cyclic]),
    [digraph:add_vertex(Graph, V) || V <- AllPositions],
    [populate(Graph, V, Grid, NumberOfRows, NumberOfColumns) || V <- AllPositions],
    ShortestPath = digraph:get_short_path(Graph, StartingPosition, TargetPosition),
    Solution2 = [digraph:get_short_path(Graph, Bottom, TargetPosition) || Bottom <- lists:filter(fun(P) -> get_grid_value(Grid, P) =:= 1 end, AllPositions)],
    digraph:delete(Graph),
    length(ShortestPath),
    lists:min([length(S) || S <- Solution2, S =/= false]).
    



populate(Graph, {R, C}, Grid, NumberOfRows, NumberOfColumns) ->
    Up = {R-1, C}, Down = {R+1, C}, Left = {R, C-1}, Right = {R, C+1},
    NextDirections = [Up, Down, Left, Right],
    Elevation = get_grid_value(Grid, {R, C}),
    IsValid = fun({Nr, Nc}) ->
        (Nr > 0) and 
        (Nc > 0) and 
        (Nr =< NumberOfRows) and 
        (Nc =< NumberOfColumns) andalso
        (max(min(get_grid_value(Grid, {Nr, Nc}), 26),1) - Elevation =< 1)
    end,
    ValidDirections = lists:filter(IsValid, NextDirections),
    lists:foreach(fun(V2) -> digraph:add_edge(Graph, {R, C}, V2) end, ValidDirections).

get_grid_value(Grid, {R, C}) -> lists:nth(C, lists:nth(R, Grid)).

parse_line(Line) ->
    ToNum = fun(X) ->
        case X of 
            83 -> 0;
            69-> 27;
            _ -> X - 96
        end
    end, 
    [ToNum(X) || X <- string:trim(Line)].