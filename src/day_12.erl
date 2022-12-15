-module(day_12).
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
    % StartingPosition = {1,1}, %Lazy and hardcoded
    % TargetPosition = {3,6}, %Lazy again
    search(Grid, StartingPosition, TargetPosition, NumberOfRows, NumberOfColumns).

search(Grid, StartingPosition, TargetPosition, NumberOfRows, NumberOfColumns) ->
    ets:new(paths, [set, named_table]),
    traverse(Grid, StartingPosition, TargetPosition, NumberOfRows, NumberOfColumns, []),
    AllPaths = ets:tab2list(paths),
    ets:delete(paths),
    lists:min([StepCount || {_, StepCount} <- AllPaths]).

traverse(_, Goal, Goal, _, _, Path) -> 
    ets:insert(paths, {Path, length(Path)}),
    Path;
traverse(Grid, {R, C}, TargetPosition, NumberOfRows, NumberOfColumns, Path) ->
    Up = {R-1, C}, Down = {R+1, C}, Left = {R, C-1}, Right = {R, C+1},
    NextDirections = [Up, Down, Left, Right],
    Elevation = get_grid_value(Grid, {R, C}),
    Visited = sets:from_list(Path),
    IsValid = fun({Nr, Nc}) -> 
        (Nr > 0) and 
        (Nc > 0) and 
        (Nr =< NumberOfRows) and 
        (Nc =< NumberOfColumns) and
        (not sets:is_element({Nr, Nc}, Visited)) andalso
        (get_grid_value(Grid, {Nr, Nc}) - Elevation =< 1)
    end,
    ValidDirections = lists:filter(IsValid, NextDirections),
    [traverse(Grid, D, TargetPosition, NumberOfRows, NumberOfColumns, [{R, C}] ++ Path) || D <- ValidDirections].

get_grid_value(Grid, {R, C}) -> lists:nth(C, lists:nth(R, Grid)).

% Skip heuristic for now
% heuristic({R1, C1}, {R2, C2}, StepCount) -> abs(R1 - R2) + abs(C1 - C2) + StepCount.

parse_line(Line) ->
    ToNum = fun(X) ->
        case X of 
            83 -> 0;
            69-> 27;
            _ -> X - 96
        end
    end, 
    [ToNum(X) || X <- string:trim(Line)].