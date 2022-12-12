-module(aoc_2022_day_8).
-export([start/0]).
-compile(export_all).

build_grid(Lines) ->
    RowToCells = fun(Row) -> [math_functions:parse_integer([X]) || X <- Row] end,
    [RowToCells(string:trim(Row)) || Row <- Lines].


get(Grid, {Row, Column}) -> lists:nth(Column, lists:nth(Row, Grid)).

step(_, {1, _}, _, _, StepCount) -> {true, StepCount};
step(_, {_, 1}, _, _, StepCount) -> {true, StepCount};
step(_, {A, _}, {_,A}, _, StepCount) -> {true, StepCount};
step(_, {_, B}, {B,_}, _, StepCount) -> {true, StepCount};
step(Grid, {Row, Column}, {Width, Height}, {DeltaRow, DeltaColumn}, StepCount) ->
    TreeHeight = get(Grid, {Row, Column}),
    {NextRow, NextColumn} = {Row + (DeltaRow * StepCount), Column + (DeltaColumn * StepCount)},
    StepTreeHeight = get(Grid, {NextRow, NextColumn}),
    if 
        TreeHeight>StepTreeHeight ->
            if 
                NextRow =:= 1 -> {true, StepCount};
                NextColumn =:= 1 -> {true, StepCount};
                NextRow =:= Height -> {true, StepCount};
                NextColumn =:= Width -> {true, StepCount};
                true -> step(Grid, {Row, Column}, {Width, Height}, {DeltaRow, DeltaColumn}, StepCount+1)
            end;
        true ->
            {false, StepCount}
    end.

traverse(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}) ->
    {CanGoUp, UpCount} = step(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}, {-1, 0}, 1),
    {CanGoDown, DownCount} = step(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}, {1, 0}, 1),
    {CanGoLeft, LeftCount} = step(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}, {0, -1}, 1),
    {CanGoRight, RightCount} = step(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}, {0, 1}, 1),
    {CanGoUp or CanGoDown or CanGoLeft or CanGoRight, UpCount * DownCount * LeftCount * RightCount}.

start() ->
    Lines = file_functions:read_lines("input.txt"),
    Grid = build_grid(Lines),
    {Width, Height} = {length(lists:nth(1,Grid)), length(Grid)},
    AllPositions = [{X, Y} || X <- lists:seq(1, Height), Y <- lists:seq(1, Width)],
    % length(lists:filter(fun(Tree) -> traverse(Grid, Tree, {Width, Height}) end, AllPositions)).
    Traversed = [traverse(Grid, T, {Width, Height}) || T <- AllPositions],
    lists:max([Score || {_, Score} <- Traversed]).
    % Visible = traverse(Grid, {4, 3}, {Width, Height}).
    