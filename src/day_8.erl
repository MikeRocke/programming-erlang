-module(day_8).
-export([start/0]).
-compile(export_all).

build_grid(Lines) ->
    RowToCells = fun(Row) -> [math_functions:parse_integer([X]) || X <- Row] end,
    [RowToCells(string:trim(Row)) || Row <- Lines].


get(Grid, {Row, Column}) -> lists:nth(Column, lists:nth(Row, Grid)).

step(_, {1, _}, _, _, _) -> true;
step(_, {_, 1}, _, _, _) -> true;
step(_, {A, _}, {_,A}, _, _) -> true;
step(_, {_, B}, {B,_}, _, _) -> true;
step(Grid, {Row, Column}, {Width, Height}, {DeltaRow, DeltaColumn}, StepCount) ->
    TreeHeight = get(Grid, {Row, Column}),
    {NextRow, NextColumn} = {Row + (DeltaRow * StepCount), Column + (DeltaColumn * StepCount)},
    StepTreeHeight = get(Grid, {NextRow, NextColumn}),
    if 
        TreeHeight>StepTreeHeight ->
            if 
                NextRow =:= 1 -> true;
                NextColumn =:= 1 -> true;
                NextRow =:= Height -> true;
                NextColumn =:= Width -> true;
                true -> step(Grid, {Row, Column}, {Width, Height}, {DeltaRow, DeltaColumn}, StepCount+1)
            end;
        true ->
            false
    end.

traverse(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}) ->
    CanGoUp = step(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}, {-1, 0}, 1),
    CanGoDown = step(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}, {1, 0}, 1),
    CanGoLeft = step(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}, {0, -1}, 1),
    CanGoRight = step(Grid, {TreeRow, TreeColumn}, {GridWidth, GridHeight}, {0, 1}, 1),
    CanGoUp or CanGoDown or CanGoLeft or CanGoRight.

start() ->
    Lines = file_functions:read_lines("input.txt"),
    Grid = build_grid(Lines),
    {Width, Height} = {length(lists:nth(1,Grid)), length(Grid)},
    AllPositions = [{X, Y} || X <- lists:seq(1, Height), Y <- lists:seq(1, Width)],
    % length(lists:filter(fun(Tree) -> traverse(Grid, Tree, {Width, Height}) end, AllPositions)).
    Visible = traverse(Grid, {2, 3}, {Width, Height}).
    