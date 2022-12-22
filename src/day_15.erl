-module(day_15).
-export([start/0]).
-compile(export_all).

start() ->
    Lines = file_functions:read_lines("input.txt"),
    Data = [to_pairs(parse_line(L)) || L <- Lines],
    solve_part_one(Data).

solve_part_one(Data) ->
    % YToCheck = 2000000,
    YToCheck = 10,
    
    Beacons = sets:from_list([{Xb, Yb} || {_, {beacon, Xb, Yb}} <- Data]),
    FilterRow = fun({_, Y}) -> Y =:= YToCheck end,
    FilterOutBeacon = fun(P) -> not sets:is_element(P, Beacons) end,
    Filter = fun(P) -> FilterRow(P) and FilterOutBeacon(P) end,

    Cells = lists:filter(Filter, lists:flatmap(fun to_coverage_area/1, Data)),
    UniqueCells = sets:from_list(Cells),
    sets:size(UniqueCells).


to_coverage_area({{sensor, Xs, Ys}, {beacon, Xb, Yb}}) ->
    Distance = distances:manhattan({Xs, Ys}, {Xb, Yb}),
    XRange = lists:seq(Xs - Distance, Xs + Distance),
    YRange = lists:seq(Ys - Distance, Ys + Distance),
    [{X, Y} || X <- XRange, Y <- YRange, distances:manhattan({Xs, Ys}, {X, Y}) =< Distance].

to_pairs([Xs,Ys,Xb,Yb]) ->
    {{sensor, Xs, Ys}, {beacon, Xb, Yb}}.

% TODO refactor common utility
parse_line(Line) ->
    case re:run(Line, "Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)", [global, {capture, all_but_first}]) of
        {match, [Groups]} -> [math_functions:parse_integer(string:substr(Line, Start+1, Length)) || {Start, Length} <- Groups];
        nomatch -> nomatch
    end.