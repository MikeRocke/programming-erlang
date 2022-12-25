-module(day_15).
-export([start/0]).
-compile(export_all).

start() ->
    Lines = file_functions:read_lines("input.txt"),
    Data = [to_pairs(parse_line(L)) || L <- Lines],
    solve_part_one(Data),
    solve_part_two(Data).

solve_part_two(Data) ->
    solve_part_two(Data, 0).
solve_part_two(Data, YToCheck) ->
    FilteredDataIntervals = lists:flatmap(fun(D) -> to_coverage_area(D, YToCheck) end, Data),
    SortedIntervals = lists:sort(FilteredDataIntervals),
    Intervals = lists:foldl(fun({S,E}, Acc) -> 
        case Acc of
            [] -> [{S, E}];
            [{OS, OE}] -> try_merge({S, E}, {OS, OE});
            [{OS, OE} | T] -> try_merge({S, E}, {OS, OE}) ++ T
        end
        end, [], SortedIntervals),
    case Intervals of 
        [_] -> solve_part_two(Data, YToCheck + 1);
        [{B, _}, {_, C}] -> {{x, C + 1, B, C}, {y, YToCheck}}
    end.

solve_part_one(Data) ->
    YToCheck = 3289729,
    % YToCheck = 2000000,
    % YToCheck = 10,
    FilteredDataIntervals = lists:flatmap(fun(D) -> to_coverage_area(D, YToCheck) end, Data),
    SortedIntervals = lists:sort(FilteredDataIntervals),
    lists:foldl(fun({S,E}, Acc) -> 
        case Acc of
            [] -> [{S, E}];
            [{OS, OE}] -> try_merge({S, E}, {OS, OE});
            [{OS, OE} | T] -> try_merge({S, E}, {OS, OE}) ++ T
        end
        end, [], SortedIntervals).

try_merge({X1, Y1}, {X2, Y2}) ->
    case does_overlap({X1, Y1}, {X2, Y2}) of
        true -> [{min(X1, X2), max(Y1, Y2)}];
        false -> [{X1, Y1}, {X2, Y2}]
    end.

does_overlap({X1, Y1}, {X2, Y2}) ->
    (X1 =< Y2) and (X2 =< Y1).

to_coverage_area({{sensor, Xs, Ys}, {beacon, Xb, Yb}}, YToCheck) ->
    Distance = distances:manhattan({Xs, Ys}, {Xb, Yb}),
    DistanceToY = abs(YToCheck - Ys),
    
    XWidth = max(Distance - DistanceToY, 0),
    case XWidth of
        0 -> [];
        _ -> [{Xs - XWidth, Xs + XWidth}]
    end.

to_pairs([Xs,Ys,Xb,Yb]) ->
    {{sensor, Xs, Ys}, {beacon, Xb, Yb}}.

% TODO refactor common utility
parse_line(Line) ->
    case re:run(Line, "Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)", [global, {capture, all_but_first}]) of
        {match, [Groups]} -> [math_functions:parse_integer(string:substr(Line, Start+1, Length)) || {Start, Length} <- Groups];
        nomatch -> nomatch
    end.