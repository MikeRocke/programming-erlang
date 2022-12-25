-module(aoc_2022_day_14).
-export([start/0]).
-compile(export_all).

parse_line(Line) ->
    {ok, Data, _} = erl_scan:string(Line),
    Filtered = lists:filter(fun(V) -> element(1, V) =:= integer end, Data),
    Mapped = [V || {integer, _, V} <- Filtered],
    part(Mapped).

part(List) ->
        part(List, []).
part([], Acc) ->
        lists:reverse(Acc);
part([H], Acc) ->
        lists:reverse([[H]|Acc]);
part([H1,H2|T], Acc) ->
        part(T, [{H1,H2}|Acc]).



start() ->
    Lines = file_functions:read_lines("input.txt"),
    Parsed = lists:map(fun parse_line/1, Lines),
    StartingPoint = {500,0},
    CollisionMap = build_collision_map(Parsed),
    AbyssY = lists:max([Y || {_, Y} <- maps:keys(CollisionMap)]),
    solve_part_1({CollisionMap, StartingPoint}, StartingPoint, AbyssY, 1),
    Floor = AbyssY + 2,
    WithFloor = maps:merge(CollisionMap, collision_map_from_two_points({0, Floor}, {1000, Floor})),
    solve_part_2({WithFloor, StartingPoint}, StartingPoint, 1).



build_collision_map(Parsed) ->
        CollisionMap = #{},
        lists:foldl(fun(ParsedLine, Acc) -> maps:merge(Acc, collision_map_from_line(ParsedLine)) end, CollisionMap, Parsed).



collision_map_from_line(L) -> collision_map_from_line(L, #{}).
collision_map_from_line([_], Map) -> Map;
collision_map_from_line([A | T], Map) ->
        B = lists:nth(1, T),
        CM = collision_map_from_two_points(A, B),
        NewMap = maps:merge(Map, CM),
        collision_map_from_line(T, NewMap).


collision_map_from_two_points({X1, Y1}, {X2, Y2}) ->
        % buggy as assumes no diag
        Xs = lists:seq(min(X1,X2), max(X1,X2)),
        Ys = lists:seq(min(Y1,Y2), max(Y1,Y2)),
        FoldFunction = fun(Key, CollisionMap) -> maps:put(Key, "#", CollisionMap) end,
        lists:foldl(FoldFunction, #{}, [{X,Y} || X <- Xs, Y <- Ys]).


solve_part_2({State, StartingPoint}, {SandX, SandY}, UnitsOfSand) ->
        case physics_step(State, {SandX, SandY}) of
                {stopped, _} when {SandX, SandY} =:= StartingPoint -> UnitsOfSand;
                {in_motion, NewState, NewPosition} -> solve_part_2({NewState, StartingPoint}, NewPosition, UnitsOfSand);
                {stopped, NewState} -> solve_part_2({NewState, StartingPoint}, StartingPoint, UnitsOfSand + 1)
        end.


solve_part_1({State, _}, {_, Y}, Y, UnitsOfSand) -> 
        visualize(State),
        UnitsOfSand - 1;
solve_part_1({State, StartingPoint}, {SandX, SandY}, AbyssY, UnitsOfSand) ->
        case physics_step(State, {SandX, SandY}) of
                {in_motion, NewState, NewPosition} -> solve_part_1({NewState, StartingPoint}, NewPosition, AbyssY, UnitsOfSand);
                {stopped, NewState} -> solve_part_1({NewState, StartingPoint}, StartingPoint, AbyssY, UnitsOfSand + 1)
        end.

physics_step(State, {SandX, SandY}) ->
        Down = {SandX, SandY + 1},
        DownLeft = {SandX - 1, SandY + 1},
        DownRight = {SandX + 1, SandY + 1},
        Possibles = [Down, DownLeft, DownRight],
        NextMoves = lists:filter(fun(P) -> not maps:is_key(P, State) end, Possibles),
        case NextMoves of
                [] -> {stopped, maps:put({SandX, SandY}, "o", State)};
                [P] -> {in_motion, State, P};
                [P | _] -> {in_motion, State, P}
        end.


visualize(CollisionMap) -> 
    timer:sleep(500),
    io:format("~n~n~n"),
    lists:foreach(fun(R) ->
        io:format("~n"),
        lists:foreach(fun(C) ->
                io:format(maps:get({C,R}, CollisionMap, "."))
            end, lists:seq(480, 510))
        end, lists:seq(0, 12)).