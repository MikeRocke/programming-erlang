-module(aoc_2022_day_9).
-export([start/0]).
-compile(export_all).

parse_instruction(Line) ->
    [Direction, Amount] = string:split(string:to_lower(Line), " "),
    {Int, _} = string:to_integer(Amount),
    {list_to_atom(Direction), Int}.

action({_, 0}, Positions) -> 
    % visualize(Positions),
    Positions;
action({Direction, Amount}, Positions) ->
    {[{X, Y}], TailPositions} = lists:split(1, Positions),
    NewPosition = case Direction of
        u -> {X, Y + 1};
        d -> {X, Y - 1};
        l -> {X - 1, Y};
        r -> {X + 1, Y}
    end,
    NewTail = move_tail(NewPosition, TailPositions, []),
    NewPositions = [NewPosition] ++ NewTail,
    action({Direction, Amount - 1}, NewPositions).


distance({X1, Y1}, {X2, Y2}) ->
    max(abs(X2-X1), abs(Y2-Y1)).

% Hate this code, but brain is barely functioning from the flu
move_closer({SameX, TargetY}, {SameX, Y}) when TargetY > Y -> {SameX, Y + 1};
move_closer({SameX, TargetY}, {SameX, Y}) when TargetY < Y -> {SameX, Y - 1};
move_closer({TargetX, SameY}, {X, SameY}) when TargetX > X -> {X + 1, SameY};
move_closer({TargetX, SameY}, {X, SameY}) when TargetX < X -> {X - 1, SameY};
move_closer({X1, Y1}, {X2, Y2}) ->
    {NewX, _} = move_closer({X1, Y1}, {X2, Y1}),
    {_, NewY} = move_closer({X1, Y1}, {X1, Y2}),
    {NewX, NewY}.

move_tail(TargetPosition, [MyPosition], NewTail) ->
    NewPosition = case distance(TargetPosition, MyPosition) of
        0 -> MyPosition;
        1 -> MyPosition;
        _ -> move_closer(TargetPosition, MyPosition)
    end,
    ets:insert(tp, {NewPosition, 1}),
    lists:reverse([NewPosition] ++ NewTail);
move_tail(TargetPosition, [MyPosition | Tail], NewTail) ->
    NewPosition = case distance(TargetPosition, MyPosition) of
        0 -> MyPosition;
        1 -> MyPosition;
        _ -> move_closer(TargetPosition, MyPosition)
    end,
    move_tail(NewPosition, Tail, [NewPosition] ++ NewTail).

start() ->
    {ok, File} = file:open("input.txt", read),
    ets:new(tp, [set, named_table]),
    Solve = fun(Line, Positions) ->
        Instruction = parse_instruction(Line),
        NewPositions = action(Instruction, Positions)
    end,
    each_line(File, Solve, [{0,0} || _ <- lists:seq(1,10) ]),
    file:close(File),
    D = ets:tab2list(tp),
    ets:delete(tp),
    length(D).


each_line(File, ActionFunction, Positions) ->
    case file:read_line(File) of 
        eof -> Positions;
        {ok, Line} -> each_line(File, ActionFunction, ActionFunction(Line, Positions))
    end.

visualize([H | T]) -> 
    timer:sleep(500),
    GridSize = 10,
    AllPoints = sets:from_list([H | T]),
    io:format("~n~n~n"),
    lists:foreach(fun(R) ->
        io:format("~n"),
        lists:foreach(fun(C) ->
            IsElement = sets:is_element({C, R}, AllPoints),
            if 
                IsElement =:= true -> io:format("#");
                true -> io:format(".")
            end
            end, lists:seq(0, GridSize))
        end, lists:seq(0, GridSize)).