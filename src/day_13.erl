-module(day_13).
-export([start/0]).
-compile(export_all).


start() ->
    Lines = file_functions:read_lines("input.txt"),
    PacketLines = [string:trim(Line) || Line <- Lines, not string:is_empty(string:trim(Line))],
    
    {LeftIndexes, RightIndexes} = lists:partition(fun(I) -> I rem 2 == 1 end, [Index || Index <- lists:seq(1, length(PacketLines))]),
    PacketPairs = [{parse(lists:nth(RI - 1, PacketLines)), parse(lists:nth(RI, PacketLines))} || RI <- RightIndexes],
    SolitionOne = lists:filter(fun(I) -> 
         {Left, Right} = lists:nth(I, PacketPairs),
         correct_order(Left, Right)
         end, lists:seq(1, length(PacketPairs))),
    lists:sum(SolitionOne),
    io:format("~p ~n", [SolitionOne]),

    FirstDivider = [[2]],
    SecondDivider = [[6]],
    ToSort = [parse(PacketLine) || PacketLine <- PacketLines] ++ [FirstDivider, SecondDivider],
    Sorted = lists:sort(fun correct_order/2, ToSort),
    [FI, SI] = [I || {I, Packet} <- lists:enumerate(Sorted), (Packet =:= FirstDivider) or (Packet =:= SecondDivider) ],
    io:format("~p  ~p ~n", [FI, SI]),
    FI * SI.



% Super thank you for teaching me this!
% https://stackoverflow.com/questions/49291088/convert-a-string-of-list-to-a-list-in-erlang
parse(Line) ->
    {ok, Data, _} = erl_scan:string(Line),
    {ok, Parsed} = erl_parse:parse_term(Data ++ [{dot, 1} || element(1, lists:last(Data)) =/= dot]),
    Parsed.



correct_order([L | LT], [R | RT]) when is_integer(L), is_integer(R), L < R -> true; %
correct_order([L | LT], [R | RT]) when is_integer(L), is_integer(R), L =:= R -> correct_order(LT, RT); %
correct_order([L | LT], [R | RT]) when is_integer(L), is_integer(R), L > R -> false; %


% correct_order([L|LT], [R | RT]) when is_list(L); is_list(R)-> correct_order(L, R);

correct_order([A|Ax], [B|Bx]) when is_list(A);  is_list(B) ->
    case correct_order(A, B) of
        ok    -> correct_order(Ax, Bx);
        Other -> Other
    end;

correct_order([], []) -> ok; %

correct_order(L, R) when is_list(L), is_integer(R) -> correct_order(L, [R]);%
correct_order(L, R) when is_integer(L), is_list(R) -> correct_order([L], R); %
correct_order([], R) when is_list(R), length(R) > 0 -> true; %
correct_order(L, []) when is_list(L), length(L) > 0 -> false. %