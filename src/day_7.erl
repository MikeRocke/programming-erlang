-module(day_7).
-export([start/0]).
-compile(export_all).

parse_instruction(Line) ->
    ChangeDirectory = case re:run(Line, "[$] cd (.*)", [global, {capture, all_but_first}]) of 
        {match, [[{Start, Length}]]} -> {cd, string:substr(Line, Start+1, Length)};
        nomatch -> nomatch
    end,
    ListDirectory = case re:run(Line, "[$] ls") of 
        {match, _} -> {ls};
        nomatch -> nomatch
    end,
    case {ChangeDirectory, ListDirectory} of 
        {ChangeDirectory, nomatch} -> ChangeDirectory;
        {nomatch, ListDirectory} -> ListDirectory 
    end.

parse_listing(Line) ->
    case re:run(Line, "([0-9]+) (.*)", [global, {capture, all_but_first}]) of
        {match, [[{ByteStart, ByteLength}, {FilenameStart, FilenameLength}]]} -> 
            {string:substr(Line, FilenameStart+1, FilenameLength), math_functions:parse_integer(string:substr(Line, ByteStart+1, ByteLength))};
        nomatch -> nomatch
    end.


next_instruction([]) -> {{done}};
next_instruction([Line | Rest]) ->
    {parse_instruction(Line), Rest}.
    
action({{done}}, Filetree, _) ->
    Filetree;

action({{cd, "/"}, Lines}, Filetree, _) ->
    action(next_instruction(Lines), Filetree, ["/"]);

action({{cd, ".."}, Lines}, Filetree, [_ | Rest]) ->
    action(next_instruction(Lines), Filetree, Rest); %Not sure if this will error

action({{cd, Folder}, Lines}, Filetree, CurrentDirectory) ->
    action(next_instruction(Lines), Filetree, [Folder | CurrentDirectory]);

action({{ls}, Lines}, Filetree, CurrentDirectory) ->
    {Contents, Rest} = lists:splitwith(fun(Line) -> string:find(Line, "$") =:= nomatch end, Lines),
    io:format("~p ~n", [Contents]),
    Parsed = lists:filter(fun(Listing) -> Listing =/= nomatch end, [parse_listing(C) || C <- Contents]),
    NewFiletree = maps:put(CurrentDirectory, Parsed, Filetree),
    action(next_instruction(Rest), NewFiletree, CurrentDirectory).

sum_contents(Contents) ->
    Sizes = [FileSize || {_, FileSize} <- Contents],
    lists:sum(Sizes).


duplicate_sum(Directory, FiletreeSizes) ->
    ChildrenSizes = [S || {D, S} <- FiletreeSizes, lists:suffix(Directory, D)],
    lists:sum(ChildrenSizes).

start() ->
    Lines = file_functions:read_lines("input.txt"),
    Filetree = action(next_instruction(Lines), #{}, ["/"]),
    DirectorySizes = [{Dir, sum_contents(DirectoryContents)} || {Dir, DirectoryContents} <- maps:to_list(Filetree)],
    DuplicatedSums = [{D, duplicate_sum(D, DirectorySizes)} || {D, S} <- DirectorySizes],
    TotalDiskSpace = 70000000,
    RequiredDiskSpace = 30000000,
    UsedDiskSpace = maps:get(["/"], maps:from_list(DuplicatedSums)),
    UnusedDiskSpace = TotalDiskSpace - UsedDiskSpace,
    lists:sort(lists:filter(fun(Size) -> UnusedDiskSpace + Size >= RequiredDiskSpace end, lists:map(fun({_, X}) -> X end, DuplicatedSums))).
    
    % lists:sum(lists:filter( fun(Size) -> Size =< 100000 end, lists:map(fun({_, X}) -> X end, DuplicatedSums))).