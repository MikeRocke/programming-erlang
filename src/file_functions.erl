-module(file_functions).
-export([read_lines/1]).


read_lines(MyList, OpenedFile) ->
    case file:read_line(OpenedFile) of
        {ok, Line} -> read_lines([Line | MyList], OpenedFile);
        eof -> MyList
    end.

read_lines(Filename) ->
    {ok, MyFile} = file:open(Filename, read),
    Lines = read_lines([], MyFile),
    file:close(MyFile),
    lists:reverse(Lines).