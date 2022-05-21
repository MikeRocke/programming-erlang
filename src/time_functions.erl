-module(time_functions).
-export([my_time_func/1]).

my_time_func(F) ->
    Start = erlang:system_time(),
    X = F(),
    End = erlang:system_time(),
    io:format("took: ~p~n", [End-Start]),
    X.
