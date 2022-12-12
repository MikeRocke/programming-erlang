-module(pascal_triangle).
-export([main/0, pascal_value/2]).

fac(0) -> 1;
fac(1) -> 1;
fac(N) -> N * fac(N-1).

pascal_value(N, R) ->
    fac(N) div (fac(R) * fac(N - R)).

% print_triangle(N, N) ->
    % L = [pascal_value(N, V) || V <- lists:seq(1, N)],
    % io:format("~p ~n", L);

print_triangle(N, Limit) when N > Limit -> true;
print_triangle(N, Limit) ->
    L = [pascal_value(N-1, V) || V <- lists:seq(0, N-1)],
    ToPrint = string:join([integer_to_list(X) || X <- L], " "),
    io:format("~s ~n", [ToPrint]),
    print_triangle(N + 1, Limit).

main() ->
    {ok, [N]} = io:fread("", "~d"),
    print_triangle(1, N).