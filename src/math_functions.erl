-module(math_functions).
-export([even/1, odd/1, filter/2, split/1, parse_integer/1]).

even(X) when X rem 2 =:= 0 -> true;
even(_) -> false.

odd(X) -> not even(X).

filter(F, L) -> [X || X <- L, F(X) =:= true].

split(L) ->
    even_and_odd_acc(L, [], []).

even_and_odd_acc([], Even, Odd) ->
    {lists:reverse(Even), lists:reverse(Odd)};

even_and_odd_acc([H|T], Even, Odd) ->
    case (even(H)) of
        true -> even_and_odd_acc(T, [H | Even], Odd);
        false -> even_and_odd_acc(T, Even, [H | Odd])
    end.

parse_integer(String) ->
    {Int, _} = string:to_integer(String),
    Int.