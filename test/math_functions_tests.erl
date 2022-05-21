-module(math_functions_tests).
-include_lib("eunit/include/eunit.hrl").

even_valid_test() -> true = math_functions:even(2).
even_invalid_test() -> false = math_functions:even(1).

odd_valid_test() -> true = math_functions:odd(3).
odd_invalid_test() -> false = math_functions:odd(4).

filter_even_test() -> [2,4,6,8,10] = math_functions:filter(fun math_functions:even/1, lists:seq(1, 10)).

split_empty_test() -> {[], []} = math_functions:split([]).
split_test() -> {[2,4], [1,3,5]} = math_functions:split(lists:seq(1,5)).
