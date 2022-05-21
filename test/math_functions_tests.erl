-module(math_functions_tests).
-include_lib("eunit/include/eunit.hrl").

even_valid_test() -> true = math_functions:even(2).
even_invalid_test() -> false = math_functions:even(1).

odd_valid_test() -> true = math_functions:odd(3).
odd_invalid_test() -> false = math_functions:odd(4).