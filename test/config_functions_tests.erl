-module(config_functions_tests).
-include_lib("eunit/include/eunit.hrl").


map_search_pred_test() -> Input = #{ a => 2, b => 3, c => 4 },  {b, 3} = config_functions:map_search_pred(Input, fun(A, B) -> {A, B} == {b,3} end).