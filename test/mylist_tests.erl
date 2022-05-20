-module(mylist_tests).
-include_lib("eunit/include/eunit.hrl").

my_tuple_to_list_empty_test() -> [] = mylist:my_tuple_to_list({}).
my_tuple_to_list_singular_tuple_test() -> [a] = mylist:my_tuple_to_list({a}).
my_tuple_to_list_multiple_tuple_test() -> [a, b, c] = mylist:my_tuple_to_list({a, b, c}).