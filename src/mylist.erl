-module(mylist).
-export([my_tuple_to_list/1]).


my_tuple_to_list(T) -> my_tuple_to_list_acc(T, 1).

my_tuple_to_list_acc(T, N) when N > size(T) -> [];
my_tuple_to_list_acc(T, N) -> [element(N, T) | my_tuple_to_list_acc(T, N+1)].