-module(my_func).
-export([
    my_tuple_to_list/1
]).

my_tuple_to_list(T) when is_tuple(T) -> my_tuple_to_list_help(T, tuple_size(T), []).
my_tuple_to_list_help(_T, 0, L) -> L;
my_tuple_to_list_help(T, P, L) -> my_tuple_to_list_help(T, P - 1, [element(P, T) | L]).
