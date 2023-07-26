-module(my_func).
-export([
    my_tuple_to_list/1,
    my_time_func/1,
    my_date_string/0
]).

my_tuple_to_list(T) when is_tuple(T) -> my_tuple_to_list_help(T, tuple_size(T), []).
my_tuple_to_list_help(_T, 0, L) -> L;
my_tuple_to_list_help(T, P, L) -> my_tuple_to_list_help(T, P - 1, [element(P, T) | L]).

my_time_func(F) ->
    {_MegaSec, _Sec, MicroSecA} = erlang:now(),
    F(),
    {_, _, MicroSecB} = erlang:now(),
    io:fwrite("Execution of Function ~p took ~p microseconds.~n", [F, MicroSecB - MicroSecA]).

my_date_string() ->
    {Y, Mon, D} = erlang:date(),
    {H, Min, S} = erlang:time(),
    io:fwrite("~p-~p-~p ~p:~p:~p~n", [Y, Mon, D, H, Min, S]).