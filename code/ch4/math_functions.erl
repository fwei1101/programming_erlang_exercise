-module(math_functions).
-export([
    even/1,
    odd/1,
    filter/2,
    split_1/1,
    split_2/1
]).

even(I) when is_integer(I) -> I rem 2 =:= 0.
odd(I) when is_integer(I) -> I rem 2 =:= 1.

filter(F, L) when is_function(F) andalso is_list(L) ->
    [X || X <- L, F(X)].

split_1(L) when is_list(L) ->
    {filter(fun(E) -> even(E) end, L), filter(fun(E) -> odd(E) end, L)}.
split_2(L) when is_list(L) ->
    split_3(L, [], []).
split_3([], Even, Odd) -> {Even, Odd};
split_3([H | T], Even, Odd) ->
    case even(H) of
        true -> split_3(T, [H | Even], Odd);
        false ->
            case odd(H) of
                true -> split_3(T, Even, [H | Odd]);
                false -> split_3(T, Even, Odd)
            end
    end.