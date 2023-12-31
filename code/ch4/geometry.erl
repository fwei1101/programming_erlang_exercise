-module(geometry).
-export([test/0, area/1]).

test() ->
    12 = area({rectangle, 3, 4}),
    144 = area({square, 12}),
    3.14159 = area({circle, 1}),
    8 = area({right_triangle, 4, 4}),
    tests_succeed.

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side}) -> Side * Side;
area({circle, Radius}) -> 3.14159 * Radius * Radius;
area({right_triangle, LegA, LegB}) -> LegA * LegB / 2.