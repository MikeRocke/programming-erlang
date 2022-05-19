-module(geometry).
-export([area/1, perimeter/1]).

-define(Pi, 3.14159).

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side}) -> Side * Side;
area({circle, Radius}) -> ?Pi * Radius * Radius;
area({right_triangle, Base, Height}) -> (Base * Height)/2.

perimeter({square, Side}) -> 4*Side;
perimeter({rectangle, Width, Height}) -> 2*Width + 2*Height;
perimeter({circle, Radius}) -> ?Pi * 2 * Radius;
perimeter({right_triangle, Base, Height}) -> Base + Height + math:sqrt(Base * Base + Height * Height).
