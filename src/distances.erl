-module(distances).
-export([manhattan/2]).


manhattan({X1, Y1}, {X2, Y2}) ->
    abs(X2-X1) + abs(Y2-Y1).