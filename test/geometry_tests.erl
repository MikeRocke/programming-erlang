-module(geometry_tests).
-include_lib("eunit/include/eunit.hrl").

area_square_test() -> 25 = geometry:area({square, 5}).
area_rectangle_test() -> 12 = geometry:area({rectangle, 3, 4}).
area_circle_test() -> 28.274309999999996 = geometry:area({circle, 3}).
area_right_angle_triangle_test() -> 25.0 = geometry:area({right_triangle, 10, 5}).

perimeter_square_test() -> 12 = geometry:perimeter({square, 3}).
perimeter_rectangle_test() -> 20 = geometry:perimeter({rectangle, 4, 6}).
perimeter_circle_test() -> 31.4159 = geometry:perimeter({circle, 5}).
perimeter_right_triangle_test() -> 12.0 = geometry:perimeter({right_triangle, 3, 4}).