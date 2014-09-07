%% Copyright (c) 2013-2014 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(rational_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).


all() ->
    common:all().

groups() ->
    common:groups(?MODULE).

adding_quarters_to_thirds_test(_Config) ->
    Quarter = rational:new(1, 4),
    Third = rational:new(1, 3),
    Sum = rational:add(Quarter, Third),
    7 = rational:numerator(Sum),
    12 = rational:denominator(Sum).

adding_three_quarters_to_five_twelves_test(_Config) ->
    A = rational:new(3, 4),
    B = rational:new(5, 12),
    Sum = rational:add(A, B),
    56 = rational:numerator(Sum),
    48 = rational:denominator(Sum).

multiply_two_thirds_by_three_quarters_test(_Config) ->
    A = rational:new(2, 3),
    B = rational:new(3, 4),
    Product = rational:multiply(A, B),
    6 = rational:numerator(Product),
    12 = rational:denominator(Product).

simplify_test(_Config) ->
    A = rational:new(63, 462),
    Simplified = rational:simplify(A),
    3 = rational:numerator(Simplified),
    22 = rational:denominator(Simplified).

is_greater_than_test(_Config) ->
    A = rational:new(3, 4),
    B = rational:new(2, 4),
    true = rational:is_greater_than(A, B),
    true = rational:is_greater_or_equal(A, B),
    false = rational:is_greater_than(B, A),
    false = rational:is_greater_than(A, A),
    true = rational:is_greater_or_equal(A, A).

is_equal_to_test(_Config) ->    
    A = rational:new(3, 4),
    B = rational:new(2, 4),
    C = rational:new(1, 2),
    false = rational:is_equal_to(A, B),
    false = rational:is_equal_to(B, A),
    true = rational:is_equal_to(A, A),
    true = rational:is_equal_to(B, B),
    true = rational:is_equal_to(B, C),
    true = rational:is_equal_to(C, B).

is_less_than_test(_Config) ->
    A = rational:new(3, 4),
    B = rational:new(2, 4),
    false = rational:is_less_than(A, B),
    true = rational:is_less_or_equal(B, A),
    true = rational:is_less_than(B, A).


subtraction_test(_Config) ->
    A = rational:new(2, 3),
    B = rational:new(1, 2),
    Difference = rational:subtract(A, B),
    1 = rational:numerator(Difference),
    6 = rational:denominator(Difference).
    
mixed_numbers_test(_Config) ->
    A = rational:new(6),
    B = rational:new(3, 4),
    Product = rational:multiply(A, B),
    18 = rational:numerator(Product),
    4 = rational:denominator(Product).

reciprocal_test(_Config) ->
    A = rational:new(3, 4),
    Reciprocal = rational:reciprocal(A),
    4 = rational:numerator(Reciprocal),
    3 = rational:denominator(Reciprocal).

divide_test(_Config) ->
    A = rational:new(1, 2),
    B = rational:new(3, 4),
    R = rational:divide(A, B),
    4 = rational:numerator(R),
    6 = rational:denominator(R).

six_from_float_test(_Config) ->
    A = rational:from_float(6.0),
    6 = rational:numerator(A),
    1 = rational:denominator(A).

point_seven_five_from_float_test(_Config) ->
    A = rational:from_float(0.75),
    3 = rational:numerator(A),
    4 = rational:denominator(A).

point_five_from_float_test(_Config) ->
    A = rational:from_float(0.5),
    1 = rational:numerator(A),
    2 = rational:denominator(A).

greatest_common_divisor_test(_Config) ->
    6 = rational:gcd(48, 18).
