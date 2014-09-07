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

-module(rational).

-export([
	 new/1, 
	 new/2, 
	 ratio/1, 
	 add/2, 
	 subtract/2, 
	 multiply/2,
	 simplify/1, 
	 reciprocal/1, 
	 divide/2, 
	 numerator/1,
	 denominator/1,
	 is_greater_than/2, 
	 is_less_than/2,
	 is_equal_to/2,
	 is_greater_or_equal/2, 
	 is_less_or_equal/2,
	 from_float/1, 
	 to_float/1,
	 gcd/2
	]).

-export_type([
	      numerator/0, 
	      denominator/0, 
	      fraction/0, 
	      ratio/0
	     ]).

-type numerator() :: integer().
-type denominator() :: pos_integer().

-record(fraction, {
	  numerator :: numerator(), 
	  denominator :: denominator()
	 }).

-type ratio() :: {numerator(), denominator()}.

-opaque fraction() :: #fraction{}.

-spec new(numerator()) -> fraction().
new(Numerator) ->
    #fraction{numerator = Numerator, denominator = 1}.

-spec new(numerator(), denominator()) -> fraction().
new(Numerator, Denominator) ->
    #fraction{numerator = Numerator, denominator = Denominator}.

-spec numerator(fraction()) -> numerator().
numerator(#fraction{numerator = Numerator}) ->
    Numerator.

-spec denominator(fraction()) -> denominator().
denominator(#fraction{denominator = Denominator}) ->
    Denominator.

-spec ratio(fraction()) -> ratio().
ratio(#fraction{numerator = Numerator, denominator = Denominator}) ->
    {Numerator, Denominator}.

-spec add(fraction(), fraction()) -> fraction().
add(#fraction{numerator = N1, denominator = D1}, 
    #fraction{numerator = N2, denominator = D2}) ->
    new((N1 * D2) + (D1 * N2), D1 * D2).

-spec subtract(fraction(), fraction()) -> fraction().
subtract(#fraction{numerator = N1, denominator = D1},
	 #fraction{numerator = N2, denominator = D2}) ->
    new((N1 * D2) - (D1 * N2), D1 * D2).

-spec multiply(fraction(), fraction()) -> fraction().
multiply(#fraction{numerator = N1, denominator = D1},
	 #fraction{numerator = N2, denominator = D2}) ->
    new(N1 * N2, D1 * D2).

-spec reciprocal(fraction()) -> fraction().
reciprocal(#fraction{numerator = Numerator, denominator = Denominator}) ->
    new(Denominator, Numerator).

-spec divide(fraction(), fraction()) -> fraction().
divide(A, B) ->
    multiply(A, reciprocal(B)).

-spec simplify(fraction()) -> fraction().
simplify(#fraction{numerator = Numerator, denominator = Denominator} = F) ->
    case gcd(Numerator, Denominator) of
	1 ->
	    F;
	GCD ->
	    new(Numerator div GCD, Denominator div GCD)
    end.


comparison(Fn, 
	#fraction{numerator = N1,
		  denominator = D1},
	#fraction{numerator = N2,
		  denominator = D2}) ->
    Fn(N1 * D2, N2 * D1).


-spec is_greater_than(fraction(), fraction()) -> boolean().
is_greater_than(F1, F2) ->
    comparison(fun erlang:'>'/2, F1, F2).

-spec is_less_than(fraction(), fraction()) -> boolean().
is_less_than(F1, F2) ->
    comparison(fun erlang:'<'/2, F1, F2).

-spec is_equal_to(fraction(), fraction()) -> boolean().
is_equal_to(F1, F2) ->
    comparison(fun erlang:'=='/2, F1, F2).

-spec is_less_or_equal(fraction(), fraction()) -> boolean().
is_less_or_equal(F1, F2) ->
    comparison(fun erlang:'=<'/2, F1, F2).

-spec is_greater_or_equal(fraction(), fraction()) -> boolean().
is_greater_or_equal(F1, F2) ->
    comparison(fun erlang:'>='/2, F1, F2).

-spec to_float(fraction()) -> float().
to_float(#fraction{numerator = Numerator,
		   denominator = Denominator}) ->
    Numerator / Denominator.

-spec from_float(float()) -> fraction().
from_float(Float) when is_float(Float) ->
    from_float(Float, 1).

from_float(Numerator, Denominator) when Numerator == trunc(Numerator) ->
    simplify(new(trunc(Numerator), Denominator));
from_float(Numerator, Denominator) ->
    from_float(Numerator * 10, Denominator * 10).


gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).
