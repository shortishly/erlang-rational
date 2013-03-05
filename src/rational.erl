
-module(rational).

-export([new/1, new/2, ratio/1, add/2, subtract/2, multiply/2,
	 simplify/1, reciprocal/1, divide/2, numerator/1,
	 denominator/1, is_greater_than/2, is_less_than/2,
	 is_equal_to/2, is_greater_or_equal/2, is_less_or_equal/2,
	 from_float/1, to_float/1]).

-export_type([numerator/0, denominator/0, ratio/0, fraction/0]).

-type numerator() :: integer().
-type denominator() :: pos_integer().

-type ratio() :: {numerator(), denominator()}.

-opaque fraction() :: fun((numerator) -> numerator())
		  | fun((denominator) -> denominator())
		  | fun((ratio) -> ratio())
		  | fun(({add, fraction()}) -> fraction())
		  | fun(({subtract, fraction()}) -> fraction())
		  | fun(({multiply, fraction()}) -> fraction())
		  | fun(({reciprocal, fraction()}) -> fraction())
		  | fun(({divide, fraction()}) -> fraction())
		  | fun((simplify) -> fraction())
		  | fun((is_greater_than, fraction()) -> boolean())
		  | fun((is_less_than, fraction()) -> boolean())
		  | fun((is_equal_to, fraction()) -> boolean()) 
		  | fun((is_less_or_equal, fraction()) -> boolean())
		  | fun((is_greater_or_equal, fraction()) -> boolean()).


-spec numerator(fraction()) -> numerator().
numerator(Fraction) ->
    Fraction(numerator).

-spec denominator(fraction()) -> denominator().
denominator(Fraction) ->
    Fraction(denominator).

-spec ratio(fraction()) -> ratio().
ratio(Fraction) ->
    Fraction(ratio).

-spec add(fraction(), fraction()) -> fraction().
add(A, B) ->
    A({add, B}).

-spec subtract(fraction(), fraction()) -> fraction().
subtract(A, B) ->
    A({subtract, B}).

-spec multiply(fraction(), fraction()) -> fraction().
multiply(A, B) ->
    A({multiply, B}).

-spec reciprocal(fraction()) -> fraction().
reciprocal(Fraction) ->
    Fraction(reciprocal).

-spec divide(fraction(), fraction()) -> fraction().
divide(A, B) ->
    multiply(A, reciprocal(B)).

-spec simplify(fraction()) -> fraction().
simplify(Fraction) ->
    Fraction(simplify).

-spec is_greater_than(fraction(), fraction()) -> boolean().
is_greater_than(F1, F2) ->
    F1({is_greater_than, F2}).

-spec is_less_than(fraction(), fraction()) -> boolean().
is_less_than(F1, F2) ->
    F1({is_less_than, F2}).

-spec is_equal_to(fraction(), fraction()) -> boolean().
is_equal_to(F1, F2) ->
    F1({is_equal_to, F2}).

is_less_or_equal(F1, F2)->
    F1({is_less_or_equal, F2}).

is_greater_or_equal(F1, F2) ->
    F1({is_greater_or_equal, F2}).

to_float(F) ->
    F(to_float).



-spec new(numerator()) -> fraction().
new(Numerator) when is_integer(Numerator) ->
    new(Numerator, 1).


-spec new(numerator(), denominator()) -> fraction().
new(Numerator, Denominator) ->
    fun(numerator) ->
	    Numerator;

       (denominator) ->
	    Denominator;

       (ratio) ->
	    {Numerator, Denominator};

       (to_float) ->
	    Numerator / Denominator;

       (reciprocal) ->
	    new(Denominator, Numerator);

       (simplify) ->
	    case gcd(Numerator, Denominator) of
		1 ->
		    new(Numerator, Denominator);
		GCD ->
		    new(Numerator div GCD, Denominator div GCD)
	    end;

       ({is_greater_than, Fraction}) ->
	    (Numerator * denominator(Fraction)) > (numerator(Fraction) * Denominator);

       ({is_less_than, Fraction}) ->
	    (Numerator * denominator(Fraction)) < (numerator(Fraction) * Denominator);

       ({is_equal_to, Fraction}) ->
	    (Numerator * denominator(Fraction)) =:= (Denominator * numerator(Fraction));

       ({is_greater_or_equal, F2}) ->
	    F1 = new(Numerator, Denominator),
	    is_greater_than(F1, F2) orelse is_equal_to(F1, F2);

       ({is_less_or_equal, F2}) ->
	    F1 = new(Numerator, Denominator),
	    is_less_than(F1, F2) orelse is_equal_to(F1, F2);

       ({add, Fraction}) ->
	    new((Numerator * denominator(Fraction)) + (Denominator * numerator(Fraction)), Denominator * denominator(Fraction));

       ({subtract, Fraction}) ->
	    new((Numerator * denominator(Fraction)) - (Denominator * numerator(Fraction)), Denominator * denominator(Fraction));
	    
       ({multiply, Fraction}) ->
	    new(Numerator * numerator(Fraction), 
		Denominator * denominator(Fraction))
    end.

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


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

gcd_test() ->
    ?assertEqual(6, gcd(48, 18)).

-endif.
