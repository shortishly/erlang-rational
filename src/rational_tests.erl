-module(rational_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_addition() ->
    ?FORALL({A, B, C, D},
	    {rational:numerator(), rational:denominator(), rational:numerator(), rational:denominator()},
	    begin
		R1 = rational:new(A, B),
		R2 = rational:new(C, D),
		{Numerator, Denominator} = rational:ratio(rational:add(R1, R2)),
		ok =:= ?assertEqual((A*D) + (B*C), Numerator) andalso
		    ok =:= ?assertEqual(B*D, Denominator)
	    end).

prop_subtraction() ->
    ?FORALL({A, B, C, D},
	    {rational:numerator(), rational:denominator(), rational:numerator(), rational:denominator()},
	    begin
		R1 = rational:new(A, B),
		R2 = rational:new(C, D),
		{Numerator, Denominator} = rational:ratio(rational:subtract(R1, R2)),
		ok =:= ?assertEqual((A*D) - (B*C), Numerator) andalso
		    ok =:= ?assertEqual(B*D, Denominator)
	    end).

prop_multiplication() ->
    ?FORALL({A, B, C, D},
	    {rational:numerator(), rational:denominator(), rational:numerator(), rational:denominator()},
	    begin
		R1 = rational:new(A, B),
		R2 = rational:new(C, D),
		{Numerator, Denominator} = rational:ratio(rational:multiply(R1, R2)),
		ok =:= ?assertEqual(A*C, Numerator) andalso
		    ok =:= ?assertEqual(B*D, Denominator)
	    end).

prop_division() ->
    ?FORALL({A, B, C, D},
	    {rational:numerator(), rational:denominator(), rational:numerator(), rational:denominator()},
	    begin
		R1 = rational:new(A, B),
		R2 = rational:new(C, D),
		{Numerator, Denominator} = rational:ratio(rational:divide(R1, R2)),
		ok =:= ?assertEqual(A*D, Numerator) andalso
		    ok =:= ?assertEqual(B*C, Denominator)
	    end).

prop_ordering() ->
    ?FORALL({A, B, C, D},
	    {rational:numerator(), rational:denominator(), rational:numerator(), rational:denominator()},
	    begin
		R1 = rational:new(A, B),
		R2 = rational:new(C, D),

		case {A*D, B*C} of
		    {AD, BC} when AD =:= BC ->
			rational:is_equal_to(R1, R2) andalso
			    not(rational:is_less_than(R1, R2)) andalso
			    not(rational:is_less_than(R2, R1)) andalso
			    not(rational:is_greater_than(R1, R2)) andalso
			    not(rational:is_greater_than(R2, R1)) andalso
			    rational:is_greater_or_equal(R1, R2) andalso
			    rational:is_greater_or_equal(R2, R1) andalso
			    rational:is_less_or_equal(R1, R2) andalso
			    rational:is_less_or_equal(R2, R1);

		    {AD, BC} when AD < BC ->
			not(rational:is_equal_to(R1, R2)) andalso
			    rational:is_less_than(R1, R2) andalso
			    not(rational:is_less_than(R2, R1)) andalso
			    not(rational:is_greater_than(R1, R2)) andalso
			    rational:is_greater_than(R2, R1) andalso
			    not(rational:is_greater_or_equal(R1, R2)) andalso
			    rational:is_greater_or_equal(R2, R1) andalso
			    rational:is_less_or_equal(R1, R2) andalso
			    not(rational:is_less_or_equal(R2, R1));

		    {AD, BC} when AD > BC ->
			not(rational:is_equal_to(R1, R2)) andalso
			    not(rational:is_less_than(R1, R2)) andalso
			    rational:is_less_than(R2, R1) andalso
			    rational:is_greater_than(R1, R2) andalso
			    not(rational:is_greater_than(R2, R1)) andalso
			    rational:is_greater_or_equal(R1, R2) andalso
			    not(rational:is_greater_or_equal(R2, R1)) andalso
			    not(rational:is_less_or_equal(R1, R2)) andalso
			    rational:is_less_or_equal(R2, R1)
		end
	    end).
    
    

proper_test_() ->
    [?_assert(proper:quickcheck(?MODULE:F(), [1000, long_result])) || {F, 0} <- module_info(exports),
								      F > 'prop_',
								      F < 'prop`'].




adding_quarters_to_thirds_test() ->
    Quarter = rational:new(1, 4),
    Third = rational:new(1, 3),
    Sum = rational:add(Quarter, Third),
    ?assertEqual(7, rational:numerator(Sum)),
    ?assertEqual(12, rational:denominator(Sum)).

adding_three_quarters_to_five_twelves_test() ->
    A = rational:new(3, 4),
    B = rational:new(5, 12),
    Sum = rational:add(A, B),
    ?assertEqual(56, rational:numerator(Sum)),
    ?assertEqual(48, rational:denominator(Sum)).

multiply_two_thirds_by_three_quarters_test() ->
    A = rational:new(2, 3),
    B = rational:new(3, 4),
    Product = rational:multiply(A, B),
    ?assertEqual(6, rational:numerator(Product)),
    ?assertEqual(12, rational:denominator(Product)).

simplify_test() ->
    A = rational:new(63, 462),
    Simplified = rational:simplify(A),
    ?assertEqual(3, rational:numerator(Simplified)),
    ?assertEqual(22, rational:denominator(Simplified)).

is_greater_than_test() ->
    A = rational:new(3, 4),
    B = rational:new(2, 4),
    ?assert(rational:is_greater_than(A, B)),
    ?assert(rational:is_greater_or_equal(A, B)),
    ?assertNot(rational:is_greater_than(B, A)),
    ?assertNot(rational:is_greater_than(A, A)),
    ?assert(rational:is_greater_or_equal(A, A)).

is_equal_to_test() ->    
    A = rational:new(3, 4),
    B = rational:new(2, 4),
    C = rational:new(1, 2),
    ?assertNot(rational:is_equal_to(A, B)),
    ?assertNot(rational:is_equal_to(B, A)),
    ?assert(rational:is_equal_to(A, A)),
    ?assert(rational:is_equal_to(B, B)),
    ?assert(rational:is_equal_to(B, C)),
    ?assert(rational:is_equal_to(C, B)).

is_less_than_test() ->
    A = rational:new(3, 4),
    B = rational:new(2, 4),
    ?assertNot(rational:is_less_than(A, B)),
    ?assert(rational:is_less_or_equal(B, A)),
    ?assert(rational:is_less_than(B, A)).


subtraction_test() ->
    A = rational:new(2, 3),
    B = rational:new(1, 2),
    Difference = rational:subtract(A, B),
    ?assertEqual(1, rational:numerator(Difference)),
    ?assertEqual(6, rational:denominator(Difference)).
    
mixed_numbers_test() ->
    A = rational:new(6),
    B = rational:new(3, 4),
    Product = rational:multiply(A, B),
    ?assertEqual(18, rational:numerator(Product)),
    ?assertEqual(4, rational:denominator(Product)).

reciprocal_test() ->
    A = rational:new(3, 4),
    Reciprocal = rational:reciprocal(A),
    ?assertEqual(4, rational:numerator(Reciprocal)),
    ?assertEqual(3, rational:denominator(Reciprocal)).

divide_test() ->
    A = rational:new(1, 2),
    B = rational:new(3, 4),
    R = rational:divide(A, B),
    ?assertEqual(4, rational:numerator(R)),
    ?assertEqual(6, rational:denominator(R)).

six_from_float_test() ->
    A = rational:from_float(6.0),
    ?assertEqual(6, rational:numerator(A)),
    ?assertEqual(1, rational:denominator(A)).

point_seven_five_from_float_test() ->
    A = rational:from_float(0.75),
    ?assertEqual(3, rational:numerator(A)),
    ?assertEqual(4, rational:denominator(A)).

point_five_from_float_test() ->
    A = rational:from_float(0.5),
    ?assertEqual(1, rational:numerator(A)),
    ?assertEqual(2, rational:denominator(A)).
    
    
