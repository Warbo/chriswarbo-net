// Search combinators

// A search combinator is any function which takes
// no arguments and returns a result. This simple
// interface represents a computation that hasn't
// been run yet (also known as a "thunk"). This
// nicely captures search and optimisation, as:
// 1) Search/optimisation algorithms never halt,
//    they can always give another value (even if
//    it stabilises to a constant), so we need to
//    represent these computations without actually
//    performing them (which would take forever).
//    Thunks effectively give us a lazy list (or a
//    generator, if you prefer) of the results.
// 2) Different search algorithms use different
//    numbers of parameters, and use variables at
//    multiple scope levels. A thunk interface
//    encapsulates all of this so that the
//    combinators can be composed interchangably.
//    We can still use parameters and state, but
//    they must be "baked" (curried) into the
//    combinators. This minimises shared state and
//    other such nastiness.
// 
// As a consequence of this currying, we generally
// don't ending up define combinators directly, but
// rather curryable functions (or 'combinator
// factories', if you prefer) which take any
// required parameters as arguments, set up any
// required state and return a combinator with
// these built-in.


// Utility functions

// Combines two arrays using the given function
var zip_with = function(f) {
    return function(a, b) {
        return a.map(function(v, k) {
            return f(v, b[k]);
        });
    };
};

// Reifies + ("add" may get confused with
// array.push)
var plus = function(a, b) { return a+b; };

// Likewise for * ("times" may get confused
// with iterate(f) or range(n))
var multiply = function(a, b) { return a*b; };

// Turns 0/1 into -1/1
var choose_direction = function(a) {
    return 2*a - 1;
};


// Combinators and combinator factories follow

// A constant combinator always gives the same
// value
var make_constant = function(val) {
    return function() {
        return val;
    };
};

// A stream of 1s
var one = constant(1);

// A reducer combines the results of a
// combinator using a reduction function.
// For function f, initial value init and
// results a, b, c, ... a reducer returns:
// f(init, a)
// f(f(init, a), b)
// f(f(f(init, a), b), c)
// ...
var make_reducer = function(init, f, comb) {
    return function() {
        init = f(init, comb());
        return init;
    };
};

// A useful example of a reducer. Produces
// a stream of the Natural numbers
var counter = make_reducer(0, plus, one);

// A reducer factory which appends results to an
// array
var make_hoarder = function(comb) {
    return make_reducer(
        [],
        function(arr, v) {
            if (typeof v === typeof []) return arr.concat([v]);
            return arr.concat(v);
	}
    );
};

// Enumerates the sentences of the given alphabet,
// according to the given combinator.
// The combinator should return numbers, and we
// turn these into sentences by first writing down
// the number in a base of the alphabet's length
// (eg.for English letters, this would be base 26)
// then we use each 'digit' of this representation
// to index the alphabet array.
// We combine these symbols by using the given
// reduction function, initialised with init.
// 
// As a concrete example, we can enumerate every
// second DNA strand in lexicographical order like
// this:
// make_enumerator(
//     ['A', 'C', 'G', 'T'],
//     plus,
//     '',
//     make_reducer(
//         0,
//         plus,
//         make_constant(2)
//     )
// );
var make_enumerator = function(alphabet, reduction, init, comb) {
    return function() {
        var val = comb();
        var result = init;
        var rem;
        do {
            rem = val % alphabet.length;
            val -= rem;
            val /= alphabet.length;
            result = reduction(result, alphabet[rem]);
        }
        while (val);
        return result;
    };
};

// Sends the given combinator's values through
// the given function. 
var make_applicator = function(f, comb) {
    return function() {
        return f(comb());
    };
};

// SIMPLE, as defined by Jurgen Schmidhuber.
// Returns every binary sequence in ascending
// order.
// 
// This version enumerates the sentences of the
// boolean alphabet
var simple = make_enumerator(
    [false, true],
    function(a, b) {
        return a.push(b);
    },
    [],
    counter
);
// This version applies a binary conversion to a
// counter
var simple = make_applicator(
    function(a) {
        return a.toString(2).split('').map(function(b) {
            return b=='1';
        });
    },
    counter
);

// Generates random values between 0 and 1
var uniform_random = function() {
    return Math.random();
};

// Generates random floats between 0 and n
var make_uniform_random = function(n) {
    return make_applicator(
        function(a) {
            return n*a;
	}
    );
};

// Generates random integers between 0 and n
var make_random_int = function(n) {
    return make_applicator(
        function(a) {
	    return Math.floor(a*n);
	},
        uniform_random
    );
};

// Generates random bits
var random_bit = make_random_int(2);

var random_step = make_applicator(choose_direction, random_bit);

// Random walk (1D)
var random_walk = make_reducer(0, plus, random_step);

// Returns samples from a Pareto distribution
var make_pareto = function(scale) {
    return function() {
	return Math.pow(1.0 - Math.random(), -1.0 / scale());
    };
};
var pareto = make_pareto(one);

var levy_flight = make_reducer(0, plus, pareto);

// Combines two combinators with the given function
var make_product = function(f, comb1, comb2) {
    return function() {
        return f(comb1(), comb2());
    };
};

// Takes 2 combinators a and b, gives a combinator
// which returns the results of a and b
var make_pair = function(a, b) {
    return function() {
        return [a(), b()];
    };
};

var make_scattered = function(n, nil, comb, choice) {
    return function() {
        var index = choice();
        var result = [];
        var length = n();
        for (var i=0; i < length; i++) {
            if (i == index) result.push(comb());
            else result.push(nil());
        }
        return result;
    };
};

// Makes a 2D walk from a boolean stream and a 1D
// walk. The booleans determine the axis to move
// along, the walk determines the distance
var make_mahattan_walk = function(horizontal, step) {
    return make_reducer(
        [0, 0],
        vector_plus,
        make_product(
            function(a, b) {
                if (a) return [b, 0];
                return [0, b];
            },
            horizontal,
            step
        )
    );
};

// Makes a 2D walk from two 1D walks, treating them
// as x and y step sizes
var make_cartesian_walk = function(x_step, y_step) {
    return make_reducer(
        [0, 0],
        vector_plus,
        make_pair(x_step, y_step)
        )
    );
};

// Makes a 2D walk from two 1D walks, treating them
// as an angle and a step size
var make_polar_walk = function(angle, distance) {
    return make_reducer(
        [0, 0],
        vector_plus,
        make_applicator(
            function(a) {
                var step = distance();
                return [step*Math.cos(a), step*Math.sin(a)];
            },
            angle()
        )
    );
};

