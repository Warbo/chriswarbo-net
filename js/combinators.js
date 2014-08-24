// Combinatory logic with the S and K combinators
// Combinators are either 'S', 'K' or an array of combinators

window.step_combinator = function(comb, allowed) {
	// "allowed" is a nullary function which returns a boolean, and
	// is called before each rewrite step. If it returns false, the
	// rewrite is aborted at its current state and this is returned.
	if (typeof allowed != typeof function(a){return a;}) {
		// If we've been given no condition, allow all rewrites
		allowed = function() { return true; };
	}
	// These are the base cases of our recursion
	if (comb === 'S') { return comb; }
	if (comb === 'K') { return comb; }
	
	// These are our rewrite rules
	// K x y -> x
	var pos = jQuery.inArray('K', comb);
	var args;
	if (pos >= 0 && pos < comb.length - 3 && allowed()) {
		args = comb.splice(pos, 3); // Pop off the elements
		comb.splice(pos, 0, args[1]); // Put the first argument to K back
	}
	// S x y z -> (x z) (y z)
	pos = jQuery.inArray('S', comb);
	if (pos >= 0 && pos < comb.length - 4 && allowed()) {
		args = comb.splice(pos, 4); // Pop off the elements
		comb.splice(pos, 0, [args[1], args[3]], [args[2], args[3]]); // Put the modified tree back
	}
	return _.map(comb, function(c) {return window.step_combinator(c, allowed); });
};
