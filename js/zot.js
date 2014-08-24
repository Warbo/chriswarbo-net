// F --> F B    [F]([B])
// F --> e      ^c.cI
// B --> 0      ^c.c(^f.fSK)
// B --> 1      ^cL.L(^lR.R(^r.c(lr)))

var parse_zot = function(s, steps) {
	var I = function(x) {
		return x;
	};
	var K = function(x) {
		if (steps-- > 0) {
			return function(y) {
				if (steps-- > 0) {
					return x;
				}
				return I;
			};
		}
		return I;
	};
	var S = function(x) {
		if (steps-- > 0) {
			return function(y) {
				if (steps-- > 0) {
					return function(z) {
						if (steps-- > 0) {
							x(z)(y(z));
						}
						return I;
					};
				}
				return I;
			};
		}
		return I;
	};
	// Zot is a language where every natural number is a valid program, since
	// each bit in its binary representation is a function application.
	// Output in Zot is accomplished by applying the print function to a
	// combinator, so we bung this on the evaluation stack first.
	var interrogate = function(f) {
		return f(I)(I)(I)(K);
	};
	var print = function(x) {
		return x(x);
	}(function(self) {
		return function(c) {
			console.log(interrogate(c)('0')('1'));
			return self(self);
		};
	});
	var fs = [print, function(c) {
		if (steps-- > 0) {
			return c(I);
		}
		return I;
	}];
	_.each(s.split(''), function(a) {
		if (steps-- > 0) {
			if (a == '0') {
				// \c.c(\f.fSK)
				fs.push(function(c) {
					if (steps-- > 0) {
						return c(function(f) {
							if (steps-- > 0) {
								return f(S)(K);
							}
							return I;
						});
					}
					return I;
				});
			}
			else {
				// \cL.L(\lR.R(\r.c(lr)))
				fs.push(function(c) {
					if (steps-- > 0) {
						return function(L) {
							if (steps-- > 0) {
								return L(function(l) {
									if (steps-- > 0) {
										return function (R) {
											if (steps-- > 0) {
												return R(function(r){
													if (steps-- > 0) {
														return c(l(r));
													}
													return I;
												});
											}
											return I;
										};
									}
									return I;
								});
							}
							return I;
						};
					}
					return I;
				});
			}
		}
	});
	var res = fs.shift();
	while (fs.length > 0) {
		res = res(fs.shift());
	}
};