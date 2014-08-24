{$,Parser hint: pure}
window.levin_searchers = [];
window.levin_counter = 0;
window.levin_round = 1;
window.levin_lines = [];
window.gradient = [];
window.fittest = false;

window.update_enum = _.throttle(function() {
    if (parseInt($('#levin_enum').text()) < window.levin_counter) {
	$('#levin_enum').text(window.levin_counter+'');
    }
    $('#levin_this_enum').text(window.levin_counter+'');
}, 50);

$(function() {
	$('#levin_playfield').svg({onLoad: levin_init});
	$('#levin_playfield').click(function(){
                $('#levin_playfield').unbind('click');
		var new_n = 1;
		window.levin_searchers.push(1);
		window.setTimeout(function(){
			levin_step($('#levin_playfield').width() / 2, $('#levin_playfield').height() / 2, window.levin_searchers.length - 1);
		}, 10);
	});
	$('#levin_fitness_display').text('-1');
});
	
levin_init = function(svg) {
	_.times(Math.round(Math.random() * 100 + 1), function(){
		var l = Math.random();
		var r = Math.random();
		var t = Math.random();
		var b = Math.random();
		var temp;
		if (l > r) {
			temp = l;
			l = r;
			r = temp;
		}
		if (t > b) {
			temp = t;
			t = b;
			b = temp;
		}
		window.gradient.push(function(x, y, draw){
			var w = $('#levin_playfield').width();
			var h = $('#levin_playfield').height();
			if (draw) {
				$('#levin_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#levin_playfield').svg('get').rect(0,0,$('#levin_playfield').width(),$('#levin_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}

window.fitn = _.memoize(function(x, y) {
    return _.reduce(_.map(window.gradient, function(a) { return a(x,y); }), function(a, b) { return a+b; }, 0);
});

levin_step = function(x, y, n) {
    if (n > 0) {
        return;
    }
    if (window.levin_searchers.length == 0) {
        while (window.levin_lines[0].length > 0) {
            $('#levin_playfield').svg('get').remove(window.levin_lines[0].shift());
        }
        return;
    }
    if (window.levin_lines.length == 0) {
	window.levin_lines[0] = [];
    }

    // Get the next solution
    window.update_enum();
    if (window.levin_counter.toString(2).length > window.levin_round) {
        $('#levin_enum').text(window.levin_counter+'');
        window.levin_counter = 0;
        window.levin_round++;
        $('#levin_round').text(window.levin_round+'');
    }
    else {
        var allowed_steps = Math.pow(2, window.levin_round - (window.levin_counter.toString(2)).length);
        var pair = window.levin_decode(window.levin_counter, allowed_steps);
        if (pair === false) {
            // Disallow values points which take too much calculation
            // (this is a bit of a cheat, since we should really terminate
            // the calculation after this many steps, not run it to
            // completion and check afterwards. However, since our
            // calculations are guaranteed to halt we can get away with it)
	    fitness = 0;
	}
        else {
            fitness = window.fitn(pair);
            if (fitness > parseInt($('#levin_fitness_display').text())) {
                $('#levin_fitness_display').text(fitness+'');
		if (fittest !== false) {
		    $('#levin_playfield').svg('get').remove(window.fittest);
		}
		window.fittest = $('#levin_playfield').svg('get').circle(pair[0], pair[1], 5, {fill: 'lime'});
            }
            window.levin_lines[0].push($('#levin_playfield').svg('get').line(x, y, pair[0], pair[1], {stroke: 'white', 'stroke-width': 1}));
            x = pair[0];
            y = pair[1];
        }
        window.levin_counter++;
    }

    // Clean up old lines
    while (window.levin_lines[0].length > 10) {
        $('#levin_playfield').svg('get').remove(window.levin_lines[0].shift());
    }


    window.setTimeout(function(){ levin_step(x, y, n); }, 0);
};

window.levin_decode = function(n, max_steps) {
    var pair = _.map(window.cantor_split(n), _.bind(window.make_rational, {}, max_steps));
    if (pair[0] === false ||
        pair[1] === false ||
        pair[0] >= $('#levin_playfield').width() ||
        pair[1] >= $('#levin_playfield').height()) {
        return false;
    }
    return pair;
};

window.cantor_pair = _.memoize(function(x, y) {
    return Math.round(0.5*(x+y)*(x+y+1)*y);
});

window.cantor_split = function(z) {
    var w = Math.floor((Math.sqrt(8*z + 1)-1)/2);
    var t = (w*w + w) / 2;
    var y = z - t;
    var x = w - y;
    return [Math.round(x), Math.round(y)];
};

window.get_power = _.memoize(function(n) {
    if (n == 0) {
        return false;
    }
    return n.toString(2).length - 1;
});

// Decodes the given natural into a rational
window.make_rational = function(max_steps, n) {
    var steps = 0;
    var stage = 1;
    var e = 0;
    var bits = [];
    var fractions = [];
    var result = 1.0;
    while (steps < max_steps) {
        steps++;
        if (stage == 1) {
            // Build an array of the bit positions in n's binary
            // representation that are set to 1
            if (n == 0) {
                stage = 2;
            } else {
                e = get_power(n);
                steps += e + 1;  // It takes ~e+1 steps to find e
                bits.push(e);
                n -= Math.pow(2, e);
            }
        }
        else if (stage == 2) {
            // Now we've got our bit positions, turn them into
            // a continued fraction.
            // NOTE: We go backwards through the bits so as to process
            // them in ascending order
            if (bits.length > 0) {
                fractions.push(bits[bits.length - 1] + 1);
            }
            stage = 3;
        } else if (stage == 3) {
            if (bits.length < 2) {
                stage = 4;
            } else {
                e = bits.pop();
                fractions.push(bits[bits.length - 1] - e);
            }
        } else if (stage == 4) {
            if (fractions.length == 0) {
                stage = 5;
            } else {
                result = fractions.pop() + (1.0/result);
            }
        } else {
            result = result - 1;
            if (result == 0) {
                return 0;
            }
            return 1.0/result;
        }
   }
   return false;
};

