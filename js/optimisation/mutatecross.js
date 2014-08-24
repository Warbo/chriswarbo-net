{$,Parser hint: pure}
window.mutatecross_population = [];
window.mutatecross_blobs = [];
window.gradient = [];

// Literate Programming merge sort (stable, in-place sort)
function msort(array, begin, end)
{
	var size=end-begin;
	if(size<2) return;

	var begin_right=begin+Math.floor(size/2);

	msort(array, begin, begin_right);
	msort(array, begin_right, end);
	merge_inplace(array, begin, begin_right, end);
}

function merge_sort_inplace(array)
{
	msort(array, 0, array.length);
}

function merge_sort(array,comparison)
{
	if(array.length < 2)
		return array;
	var middle = Math.ceil(array.length/2);
	return merge(merge_sort(array.slice(0,middle),comparison),
			merge_sort(array.slice(middle),comparison),
			comparison);
}

function merge_sort(array,comparison)
{
	if(array.length < 2)
		return array;
	var middle = Math.ceil(array.length/2);
	return merge(merge_sort(array.slice(0,middle),comparison),
			merge_sort(array.slice(middle),comparison),
			comparison);
}

Array.prototype.swap=function(a, b)
{
	var tmp=this[a];
	this[a]=this[b];
	this[b]=tmp;
}

function insert(array, begin, end, v)
{
	while(begin+1<end && array[begin+1]<v) {
		array.swap(begin, begin+1);
		++begin;
	}
	array[begin]=v;
}

function merge(left,right,comparison)
{
	var result = new Array();
	while((left.length > 0) && (right.length > 0))
	{
		if(comparison(left[0],right[0]) <= 0)
			result.push(left.shift());
		else
			result.push(right.shift());
	}
	while(left.length > 0)
		result.push(left.shift());
	while(right.length > 0)
		result.push(right.shift());
	return result;
}

// End merge sort

$(function() {
	$('#mutatecross_playfield').svg({onLoad: mutatecross_init});
	$('#mutatecross_playfield').click(function(){
		$('#mutatecross_playfield').unbind('click');
		mutatecross_step();
	});
	$('#mutatecross_population').change(function(){
		$('#mutatecross_population_display').text($('#mutatecross_population').val());
	});
	$('#mutatecross_population').change();
	
	$('#mutatecross_fitness').text('0');
	
	$('#mutatecross_cross_rate').change(function(){
		$('#mutatecross_cross_rate_display').text(Math.round($('#mutatecross_cross_rate').val()));
	});
	$('#mutatecross_cross_rate').change();
	$('#mutatecross_mutate_rate').change(function(){
		$('#mutatecross_mutate_rate_display').text(Math.round($('#mutatecross_mutate_rate').val()));
	});
	$('#mutatecross_mutate_rate').change();
});
	
mutatecross_init = function(svg) {
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
			var w = $('#mutatecross_playfield').width();
			var h = $('#mutatecross_playfield').height();
			if (draw) {
				$('#mutatecross_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#mutatecross_playfield').svg('get').rect(0,0,$('#mutatecross_playfield').width(),$('#mutatecross_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
};

fitn_reduce = function(a,b) { return a + b; };
fitn = _.memoize(function(z) {
	var x = z[0];
	var y = z[1];
	if ((x < 0) || (x > $('#mutatecross_playfield').width())) { return 0; }
	if ((y < 0) || (y > $('#mutatecross_playfield').height())) { return 0; }
	return _.reduce(_.map(window.gradient, function(a){ return a(x, y, false); }), fitn_reduce);
});

// Compare 2 solutions based on their fitness
comp = function (a, b) {
	var fit_a = fitn(a['solution']);
	var fit_b = fitn(b['solution']);
	if (fit_a < fit_b) {
		return -1;
	}
	if (fit_a > fit_b) {
		return 1;
	}
	return 0;
};
			
crossover = function(a, b) {
	if (typeof a === 'undefined' || typeof b == 'undefined') { return 0; }
	// Duplicate the parents, turning [x, y] into xy
	var child1 = (512 * a['solution'][0]) + a['solution'][1];
	var child2 = (512 * b['solution'][0]) + b['solution'][1];
	var w = $('#mutatecross_playfield').width();
	var h = $('#mutatecross_playfield').height();
	var mutatecross_points = [];
	_.times($('#mutatecross_cross_rate').val(), function() {
		do {
			point = Math.round(Math.random() * 16.0);
		} while ($.inArray(point, mutatecross_points) !== -1);
		mutatecross_points.push(point);
	});
	_.each(mutatecross_points, function(p) {
		var point = Math.pow(2, p);
		var section1 = child1 % point;
		var section2 = child2 % point;
		child1 = child1 - section1 + section2;
		child2 = child2 - section2 + section1;
	});
	// Now split apart the x and y again
	var temp = child1 % 512;
	child1 = {
		solution:[temp, (child1 - temp) / 512],
		blob:false
	};
	temp = child2 % 512;
	child2 = {
		solution:[temp, (child2 - temp) / 512],
		blob:false
	};
	return [child1, child2];
};

mutate = function(a) {
	if (typeof a === 'undefined') { return 0; }
	var x = a['solution'][0];
	var y = a['solution'][1];
	var w = $('#mutatecross_playfield').width();
	var h = $('#mutatecross_playfield').height();
	var new_x;
	var new_y;
	var mutate_rate = 1.0 / $('#mutatecross_mutate_rate').val();
	do {
		new_x = x;
		new_y = y;
		_.each([1, 2, 4, 8, 16, 32, 64, 256], function(a) {
			if (Math.random() < mutate_rate) {
				if (new_x & a == a) {
					new_x -= a;		// Flip 1 to 0
				}
				else {
					new_x += a;		// Flip 0 to 1
				}
			}
			if (Math.random() < mutate_rate) {
				if (new_y & a == a) {
					new_y -= a;		// Flip 1 to 0
				}
				else {
					new_y += a;		// Flip 0 to 1
				}
			}
		});
	} while ((0 > new_x) || (new_x > w) || (0 > new_y) || (new_y > h));
	return {
		solution:[new_x, new_y],
		blob:$('#mutatecross_playfield').svg('get').circle(new_x, new_y, 2, {fill: 'red'})
	};
};

mutatecross = function(a, b) { return _.map(crossover(a, b), mutate); };

window.root2 = Math.sqrt(2);

window.get_mid = _.memoize(function(top, bottom) {
	return Math.ceil((top - bottom) / window.root2);
});

window.reproduce = function() {
	// Choose a parent probabilistically. We divide our population in 2
	// over and over until we narrow down a single solution.
	var top = window.mutatecross_population.length;
	var bottom = 0;
	// This function finds the point x such that the probability from
	// bottom to x is equal to the probability from x to top
	var mid;
	var parents = [];
	// Beware while loops! Do as little as possible in them.
	do {
		top = window.mutatecross_population.length;
		bottom = 0;
		while (top - bottom > 1) {
			mid = get_mid(top, bottom);
			if (Math.random() > 0.5) {
				top = mid;
			}
			else {
				bottom = mid;
			}
		}
		bottom = Math.ceil(bottom);
		if ($.inArray(bottom, parents) === -1) {
			parents.push(Math.ceil(bottom));
		}
	} while (parents.length < 2);
	// We have 2 parents, perform a mutated crossover
	var children = mutatecross(window.mutatecross_population[parents[0]], window.mutatecross_population[parents[1]]);
	window.new_solutions.push(children[0])
	window.new_solutions.push(children[1]);
};

window.colours = ['red', 'red', 'orange', 'orange', 'lime'];
window.draw_blobs = function(x) {
	var solution = window.mutatecross_population[window.mutatecross_population.length - 1]['solution'];
	window.mutatecross_blobs.push(
		$('#mutatecross_playfield').svg('get').circle(solution[0], solution[1], 5, {fill: 'lime'})
	);
};
		
mutatecross_step = function() {
	// Grab our population first (if we keep reading the input, we
	// may get inconsistent values)
	var pop = parseInt($('#mutatecross_population').val());

	// We are swapping 1/4 of the population each time
	var to_swap = Math.ceil(pop / 4.0);

	window.new_solutions = [];

	// Bulk out the population if the slider has been increased
	if (pop > window.mutatecross_population.length) {
		window.mutatecross_population.reverse();
		window.mutatecross_population.push.apply(window.mutatecross_population, _.map(_.range(pop - window.mutatecross_population.length), function(x) {
			var x = Math.round(Math.random()*$('#mutatecross_playfield').width());
			var y = Math.round(Math.random()*$('#mutatecross_playfield').height());
			return {
				solution:[x,y],
				blob: $('#mutatecross_playfield').svg('get').circle(x, y, 2, {fill: 'red'})
			};
		}));
		window.mutatecross_population.reverse();
	}

	_.times(to_swap, window.reproduce);

	// Sort the population by fitness
	if ($('#mutatecross_stable').is(':checked')) {
		window.mutatecross_population = merge_sort(window.mutatecross_population, comp);
	}
	else {
		window.mutatecross_population.sort(comp);
	}

	$('#mutatecross_fitness').text(fitn(window.mutatecross_population[window.mutatecross_population.length - 1]['solution'])+"");

	// At this point we have a sorted array, so draw our blobs
	while (window.mutatecross_blobs.length > 0) {
		$('#mutatecross_playfield').svg('get').remove(window.mutatecross_blobs.shift());
	}

	if (window.mutatecross_population.length >= 1) {
		window.draw_blobs(0);
	}

	// Throw away any excess solutions and the least-fit solutions
	var s;
	while (window.mutatecross_population.length > pop - to_swap) {
		s = window.mutatecross_population.shift();
		if (s['blob']) {
			$('#mutatecross_playfield').svg('get').remove(s['blob']);
		}
	}

	// Add the new solutions to the mix
	window.mutatecross_population.reverse();
	window.mutatecross_population.push.apply(window.mutatecross_population, window.new_solutions);
	window.mutatecross_population.reverse();

	window.setTimeout(function(){ mutatecross_step(); }, 10);
};
