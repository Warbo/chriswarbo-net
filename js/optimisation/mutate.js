{$,Parser hint: pure}
window.mutate_population = [];
window.mutate_blobs = [];
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
	$('#mutate_playfield').svg({onLoad: mutate_init});
	$('#mutate_playfield').click(function(){
		$('#mutate_playfield').unbind('click');
		mutate_step();
	});
	$('#mutate_population').change(function(){
		$('#mutate_population_display').text($('#mutate_population').val());
	});
	$('#mutate_population').change();
	
	$('#mutate_fitness').text('0');
	
	$('#mutate_rate').change(function(){
		$('#mutate_rate_display').text(Math.round($('#mutate_rate').val()));
	});
	$('#mutate_rate').change();
});
	
mutate_init = function(svg) {
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
			var w = $('#mutate_playfield').width();
			var h = $('#mutate_playfield').height();
			if (draw) {
				$('#mutate_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#mutate_playfield').svg('get').rect(0,0,$('#mutate_playfield').width(),$('#mutate_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}

fitn_reduce = function(a,b) { return a + b; };
fitn = _.memoize(function(z) {
	var x = z[0];
	var y = z[1];
	if ((x < 0) || (x > $('#mutate_playfield').width())) { return 0; }
	if ((y < 0) || (y > $('#mutate_playfield').height())) { return 0; }
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
			
mutate = function(a) {
	if (typeof a === 'undefined') { return 0; }
	var x = a['solution'][0];
	var y = a['solution'][1];
	var w = $('#mutate_playfield').width();
	var h = $('#mutate_playfield').height();
	var new_x;
	var new_y;
	var mutate_rate = $('#mutate_rate').val() / 16.0;
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
	return {blob:$('#mutate_playfield').svg('get').circle(new_x, new_y, 2, {fill: 'red'}),
		solution:[new_x, new_y]
	};
}

window.root2 = Math.sqrt(2);

window.get_mid = _.memoize(function(top, bottom) {
	return Math.ceil((top - bottom) / window.root2);
});

window.reproduce = function() {
	// Choose a parent probabilistically. We divide our population in 2
	// over and over until we narrow down a single solution.
	var top = window.mutate_population.length;
	var bottom = 0;
	// This function finds the point x such that the probability from
	// bottom to x is equal to the probability from x to top
	var mid;
	// Beware while loops! Do as little as possible in them.
	while (top - bottom > 1) {
		mid = get_mid(top, bottom);
		if (Math.random() > 0.5) {
			top = mid;
		}
		else {
			bottom = mid;
		}
	}
	// We have a parent, mutate it
	window.new_solutions.push(mutate(window.mutate_population[Math.ceil(bottom)]));
};

window.draw_blobs = function(x) {
	solution = window.mutate_population[window.mutate_population.length - 1];
	window.mutate_blobs.push(
		$('#mutate_playfield').svg('get').circle(solution['solution'][0], solution['solution'][1], 5, {fill: 'lime'})
	);
};
		
mutate_step = function() {
	// Grab our population first (if we keep reading the input, we
	// may get inconsistent values)
	var pop = parseInt($('#mutate_population').val());
	
	// We are swapping 1/4 of the population each time
	var to_swap = Math.ceil(pop / 4.0);
	
	window.new_solutions = [];
	
	// Bulk out the population if the slider has been increased
	if (pop > window.mutate_population.length) {
		window.mutate_population.reverse();
		window.mutate_population.push.apply(window.mutate_population, _.map(_.range(pop - window.mutate_population.length), function(x) {
			return {
				blob:false,
				solution:[Math.round(Math.random()*$('#mutate_playfield').width()),Math.round(Math.random()*$('#mutate_playfield').height())]
			};
		}));
		window.mutate_population.reverse();
	}
	
	_.times(to_swap, window.reproduce);
	
	// Sort the population by fitness
	if ($('#mutate_stable').is(':checked')) {
		window.mutate_population = merge_sort(window.mutate_population, comp);
	}
	else {
		window.mutate_population.sort(comp);
	}
	
	$('#mutate_fitness').text(fitn(window.mutate_population[window.mutate_population.length - 1]['solution'])+"");
	
	// At this point we have a sorted array, so draw our blobs
	while (window.mutate_blobs.length > 0) {
		$('#mutate_playfield').svg('get').remove(window.mutate_blobs.shift());
	}

	if (window.mutate_population.length >= 5) {
		_.each(_.range(5), window.draw_blobs);
	}

	// Throw away any excess solutions and the least-fit solutions
	var sol;
	while (window.mutate_population.length > pop) {
		window.mid_points = [];
		sol = mutate_population.shift();
		if (sol['blob']) {
			$('#mutate_playfield').svg('get').remove(sol['blob']);
		}
	}
	
	// Add the new solutions to the mix
	window.mutate_population.reverse();
	window.mutate_population.push.apply(window.mutate_population, window.new_solutions);
	window.mutate_population.reverse();
	
	window.setTimeout(function(){ mutate_step(); }, 10);
}