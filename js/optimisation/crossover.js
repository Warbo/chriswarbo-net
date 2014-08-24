{$,Parser hint: pure}
window.crossover_population = [];
window.crossover_blobs = [];
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
	$('#crossover_playfield').svg({onLoad: crossover_init});
	$('#crossover_playfield').click(function(){
		$('#crossover_playfield').unbind('click');
		crossover_step();
	});
	$('#crossover_population').change(function(){
		$('#crossover_population_display').text($('#crossover_population').val());
	});
	$('#crossover_population').change();
	
	$('#crossover_fitness').text('0');
	
	$('#crossover_rate').change(function(){
		$('#crossover_rate_display').text(Math.round($('#crossover_rate').val()));
	});
	$('#crossover_rate').change();
});
	
crossover_init = function(svg) {
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
			var w = $('#crossover_playfield').width();
			var h = $('#crossover_playfield').height();
			if (draw) {
				$('#crossover_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#crossover_playfield').svg('get').rect(0,0,$('#crossover_playfield').width(),$('#crossover_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}

fitn_reduce = function(a,b) { return a + b; };
fitn = _.memoize(function(z) {
	var x = z[0];
	var y = z[1];
	if ((x < 0) || (x > $('#crossover_playfield').width())) { return 0; }
	if ((y < 0) || (y > $('#crossover_playfield').height())) { return 0; }
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
	var w = $('#crossover_playfield').width();
	var h = $('#crossover_playfield').height();
	var crossover_points = [];
	_.times($('#crossover_rate').val(), function() {
		do {
			point = Math.round(Math.random() * 16.0);
		} while ($.inArray(point, crossover_points) !== -1);
		crossover_points.push(point);
	});
	_.each(crossover_points, function(p) {
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
		blob:$('#crossover_playfield').svg('get').circle(temp, (child1-temp)/512, 2, {fill: 'red'})
	};
	temp = child2 % 512;
	child2 = {
		solution:[temp, (child2 - temp) / 512],
		blob:$('#crossover_playfield').svg('get').circle(temp, (child2-temp)/512, 2, {fill: 'red'})
	};
	return [child1, child2];
}

window.root2 = Math.sqrt(2);

window.get_mid = _.memoize(function(top, bottom) {
	return Math.ceil((top - bottom) / window.root2);
});

window.reproduce = function() {
	// Choose a parent probabilistically. We divide our population in 2
	// over and over until we narrow down a single solution.
	var top = window.crossover_population.length;
	var bottom = 0;
	// This function finds the point x such that the probability from
	// bottom to x is equal to the probability from x to top
	var mid;
	var parents = [];
	// Beware while loops! Do as little as possible in them.
	do {
		top = window.crossover_population.length;
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
	// We have 2 parents, perform a crossover
	var children = crossover(window.crossover_population[parents[0]], window.crossover_population[parents[1]]);
	window.new_solutions.push(children[0]);
	window.new_solutions.push(children[1]);
};

window.draw_blobs = function(x) {
	var solution = window.crossover_population[window.crossover_population.length - 1];
	window.crossover_blobs.push(
		$('#crossover_playfield').svg('get').circle(solution['solution'][0], solution['solution'][1], 5, {fill: 'lime'})
	);
};
		
crossover_step = function() {
	// Grab our population first (if we keep reading the input, we
	// may get inconsistent values)
	var pop = parseInt($('#crossover_population').val());
	
	// We are swapping 1/4 of the population each time
	var to_swap = Math.ceil(pop / 4.0);
	
	window.new_solutions = [];
	
	// Bulk out the population if the slider has been increased
	if (pop > window.crossover_population.length) {
		window.crossover_population.reverse();
		window.crossover_population.push.apply(window.crossover_population, _.map(_.range(pop - window.crossover_population.length), function(x) {
			return {
				solution:[Math.round(Math.random()*$('#crossover_playfield').width()),Math.round(Math.random()*$('#crossover_playfield').height())],
				blob:false
			};
		}));
		window.crossover_population.reverse();
	}
	
	_.times(Math.ceil(to_swap / 2), window.reproduce);
	
	// Sort the population by fitness
	if ($('#crossover_stable').is(':checked')) {
		window.crossover_population = merge_sort(window.crossover_population, comp);
	}
	else {
		window.crossover_population.sort(comp);
	}
	
	$('#crossover_fitness').text(fitn(window.crossover_population[window.crossover_population.length - 1]['solution'])+"");
	
	// At this point we have a sorted array, so draw our blobs
	while (window.crossover_blobs.length > 0) {
		$('#crossover_playfield').svg('get').remove(window.crossover_blobs.shift());
	}

	window.draw_blobs(0);

	// Throw away any excess solutions and the least-fit solutions
	var s;
	while (window.crossover_population.length > pop) {
		window.mid_points = [];
		s = window.crossover_population.shift();
		if (s['blob']) {
			$('#crossover_playfield').svg('get').remove(s['blob']);
		}
	}
	
	// Add the new solutions to the mix
	window.crossover_population.reverse();
	window.crossover_population.push.apply(window.crossover_population, window.new_solutions);
	window.crossover_population.reverse();
	
	window.setTimeout(function(){ crossover_step(); }, 10);
}