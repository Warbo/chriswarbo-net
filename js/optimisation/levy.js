{$,Parser hint: pure}
window.levy_searchers = [];
window.levy_lines = [];
window.gradient = [];

pareto = function() {
	return Math.pow(1.0 - Math.random(), -1.0 / $('#levy_scale').val());
}

$(function() {
	$('#levy_playfield').svg({onLoad: levy_init});
	$('#levy_number').change(function(){
		var new_n = Math.round($('#levy_number').val());
		$('#levy_number_display').text(new_n+"");
		while (new_n > window.levy_searchers.length) {
			window.levy_searchers.push(1);
			window.setTimeout(function(){
				levy_step($('#levy_playfield').width() / 2, $('#levy_playfield').height() / 2, window.levy_searchers.length - 1);
			}, 10);
		}
		while (new_n < window.levy_searchers.length) {
			window.levy_searchers.pop();
		}
	});
	$('#levy_number').change();
	
	$('#levy_fitness').change(function(){
		$('#levy_fitness_display').text(Math.round($('#levy_fitness').val()));
	});
	$('#levy_fitness').change();
	
	$('#levy_scale').change(function(){
		$('#levy_scale_display').text(Math.round($('#levy_scale').val()));
	});
	$('#levy_scale').change();
});
	
levy_init = function(svg) {
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
			var w = $('#levy_playfield').width();
			var h = $('#levy_playfield').height();
			if (draw) {
				$('#levy_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#levy_playfield').svg('get').rect(0,0,$('#levy_playfield').width(),$('#levy_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}
	
levy_step = function(x, y, n) {
	if (n + 1 > window.levy_searchers.length) {
		while (window.levy_lines[n].length > 0) {
			$('#levy_playfield').svg('get').remove(window.levy_lines[n].shift());
		}
		return;
	}
	var length = pareto();
	var angle = Math.random() * 2 * Math.PI;
	var new_x = x + (length * Math.cos(angle));
	var new_y = y + (length * Math.sin(angle));
	var w = $('#levy_playfield').width();
	var h = $('#levy_playfield').height();
	while ((new_x < 0) || (new_x >= w) || (new_y < 0) || (new_y >= h)) {
		while (length > w / 2.0) {
			length = pareto();
		}
		angle = Math.random() * 2 * Math.PI;
		new_x = x + (length * Math.cos(angle));
		new_y = y + (length * Math.sin(angle));
	}
	
	while (window.levy_lines.length < n + 1) { window.levy_lines.push([]); }
	var cols = ['white', 'yellow', 'lime', 'green', 'cyan', 'blue', 'purple'];
	window.levy_lines[n].push($('#levy_playfield').svg('get').line(x, y, new_x, new_y, {stroke: cols[n % cols.length], 'stroke-width': 1}));

	// Find the fitness of this point
	var fitness = _.reduce(_.map(window.gradient, function(a){ return a(x, y, false); }), function(a,b) { return a + b; });
	if (fitness >= $('#levy_fitness').val()) {
		new_x = x;
		new_y = y;
	}
			
	// Clean up old lines
	while (window.levy_lines[n].length > 10) {
		$('#levy_playfield').svg('get').remove(window.levy_lines[n].shift());
	}
	
	window.setTimeout(function(){ levy_step(new_x, new_y, n); }, 10);
}

