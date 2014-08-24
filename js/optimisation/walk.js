window.walk_searchers = [];
window.walk_lines = [];
window.gradient = [];

$(function() {
	$('#walk_playfield').svg({onLoad: walk_init});
	$('#walk_number').change(function(){
		var new_n = Math.round($('#walk_number').val());
		$('#walk_number_display').text(new_n+"");
		while (new_n > window.walk_searchers.length) {
			window.walk_searchers.push(1);
			window.setTimeout(function(){
				walk_step($('#walk_playfield').width() / 2, $('#walk_playfield').height() / 2, window.walk_searchers.length - 1);
			}, 10);
		}
		while (new_n < window.walk_searchers.length) {
			window.walk_searchers.pop();
		}
	});
	$('#walk_number').change();

	$('#walk_fitness').change(function(){
		$('#walk_fitness_display').text(Math.round($('#walk_fitness').val()));
	});
	$('#walk_fitness').change();

	$('#walk_step').change(function(){
		$('#walk_step_display').text(Math.round($('#walk_step').val()));
	});
	$('#walk_step').change();
});

walk_init = function(svg) {
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
			var w = $('#walk_playfield').width();
			var h = $('#walk_playfield').height();
			if (draw) {
				$('#walk_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#walk_playfield').svg('get').rect(0,0,$('#walk_playfield').width(),$('#walk_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}

fitn = _.memoize(function(x, y) {
	return _.reduce(_.map(window.gradient, function(a){ return a(x, y, false); }), function(a,b) { return a + b; });
});

walk_step = function(x, y, n) {
	if (n + 1 > window.walk_searchers.length) {
		while (window.walk_lines[n].length > 0) {
			$('#walk_playfield').svg('get').remove(window.walk_lines[n].shift());
		}
		return;
	}
	var length = $('#walk_step').val();
	var angle = Math.random() * 2 * Math.PI;
	var new_x = x + (length * Math.cos(angle));
	var new_y = y + (length * Math.sin(angle));
	var w = $('#walk_playfield').width();
	var h = $('#walk_playfield').height();
	if ((new_x < 0) || (new_x >= w) || (new_y < 0) || (new_y >= h)) {
		if (new_x < 0) { new_x = new_x + w - 1; }
		if (new_x >= w) { new_x = new_x - w + 1; }
		if (new_y < 0) { new_y = new_y + h - 1; }
		if (new_y >= h) { new_y = new_y - h + 1; }
	}
	else {
		while (window.walk_lines.length < n + 1) { window.walk_lines.push([]); }
		var cols = ['white', 'yellow', 'lime', 'green', 'cyan', 'blue', 'purple'];
		window.walk_lines[n].push($('#walk_playfield').svg('get').line(x, y, new_x, new_y, {stroke: cols[n % cols.length], 'stroke-width': 1}));

		// Find the fitness of this point
		var fitness = fitn(x, y);
		if (fitness >= $('#walk_fitness').val()) {
			new_x = x;
			new_y = y;
		}

		// Clean up old lines
		while (window.walk_lines[n].length > 10) {
			$('#walk_playfield').svg('get').remove(window.walk_lines[n].shift());
		}
	}
	window.setTimeout(function(){ walk_step(new_x, new_y, n); }, 10);
}
