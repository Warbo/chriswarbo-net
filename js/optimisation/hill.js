{$,Parser hint: pure}
window.hill_searchers = [];
window.hill_lines = [];
window.gradient = [];

$(function() {
	$('#hill_playfield').svg({onLoad: hill_init});
	$('#hill_number').change(function(){
	// Set the fitness to -1 when we remove a searcher
		var new_n = Math.round($('#hill_number').val());
		$('#hill_number_display').text(new_n+"");
		while (new_n > window.hill_searchers.length) {
			window.hill_searchers.push(1);
			window.setTimeout(function(){
				hill_step(
					Math.round(Math.random() * $('#hill_playfield').width()),
					Math.round(Math.random() * $('#hill_playfield').height()),
					window.hill_searchers.length - 1
				);
			}, 10);
		}
		while (new_n < window.hill_searchers.length) {
			window.hill_searchers.pop();
		}
	});
	$('#hill_number').change();
	
	$('#hill_fitness_display').text(0);
	
	$('#hill_step').change(function(){
		$('#hill_step_display').text(Math.round($('#hill_step').val()));
	});
	$('#hill_step').change();
});
	
hill_init = function(svg) {
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
			var w = $('#hill_playfield').width();
			var h = $('#hill_playfield').height();
			if (draw) {
				$('#hill_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#hill_playfield').svg('get').rect(0,0,$('#hill_playfield').width(),$('#hill_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}

fitn = function(x, y) {
  return _.reduce(_.map(window.gradient, function(a){ return a(x, y, false); }), function(a,b) { return a + b; });
};

hill_step = function(x, y, n) {
	if (n + 1 > window.hill_searchers.length) {
		while (window.hill_lines[n].length > 0) {
			$('#hill_playfield').svg('get').remove(window.hill_lines[n].shift());
		}
		return;
	}
	var length = $('#hill_step').val();
	var angle = Math.random() * 2 * Math.PI;
	var new_x = x + (length * Math.cos(angle));
	var new_y = y + (length * Math.sin(angle));
	var w = $('#hill_playfield').width();
	var h = $('#hill_playfield').height();
	if ((new_x < 0) || (new_x >= w) || (new_y < 0) || (new_y >= h)) {
		if (new_x < 0) { new_x = new_x + w - 1; }
		if (new_x >= w) { new_x = new_x - w + 1; }
		if (new_y < 0) { new_y = new_y + h - 1; }
		if (new_y >= h) { new_y = new_y - h + 1; }
	}
	else {
		var old_fit = fitn(x, y);
		var new_fit = fitn(new_x, new_y);
		
		while (window.hill_lines.length < n + 1) { window.hill_lines.push([]); }
		var cols = ['white', 'yellow', 'lime', 'green', 'cyan', 'blue', 'purple'];
		window.hill_lines[n].push($('#hill_playfield').svg('get').line(x, y, new_x, new_y, {stroke: cols[n % cols.length], 'stroke-width': 1}));

		if (new_fit < old_fit) {
			new_x = x;
			new_y = y;
			new_fit = old_fit;
		}
		
		// Find the fitness of this point
		// Only update if it's more than the current value
		$('#hill_fitness_display').text(new_fit+"");
			
		// Clean up old lines
		while (window.hill_lines[n].length > 10) {
			$('#hill_playfield').svg('get').remove(window.hill_lines[n].shift());
		}
	}
	window.setTimeout(function(){ hill_step(new_x, new_y, n); }, 10);
}
