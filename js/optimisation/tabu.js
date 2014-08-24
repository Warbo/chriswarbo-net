window.tabu_searchers = [];
window.tabu_lines = [];
window.gradient = [];

$(function() {
	$('#tabu_playfield').svg({onLoad: tabu_init});
	$('#tabu_number').change(function(){
		var new_n = Math.round($('#tabu_number').val());
		$('#tabu_number_display').text(new_n+"");
		while (new_n > window.tabu_searchers.length) {
			window.tabu_searchers.push([]);
			window.setTimeout(function(){
				tabu_step($('#tabu_playfield').width() / 2, $('#tabu_playfield').height() / 2, window.tabu_searchers.length - 1);
			}, 10);
		}
		while (new_n < window.tabu_searchers.length) {
			window.tabu_searchers.pop();
		}
	});
	$('#tabu_number').change();

	$('#tabu_fitness').change(function(){
		$('#tabu_fitness_display').text(Math.round($('#tabu_fitness').val()));
	});
	$('#tabu_fitness').change();

	$('#tabu_step').change(function(){
		$('#tabu_step_display').text(Math.round($('#tabu_step').val()));
	});
	$('#tabu_step').change();
});

tabu_init = function(svg) {
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
			var w = $('#tabu_playfield').width();
			var h = $('#tabu_playfield').height();
			if (draw) {
				$('#tabu_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#tabu_playfield').svg('get').rect(0,0,$('#tabu_playfield').width(),$('#tabu_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}

tabu_step = function(x, y, n) {
	if (n + 1 > window.tabu_searchers.length) {
		while (window.tabu_lines[n].length > 0) {
			$('#tabu_playfield').svg('get').remove(window.tabu_lines[n].shift());
		}
		return;
	}
	var length = $('#tabu_step').val();
	var angle = Math.random();
	if (length == 1.0) {
		angle = angle * 4.0;
		angle = Math.round(angle);
		angle = angle / 4.0;
	}
	angle = angle * 2 * Math.PI;
	var new_x = x + (length * Math.cos(angle));
	var new_y = y + (length * Math.sin(angle));

	if ($.inArray([Math.round(new_x), Math.round(new_y)], window.tabu_searchers[n]) != -1) {
		setTimeout(function(){ tabu_step(x, y, n); }, 100);
		return;
	}

	var w = $('#tabu_playfield').width();
	var h = $('#tabu_playfield').height();
	if ((new_x < 0) || (new_x >= w) || (new_y < 0) || (new_y >= h)) {
		if (new_x < 0) { new_x = new_x + w - 1; }
		if (new_x >= w) { new_x = new_x - w + 1; }
		if (new_y < 0) { new_y = new_y + h - 1; }
		if (new_y >= h) { new_y = new_y - h + 1; }
	}
	else {
		while (window.tabu_lines.length < n + 1) { window.tabu_lines.push([]); }
		var cols = ['white', 'yellow', 'lime', 'green', 'cyan', 'blue', 'purple'];
		window.tabu_lines[n].push($('#tabu_playfield').svg('get').line(x, y, new_x, new_y, {stroke: cols[n % cols.length], 'stroke-width': 1}));

		// Find the fitness of this point
		var fitness = _.reduce(_.map(window.gradient, function(a){ return a(x, y, false); }), function(a,b) { return a + b; });
		if (fitness >= $('#tabu_fitness').val()) {
			new_x = x;
			new_y = y;
		}

		// Clean up old lines
		if (length == 1.0) {
			while (window.tabu_lines[n].length > window.tabu_searchers[n].length) {
				$('#tabu_playfield').svg('get').remove(window.tabu_lines[n].shift());
			}
		}
		else {
			while (window.tabu_lines[n].length > 10) {
				$('#tabu_playfield').svg('get').remove(window.tabu_lines[n].shift());
			}
		}
	}
	window.tabu_searchers[n].push([Math.round(new_x), Math.round(new_y)]);
	while (window.tabu_searchers[n].length > 500) {
		window.tabu_searchers[n].shift();
	}
	window.setTimeout(function(){ tabu_step(new_x, new_y, n); }, 10);
}
