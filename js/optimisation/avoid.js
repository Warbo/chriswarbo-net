{$,Parser hint: pure}
window.avoid_searchers = [];
window.avoid_lines = [];
window.gradient = [];

$(function() {
	$('#avoid_playfield').svg({onLoad: avoid_init});
	$('#avoid_number').change(function(){
		var new_n = Math.round($('#avoid_number').val());
		$('#avoid_number_display').text(new_n+"");
		while (new_n > window.avoid_searchers.length) {
			window.avoid_searchers.push([]);
			window.setTimeout(function(){
				avoid_step($('#avoid_playfield').width() / 2, $('#avoid_playfield').height() / 2, window.avoid_searchers.length - 1);
			}, 10);
		}
		while (new_n < window.avoid_searchers.length) {
			window.avoid_searchers.pop();
		}
	});
	$('#avoid_number').change();
	
	$('#avoid_fitness').change(function(){
		$('#avoid_fitness_display').text(Math.round($('#avoid_fitness').val()));
	});
	$('#avoid_fitness').change();
	
	$('#avoid_step').change(function(){
		$('#avoid_step_display').text(Math.round($('#avoid_step').val()));
	});
	$('#avoid_step').change();
});
	
avoid_init = function(svg) {
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
			var w = $('#avoid_playfield').width();
			var h = $('#avoid_playfield').height();
			if (draw) {
				$('#avoid_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#avoid_playfield').svg('get').rect(0,0,$('#avoid_playfield').width(),$('#avoid_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}
	
avoid_step = function(x, y, n) {
	if (n + 1 > window.avoid_searchers.length) {
		while (window.avoid_lines[n].length > 0) {
			$('#avoid_playfield').svg('get').remove(window.avoid_lines[n].shift());
		}
		return;
	}
	var length = $('#avoid_step').val();
	var angle = Math.random() * 2 * Math.PI;
	var new_x = x + (length * Math.cos(angle));
	var new_y = y + (length * Math.sin(angle));
	
	if ($.inArray([Math.round(new_x), Math.round(new_y)], window.avoid_searchers[n]) != -1) {
		setTimeout(function(){ avoid_step(x, y, n); }, 0);
		return;
	}
	
	var w = $('#avoid_playfield').width();
	var h = $('#avoid_playfield').height();
	if ((new_x < 0) || (new_x >= w) || (new_y < 0) || (new_y >= h)) {
		if (new_x < 0) { new_x = new_x + w - 1; }
		if (new_x >= w) { new_x = new_x - w + 1; }
		if (new_y < 0) { new_y = new_y + h - 1; }
		if (new_y >= h) { new_y = new_y - h + 1; }
	}
	else {
		while (window.avoid_lines.length < n + 1) { window.avoid_lines.push([]); }
		var cols = ['white', 'yellow', 'lime', 'green', 'cyan', 'blue', 'purple'];
		window.avoid_lines[n].push($('#avoid_playfield').svg('get').line(x, y, new_x, new_y, {stroke: cols[n % cols.length], 'stroke-width': 1}));

		// Find the fitness of this point
		var fitness = _.reduce(_.map(window.gradient, function(a){ return a(x, y, false); }), function(a,b) { return a + b; });
		if (fitness >= $('#avoid_fitness').val()) {
			new_x = x;
			new_y = y;
		}
			
		// Clean up old lines
		if (length != 1.0) {
			while (window.avoid_lines[n].length > 10) {
				$('#avoid_playfield').svg('get').remove(window.avoid_lines[n].shift());
			}
		}
	}
	window.avoid_searchers[n].push([Math.round(new_x), Math.round(new_y)]);
	if (window.avoid_searchers[n].length > 10000) {
		$('#avoid_number').val($('#avoid_number').val() - 1);
	}
	window.setTimeout(function(){ avoid_step(new_x, new_y, n); }, 10);
}

