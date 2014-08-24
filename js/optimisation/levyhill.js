{$,Parser hint: pure}
window.levyhill_searchers = [];
window.levyhill_lines = [];
window.gradient = [];

pareto = function() {
	return Math.pow(1.0 - Math.random(), -1.0 / $('#levyhill_scale').val());
}

fitn = function(x, y) {
	return _.reduce(_.map(window.gradient, function(a){ return a(x, y, false); }), function(a,b) { return a + b; });
}

$(function() {
	$('#levyhill_playfield').svg({onLoad: levyhill_init});
	$('#levyhill_number').change(function(){
		var new_n = Math.round($('#levyhill_number').val());
		$('#levyhill_number_display').text(new_n+"");
		while (new_n > window.levyhill_searchers.length) {
			window.levyhill_searchers.push(1);
			window.setTimeout(function(){
				levyhill_step(
					Math.round(Math.random()*$('#levyhill_playfield').width()),
					Math.round(Math.random()*$('#levyhill_playfield').height()),
					window.levyhill_searchers.length - 1
				);
			}, 10);
		}
		while (new_n < window.levyhill_searchers.length) {
			window.levyhill_searchers.pop();
		}
	});
	$('#levyhill_number').change();
	
	$('#levyhill_fitness').change(function(){
		$('#levyhill_fitness_display').text(Math.round($('#levyhill_fitness').val()));
	});
	$('#levyhill_fitness').change();
	
	$('#levyhill_scale').change(function(){
		$('#levyhill_scale_display').text(Math.round($('#levyhill_scale').val()));
	});
	$('#levyhill_scale').change();
});
	
levyhill_init = function(svg) {
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
			var w = $('#levyhill_playfield').width();
			var h = $('#levyhill_playfield').height();
			if (draw) {
				$('#levyhill_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#levyhill_playfield').svg('get').rect(0,0,$('#levyhill_playfield').width(),$('#levyhill_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}
	
levyhill_step = function(x, y, n) {
	if (n + 1 > window.levyhill_searchers.length) {
		while (window.levyhill_lines[n].length > 0) {
			$('#levyhill_playfield').svg('get').remove(window.levyhill_lines[n].shift());
		}
		return;
	}
	var length = pareto();
	var angle = Math.random() * 2 * Math.PI;
	var new_x = x + (length * Math.cos(angle));
	var new_y = y + (length * Math.sin(angle));
	var w = $('#levyhill_playfield').width();
	var h = $('#levyhill_playfield').height();
	while ((new_x < 0) || (new_x >= w) || (new_y < 0) || (new_y >= h)) {
		while (length > w / 2.0) {
			length = pareto();
		}
		angle = Math.random() * 2 * Math.PI;
		new_x = x + (length * Math.cos(angle));
		new_y = y + (length * Math.sin(angle));
	}
	
	var new_fit = fitn(new_x, new_y);
	var old_fit = fitn(x, y);
	
	while (window.levyhill_lines.length < n + 1) { window.levyhill_lines.push([]); }
	var cols = ['white', 'yellow', 'lime', 'green', 'cyan', 'blue', 'purple'];
	window.levyhill_lines[n].push($('#levyhill_playfield').svg('get').line(x, y, new_x, new_y, {stroke: cols[n % cols.length], 'stroke-width': 1}));
	
	// Clean up old lines
	while (window.levyhill_lines[n].length > 10) {
		$('#levyhill_playfield').svg('get').remove(window.levyhill_lines[n].shift());
	}
	
	if (new_fit < old_fit) {
		new_x = x;
		new_y = y;
	}
	
	window.setTimeout(function(){ levyhill_step(new_x, new_y, n); }, 10);
}


