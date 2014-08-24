{$,Parser hint: pure}
window.gradient = [];

$(function() {
	$('#enum_playfield').svg({onLoad: enum_init});
	$('#enum_playfield').click(function(){
		var self = jQuery(this);
		var fittest = -1;
		var green = false;
		var blobs = [];
		
		var worker = new Worker('/data_custom/enum.js');
		worker.onmessage = function(event) {
			var x = event.data[0];
			var y = event.data[1];

			while (blobs.length > 256) {
				self.svg('get').remove(blobs.shift());
			}
			blobs.push(self.svg('get').circle(x, y, 2, {fill: 'red'}));

			var this_fit = _.reduce(
				_.map(window.gradient, function(a){
					return a(x, y, false);
				}),
				function(a,b) {
					return a + b;
				}
			);
			if (this_fit >= fittest) {
				fittest = this_fit;
				if (green) {
					self.svg('get').remove(green);
				}
				green = self.svg('get').circle(x, y, 5, {fill: 'lime'});
				$('#enum_fitness_display').text(fittest+'');
			}
			worker.postMessage('');
		};
		worker.postMessage('');
	});
});

enum_init = function(svg) {
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
			var w = $('#enum_playfield').width();
			var h = $('#enum_playfield').height();
			if (draw) {
				$('#enum_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#enum_playfield').svg('get').rect(0,0,$('#enum_playfield').width(),$('#enum_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}
