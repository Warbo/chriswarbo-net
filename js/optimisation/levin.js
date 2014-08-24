window.gradient = [];
window.fittest = false;

$(function() {
	$('#levin_playfield').svg({onLoad: levin_init});
	$('#levin_playfield').click(function(){
            (function() {
                var x = 0;
                var y = 0;
                var worker = new Worker('/js/optimisation/levin_bbj.js');
                var dots = [];
                var phase = 0;
                var m = 0;
                var fit = -1;
                var new_fit = -1;
		var blob;
                worker.onmessage = function(event) {
                    data = event.data;

                    x = Math.min(data["x"], 500);
                    y = Math.min(data["y"], 500);

                    // Clean up old dots
                    while (dots.length > 10) {
                        $('#levin_playfield').svg('get').remove(dots.shift());
                    }
                    _.each(dots, function(d) {
                        $('#levin_playfield').svg('get').change(d, {'fill-opacity': d.fillOpacity * 0.9});
                    });

                    // Update display
                    if (phase < data.phase) {
                        phase = data.phase;
                        $('#levin_phase').text(phase+'');
                    }
                    if (m != data.m) {
                        m = data.m;
                        $('#levin_m').text(m+'');
                    }

                    new_fit = fitn(x, y);
                    if (new_fit > fit) {
			fit = new_fit;
                        $('#levin_fitness_display').text(fit+'');
                        $('#levin_winner').text('Program: '+data.p.toString(2)+' Machine: '+data.m+' Complexity: '+data.phase);
			if (blob) {
			    $('#levin_playfield').svg('get').remove(blob);
			}
                        blob = $('#levin_playfield').svg('get').circle(x, y, 5, {fill: 'lime'});
		    }
                    else {
                        dots.push($('#levin_playfield').svg('get').circle(x, y, 2, {fill: 'red', 'fill-opacity': 0.9}));
                    }
                    worker.postMessage('');
                };

                worker.postMessage('');
            })();

            $('#levin_playfield').unbind('click');
	});
	$('#levin_fitness_display').text('-1');
});

levin_init = function(svg) {
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
			var w = $('#levin_playfield').width();
			var h = $('#levin_playfield').height();
			if (draw) {
				$('#levin_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
			}
			else {
				if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
					return 1;
				}
				return 0;
			}
		});
	});
	$('#levin_playfield').svg('get').rect(0,0,$('#levin_playfield').width(),$('#levin_playfield').height(),{'fill': 'black'});
	_.each(window.gradient, function(a){a(0, 0, true);});
}

window.fitn = _.memoize(function(x, y) {
    return _.reduce(_.map(window.gradient, function(a) { return a(x,y); }), function(a, b) { return a+b; }, 0);
});
