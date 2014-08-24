{$,Parser hint: pure}

$(function() {
	var gradient = [];
	var fittest = false;
	var fitness = [];
	var harmony_init = function(svg) {
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
			fitness.push([500*l, 500*t, 500*r, 500*b]);
			gradient.push(function(x, y, draw){
				var w = $('#harmony_playfield').width();
				var h = $('#harmony_playfield').height();
				if (draw) {
					$('#harmony_playfield').svg('get').rect(l*w, t*h, (r-l)*w, (b-t)*h, {'fill': 'white', 'fill-opacity': 0.02});
				}
				else {
					if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
						return 1;
					}
					return 0;
				}
			});
		});
		$('#harmony_playfield').svg('get').rect(0,0,$('#harmony_playfield').width(),$('#harmony_playfield').height(),{'fill': 'black'});
		_.each(gradient, function(a){a(0, 0, true);});
	};

	$('#harmony_playfield').svg({onLoad: harmony_init});
	$('#harmony_playfield').click(function(){
            (function() {
                var x = 0;
                var y = 0;
                var worker = new Worker('data_custom/harmony.js');
                var dots = [];
                var fit = -1;
                var new_fit = -1;
		var blob;
                worker.onmessage = function(event) {
                    data = event.data;

		    if (data == '') {
		    	// Our worker sends us an empty string when its latest solution
		    	// was too weak to be acceptable. Ask the worker for another one.
			worker.postMessage('');
			return;
		    }

		    // We're given an array of solutions
		    _.each(data, function(data) {
                        x = Math.min(data["x"], 500);
                        y = Math.min(data["y"], 500);
	                new_fit = data["fitness"];

                        // Clean up old dots
                        //while (dots.length > 100) {
                        //    $('#harmony_playfield').svg('get').remove(dots.shift());
                        //}

                        // Update display
                        if (dots[data['removed']]) {
                        	$('#harmony_playfield').svg('get').remove(dots[data['removed']]);
                        }
                        dots[data['removed']] = $('#harmony_playfield').svg('get').circle(x, y, 2, {fill: 'red'});
                        
                        if (new_fit > fit) {
			    fit = new_fit;
                            $('#harmony_fitness_display').text(fit+'');
	  		    if (blob) {
			        $('#harmony_playfield').svg('get').remove(blob);
 			    }
                            blob = $('#harmony_playfield').svg('get').circle(x, y, 5, {fill: 'lime'});
		        }
                    });
                    worker.postMessage('');
                };
                
                worker.postMessage({
		    hms: parseInt($('#hms').val()),
		    hmcr: $('#hmcr').val()/10.0,
		    par: $('#par').val()/10.0,
		    fw: $('#fw').val()/10.0,
		    fitness: fitness
		});
            })();

            $('#harmony_playfield').unbind('click');
	});
	$('#harmony_fitness_display').text('-1');
});