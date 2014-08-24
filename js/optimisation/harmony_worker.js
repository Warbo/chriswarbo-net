onmessage = (function() {
    var mem, fitness, fitnesses;
    
    // The first message handler expects our search parameters
    return function(event) {
        // Set our parameters
        var hms, hmcr, par, delta, fw;
        hms = event.data['hms'];
        hmcr = event.data['hmcr'];
        par = event.data['par'];
        delta = 1;
        fw = event.data['fw'];
        fitness = (function() {
            var fitnesses = event.data['fitness'];
            return function(x, y) {
                var fitness = 0;
                for (var i=0; i < fitnesses.length; i++) {
                    if (
                        fitnesses[i][0] <= x &&
                        fitnesses[i][1] <= y &&
                        fitnesses[i][2] >= x &&
                        fitnesses[i][3] >= y
                    ) {
                        fitness++;
                    }
                }
                return fitness;
            };
        })();
        fitnesses = [];
        
        // Create our memory, which contains vectors
	mem = [];
	var vec;
        for (var i = 0; i < hms; i++) {
            vec = [];
	    for (var j=0; j < 18; j++) {
		vec.push(Math.round(Math.random()));
	    }
	    mem.push(vec);
	    fitnesses.push([fitness(
	        parseInt(vec.slice(0, 9).join(''), 2),
		parseInt(vec.slice(9).join(''), 2)
	    ), i]);
        }
        
        // Now that we've been initialised, switch to
        // the real search algorithm
        onmessage = function(event) {
            var new_solution = [];
            var scalar;
            for (var i = 0; i < 18; i++) {
                if (Math.random() < hmcr) {
                    // Pick a value from memory
                    scalar = ~~(Math.floor(Math.random() * hms));  // Choose a vector randomly
                    scalar = mem[scalar][i];
                    
                    // Potentially mutate the value
                    if (Math.random() < par) {
                        scalar = scalar + 1 % 2;
                    }
                }
                else {
                    // Generate a new value
                    new_solution.push(Math.round(Math.random()));
                }
            }
	    var x = parseInt(new_solution.slice(0, 9).join(''), 2);
	    var y = parseInt(new_solution.slice(9).join(''), 2);
            var comp = function(f, g) {
		if (f[0] > g) return 1;
		if (f[0] < g) return -1;
		return 0;
	    };
            // Replace our weakest solution with this one, if
            // it's better
            fitnesses.sort(comp);
            var weakest = fitnesses[0];
            var this_fitness = fitness(x, y);
            if (this_fitness >= weakest[0]) {
                mem[weakest[1]] = new_solution;
                fitnesses[0] = [this_fitness, weakest[1]];

		postMessage([{removed: weakest[1], x: x, y: y, fitness: this_fitness }]);
            }
	    else {
		postMessage('');
	    }
        };
        (function() {
            var results = [];
	    var x, y;
            for (var i = 0; i < mem.length; i++) {
		x = parseInt(mem[i].slice(0, 9).join(''), 2);
		y = parseInt(mem[i].slice(9).join(''), 2);
		results.push({
                    x: x,
                    y: y,
                    fitness: fitness(x, y),
                    removed: i
                });
	    }
	    postMessage(results);
        })();
    };
})();
    