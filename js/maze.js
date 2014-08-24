var building = false;
var lines = [];

create_maze = function(id, difficulty) {
	if (building) {
		window.setTimeout(function() { create_maze(id, difficulty); }, 200);
		return;
	}
	building = true;
	var elem = $('#'+id);
	_.each(lines, function(l) {
		elem.svg('get').remove(l);
	});
	lines = [];
	var width = elem.width();
	var height = elem.height();
	var h = Math.floor(height / difficulty);
	var w = Math.floor(width / difficulty);
	window.maze = [];
	window.scale = difficulty;
	var y;
	var x;
	_.each(_.range(w), function(x){
		window.maze.push([]);		// Columns
		_.each(_.range(h), function(y){
			window.maze[window.maze.length-1].push([]);		// Cells
			if (x == 0) {
				window.maze[window.maze.length-1][window.maze[window.maze.length-1].length-1].push([0,0]);
			} else {
				window.maze[window.maze.length-1][window.maze[window.maze.length-1].length-1].push([-1,0]);
			}
			if (x == w-1) {
				window.maze[window.maze.length-1][window.maze[window.maze.length-1].length-1].push([0,0]);
			} else {
				window.maze[window.maze.length-1][window.maze[window.maze.length-1].length-1].push([1,0]);
			}
			if (y == 0) {
				window.maze[window.maze.length-1][window.maze[window.maze.length-1].length-1].push([0,0]);
			} else {
				window.maze[window.maze.length-1][window.maze[window.maze.length-1].length-1].push([0,-1]);
			}
			if (y == h-1) {
				window.maze[window.maze.length-1][window.maze[window.maze.length-1].length-1].push([0,0]);
			} else {
				window.maze[window.maze.length-1][window.maze[window.maze.length-1].length-1].push([0,1]);
			}
			window.maze[window.maze.length-1][window.maze[window.maze.length-1].length-1].push(true);
		});
	});
	window.backtrackers.push(function(){recursive_backtracker(0, 0);});
	window.setTimeout(run_backtrackers, 50);
};

window.backtrackers = [];

recursive_backtracker = function(x, y, from) {
	window.maze[x][y][4] = false;
	window.backtrackers.push(function() { draw_maze(x, y, window.scale); });
	if (typeof from !== 'undefined') {
		var f;
		if (x-from[0] == 1) {
			f = 0;
		}
		if (x-from[0] == -1) {
			f = 1;
		}
		if (y-from[1] == 1) {
			f = 2;
		}
		if (y-from[1] == -1) {
			f = 3;
		}
		window.maze[x][y][f] = false;
	}
	var choices = shuffle([0,1,2,3]);
	_.each(choices, function(choice) {
		if (window.maze[x][y][choice] !== false) {
			window.backtrackers.push(function() {
				var cell = window.maze[x][y];
				var n = cell[choice];
				window.go = [x, y, choice];
				if (window.maze[x+n[0]][y+n[1]][4] === true) {
					window.maze[x][y][choice] = false;
					window.backtrackers.push(function(){
						recursive_backtracker(x+n[0], y+n[1], [x,y]);
					});
				}
			});
		}
	});
};

run_backtrackers = function(){
	var f;
	var count = 0;
	while (window.backtrackers.length > 0 && count < 100) {
		f = window.backtrackers.pop();
		f();
		count++;
	}
	if (window.backtrackers.length > 0) {
		window.setTimeout(function(){ run_backtrackers(); }, 10);
	}
	else {
		building = false;
	}
};

draw_maze = function(x, y, scale){
	var svg = $('#maze').svg('get');
	_.each(_.range(4), function(n) {
		if (window.maze[x][y][n] === false) { return; }
		var old_x;
		var old_y;
		var new_x;
		var new_y;
		if (n == 0) {
			old_x = x;
			old_y = y;
			new_x = x;
			new_y = y+1;
		} else if (n == 1) {
			old_x = x+1;
			old_y = y;
			new_x = x+1;
			new_y = y+1;
		} else if (n == 2) {
			old_x = x;
			old_y = y;
			new_x = x+1;
			new_y = y;
		} else if (n == 3) {
			old_x = x;
			old_y = y+1;
			new_x = x+1;
			new_y = y+1;
		}
		else {
			return;
		}
		lines.push(svg.line(old_x*scale, old_y*scale, new_x*scale, new_y*scale, {stroke: 'white', 'stroke-width': 1}));
	});
};

shuffle = function(a) {
    var tmp, current, top = a.length;

    if(top) while(--top) {
        current = Math.floor(Math.random() * (top + 1));
        tmp = a[current];
        a[current] = a[top];
        a[top] = tmp;
    }

    return a;
};
