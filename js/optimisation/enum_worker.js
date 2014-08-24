onmessage = (function(){
	var denominator = 2;
	var x = 1;
	var y = -1;

	return function(event) {
		y += 2;
		if (y > denominator) {
			y = 1;
			x += 2;
		}
		if (x > denominator) {
			x = 1;
			y = 1;
			denominator *= 2;
		}
		postMessage([(500 * x) / denominator, (500 * y) / denominator]);
	};
})();