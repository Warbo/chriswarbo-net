window.addEventListener('load', function() {
    var canvas    = document.getElementById('canvas');
    var ctx       = canvas.getContext('2d');
    var render = function(c, x, y) {
        console.log(arguments);
        ctx.fillStyle = 'rgb(' + c + ',' + c + ',' + c + ')';
        ctx.fillRect(x, y, 2, 2);
    };
    var runner = 0;
    canvas.addEventListener('click', function() {
        ctx.fillStyle = 'rgb(128,128,128)';
        ctx.fillRect(0, 0, 512, 512);
        var reward, output;
        var machine = bf(
            256,
            bf_prog(),
            function( ) { return 128; },
            function(x) {
                if (reward === -1) {
                    reward = x;
                    return;
                }
                output.push(x);
            });
        var this_runner = runner;
        var run = function() {
            if (this_runner !== runner) return;
            reward = -1;
            output = [];
            var result  = machine();
            var x, y;
            if (result.success && reward > -1) {
                while (output.length > 1) {
                    x = output.shift();
                    y = output.shift();
                    render(reward, x, y);
                }
            }
            setTimeout(run, 0);
        };
        run();
    });
});
