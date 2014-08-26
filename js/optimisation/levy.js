(function() {
  var levy_searchers = [];
  var levy_lines     = [];
  var gradient       = [];
  var best_fit       = 0;
  var best;
  var w;
  var h;

  function pareto() {
    return 1.0 / (1.0 - Math.random());
  }

  $(function() {
      w = $('#levy_playfield').width();
      h = $('#levy_playfield').height();
      $('#levy_playfield').svg({onLoad: levy_init});
      $('#levy_playfield').click(function() {
                                   setTimeout(function() {
                                                levy_step(w / 2, h / 2);
                                              }, 10);
                                 });
    });

  function levy_init(svg) {
    _.times(Math.round(Math.random() * 100 + 1),
            function() {
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
              gradient.push(function(x, y, draw) {
                              if (draw) {
                                $('#levy_playfield').svg('get')
                                                    .rect(    l*w,     t*h,
                                                          (r-l)*w, (b-t)*h,
                                                          {'fill': 'white',
                                                           'fill-opacity': 0.02});
                              }
                              else {
                                if ((l < x / w) && (r > x / w) &&
                                    (t < y / h) && (b > y / h)) {
                                  return 1;
                                }
                                return 0;
                              }
                            });
            });
    $('#levy_playfield').svg('get')
                        .rect(0, 0, w, h, {'fill': 'black'});
    _.each(gradient, function(a){a(0, 0, true);});
  }

  function levy_step(x, y) {
    var length = pareto();
    var angle = Math.random() * 2 * Math.PI;
    var new_x = x + (length * Math.cos(angle));
    var new_y = y + (length * Math.sin(angle));

    var wrapped = false;
    while (new_x < 0) { new_x += w; wrapped = true; }
    while (new_x > w) { new_x -= w; wrapped = true; }
    while (new_y < 0) { new_y += h; wrapped = true; }
    while (new_y > h) { new_y -= h; wrapped = true; }

    if (!wrapped) {
      levy_lines.push($('#levy_playfield').svg('get')
                                          .line(    x,     y,
                                                new_x, new_y,
                                                {'stroke': 'white',
                                                 'stroke-width': 1}));
    }

    // Find the fitness of this point
    var fitness = fitn(new_x, new_y);
    if (fitness >= best_fit) {
      best_fit = fitness;
      if (best) {
        $('#levy_playfield').svg('get').remove(best);
      }
      best = $('#levy_playfield').svg('get')
                                 .circle(new_x, new_y, 5, {fill: 'lime'});
    }

    // Clean up old lines
    while (levy_lines.length > 10) {
      $('#levy_playfield').svg('get').remove(levy_lines.shift());
    }

    setTimeout(function(){ levy_step(new_x, new_y); }, 10);
  }

  function fitn(x, y) {
    return _.reduce(_.map(gradient,
                          function(a){ return a(x, y, false); }),
                    function(a,b) { return a + b; });
  }
}());
