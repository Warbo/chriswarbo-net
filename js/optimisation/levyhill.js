(function() {
  var levyhill_searchers = [];
  var levyhill_lines     = [];
  var gradient           = [];
  var fitness            = 0;
  var w;
  var h;

  function pareto() {
    return 1.0 / (1.0 - Math.random());
  }

  function fitn(x, y) {
    return _.reduce(_.map(gradient,
                          function(a){ return a(x, y, false); }),
                    function(a,b) { return a + b; });
  }

  $(function() {
      w = $('#levyhill_playfield').width();
      h = $('#levyhill_playfield').height();
      $('#levyhill_playfield').svg({onLoad: levyhill_init});
      $('#levyhill_playfield').click(
        function() {
          setTimeout(function() {
                       levyhill_step(Math.round(Math.random()*w),
                                     Math.round(Math.random()*h));
                     }, 10);
        });
      show_fit();
    });

  function levyhill_init(svg) {
    _.times(Math.round(Math.random() * 100 + 1),
            function() {
              var l = Math.random();
              var r = Math.random();
              var t = Math.random();
              var b = Math.random();
              var temp;
              if (l > r) {
                temp = l;
                l    = r;
                r    = temp;
              }
              if (t > b) {
                temp = t;
                t    = b;
                b    = temp;
              }
              gradient.push(function(x, y, draw){
                              if (draw) {
                                $('#levyhill_playfield').svg('get')
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
    $('#levyhill_playfield').svg('get')
                            .rect(0, 0, w, h, {'fill': 'black'});
    _.each(gradient, function(a){a(0, 0, true);});
  }

  function levyhill_step(x, y) {
    var length = pareto();
    var angle = Math.random() * 2 * Math.PI;
    var new_x = x + (length * Math.cos(angle));
    var new_y = y + (length * Math.sin(angle));
    var wrapped = false;
    while (new_x < 0) { new_x += w; wraped = true; }
    while (new_x > w) { new_x -= w; wraped = true; }
    while (new_y < 0) { new_y += h; wraped = true; }
    while (new_y > h) { new_y -= h; wraped = true; }

    var new_fit = fitn(new_x, new_y);
    if (!wrapped) {
      levyhill_lines.push($('#levyhill_playfield').svg('get')
                                                  .line(x, y, new_x, new_y,
                                                        {'stroke': 'white',
                                                         'stroke-width': 1}));
    }

    // Clean up old lines
    while (levyhill_lines.length > 10) {
      $('#levyhill_playfield').svg('get').remove(levyhill_lines.shift());
    }

    if (new_fit < fitness) {
      new_x = x;
      new_y = y;
    }
    else {
      fitness = new_fit
      show_fit();
    }

    setTimeout(function(){ levyhill_step(new_x, new_y); }, 10);
  }

  function show_fit() {
    $('#levyhill_fitness_display').text(Math.round(fitness));
  }
}());
