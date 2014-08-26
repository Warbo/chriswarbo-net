(function() {
  var tabu_searchers = [];
  var tabu_lines     = [];
  var gradient       = [];
  var best_fit       = 0;
  var best;
  var length;
  var w;
  var h;

  $(function() {
      w = $('#tabu_playfield').width();
      h = $('#tabu_playfield').height();
      $('#tabu_playfield').svg({onLoad: tabu_init});
      $('#tabu_playfield').click(
        function() {
          setTimeout(function() {
                       tabu_step(w / 2, h / 2);
                     }, 10);
        });

      $('#tabu_step').change(function(){
                               length = $('#tabu_step').val();
                               $('#tabu_step_display').text(Math.round(length));
                             });
      $('#tabu_step').change();
  });

  function tabu_init(svg) {
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
                                $('#tabu_playfield').svg('get')
                                                    .rect(    l*w,     t*h,
                                                          (r-l)*w, (b-t)*h,
                                                          {'fill': 'white',
                                                           'fill-opacity': 0.02});
                              }
                              else {
                                if ((l < x / w) && (r > x / w) && (t < y / h) && (b > y / h)) {
                                  return 1;
                                }
                                return 0;
                              }
                            });
            });
    $('#tabu_playfield').svg('get')
                        .rect(0, 0, w, h, {'fill': 'black'});
    _.each(gradient, function(a){a(0, 0, true);});
  }

  function tabu_step(x, y) {
    var angle = Math.random();
    if (length == 1.0) {
      angle = angle * 4.0;
      angle = Math.round(angle);
      angle = angle / 4.0;
    }
    angle = angle * 2 * Math.PI;
    var new_x = x + (length * Math.cos(angle));
    var new_y = y + (length * Math.sin(angle));

    if ($.inArray([Math.round(new_x), Math.round(new_y)],
                  tabu_searchers) != -1) {
      setTimeout(function(){ tabu_step(x, y, n); }, 100);
      return;
    }

    if ((new_x < 0) || (new_x >= w) ||
        (new_y < 0) || (new_y >= h)) {
      if (new_x <  0) { new_x = new_x + w - 1; }
      if (new_x >= w) { new_x = new_x - w + 1; }
      if (new_y <  0) { new_y = new_y + h - 1; }
      if (new_y >= h) { new_y = new_y - h + 1; }
    }
    else {
      tabu_lines.push($('#tabu_playfield').svg('get')
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
        $('#tabu_playfield').svg('get').remove(best);
      }
      best = $('#tabu_playfield').svg('get')
                                 .circle(new_x, new_y, 5, {fill: 'lime'});
    }

    // Clean up old lines
    if (length == 1.0) {
      while (tabu_lines.length > tabu_searchers.length) {
        $('#tabu_playfield').svg('get').remove(tabu_lines.shift());
      }
    }
    else {
      while (tabu_lines.length > 10) {
        $('#tabu_playfield').svg('get').remove(tabu_lines.shift());
      }
    }

    tabu_searchers.push([Math.round(new_x), Math.round(new_y)]);
    while (tabu_searchers.length > 500) {
      tabu_searchers.shift();
    }
    setTimeout(function(){ tabu_step(new_x, new_y); }, 10);
  }

  function fitn(x, y) {
    return _.reduce(_.map(gradient,
                          function(a){ return a(x, y, false); }),
                    function(a,b) { return a + b; });
  }
}());
