(function() {
  var hill_searchers = [];
  var hill_lines     = [];
  var gradient       = [];
  var fitness        = 0;
  var length;
  var w;
  var h;

  $(function() {
      w = $('#hill_playfield').width();
      h = $('#hill_playfield').height();
      $('#hill_playfield').svg({onLoad: hill_init});
      $('#hill_playfield').click(
        function() {
          $('#hill_step').change();
          setTimeout(function() {
                       hill_step(Math.round(Math.random() * w),
                                 Math.round(Math.random() * h));
                     }, 10);
        });

      $('#hill_fitness_display').text(Math.round(fitness));

      $('#hill_step').change(function() {
                               length = $('#hill_step').val();
                               $('#hill_step_display').text(Math.round(length));
                             });
      $('#hill_step').change();
    });

  function hill_init(svg) {
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
                                $('#hill_playfield').svg('get')
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
    $('#hill_playfield').svg('get')
                        .rect(0, 0, w, h, {'fill': 'black'});
    _.each(gradient, function(a){a(0, 0, true);});
  }

  function fitn(x, y) {
    return _.reduce(_.map(gradient,
                          function(a){ return a(x, y, false); }),
                    function(a,b) { return a + b; });
  }

  function hill_step(x, y) {
    var angle = Math.random() * 2 * Math.PI;
    var new_x = x + (length * Math.cos(angle));
    var new_y = y + (length * Math.sin(angle));
    if ((new_x < 0) || (new_x >= w) ||
        (new_y < 0) || (new_y >= h)) {
      if (new_x <  0) { new_x = new_x + w - 1; }
      if (new_x >= w) { new_x = new_x - w + 1; }
      if (new_y <  0) { new_y = new_y + h - 1; }
      if (new_y >= h) { new_y = new_y - h + 1; }
    }
    else {
      var old_fit = fitn(x, y);
      var new_fit = fitn(new_x, new_y);

      hill_lines.push($('#hill_playfield').svg('get')
                                          .line(    x,     y,
                                                new_x, new_y,
                                                {'stroke': 'white',
                                                 'stroke-width': 1}));

      if (new_fit < old_fit) {
        new_x = x;
        new_y = y;
        new_fit = old_fit;
      }

      // Find the fitness of this point
      // Only update if it's more than the current value
      $('#hill_fitness_display').text(new_fit+"");

      // Clean up old lines
      while (hill_lines.length > 10) {
        $('#hill_playfield').svg('get').remove(hill_lines.shift());
      }
    }
    setTimeout(function(){ hill_step(new_x, new_y); }, 10);
  }
}());
