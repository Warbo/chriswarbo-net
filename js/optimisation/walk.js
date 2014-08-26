(function() {
  var w          = $('#walk_playfield').width();
  var h          = $('#walk_playfield').height();
  var walk_lines = [];
  var gradient   = [];
  var best_fit   = 0;
  var best_x     = 0;
  var best_y     = 0;
  var best;
  var length;

  // Increase the fitness of some randomly chosen, overlapping areas
  function walk_init(svg) {
    _.times(Math.round(Math.random() * 100 + 1),
                       function make_rect() {
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
                           t = b;
                           b = temp;
                         }
                         gradient.push(
                           function(x, y, draw){
                             if (draw) {
                               $('#walk_playfield').svg('get')
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
    $('#walk_playfield').svg('get')
                        .rect(0,
                              0,
                              $('#walk_playfield').width(),
                              $('#walk_playfield').height(),
                              {'fill': 'black'});
      _.each(gradient, function(a){a(0, 0, true);});
  };

  var fitn = _.memoize(function(x, y) {
                         return _.reduce(_.map(gradient,
                                               function(a) {
                                                 return a(x, y, false);
                                               }),
                                         function(a,b) { return a + b; });
                       });

  function walk_step(x, y) {
    var angle  = Math.random() * 2 * Math.PI;
    var new_x  = x + (length * Math.cos(angle));
    var new_y  = y + (length * Math.sin(angle));
    if ((new_x < 0) || (new_x >= w) ||
        (new_y < 0) || (new_y >= h)) {
      // Wrap edges, but don't draw lines
      if (new_x <  0) { new_x = new_x + w - 1; }
      if (new_x >= w) { new_x = new_x - w + 1; }
      if (new_y <  0) { new_y = new_y + h - 1; }
      if (new_y >= h) { new_y = new_y - h + 1; }
    }
    else {
      // No need to wrap; draw the line
      walk_lines.push(
        $('#walk_playfield').svg('get')
                            .line(    x,     y,
                                  new_x, new_y,
                                  {'stroke-width': 1,
                                   'stroke': 'white'}));

      // Find the fitness of this point
      var fitness = fitn(new_x, new_y);
      if (fitness >= best_fit) {
        best_x   = new_x;
        best_y   = new_y;
        best_fit = fitness;
        if (best) {
          $('#walk_playfield').svg('get').remove(best);
        }
        best = $('#walk_playfield').svg('get')
                                   .circle(best_x, best_y, 5, {fill: 'lime'});
      }

      // Clean up old lines
      while (walk_lines.length > 10) {
        $('#walk_playfield').svg('get')
                            .remove(walk_lines.shift());
      }
    }
    next_step(new_x, new_y);
  }

  function next_step(x, y, n) {
    setTimeout(function() {
                 walk_step(x, y);
               }, 10);
  }

  // Initialise the demo on document.ready
  $(function() {
      $('#walk_playfield').svg({onLoad: walk_init});
      $('#walk_playfield').click(function() {
                                   length = $('#walk_step').val();
                                   next_step(w / 2, h / 2);
                                 });

      $('#walk_step').change(function(){
                               length = $('#walk_step').val();
                               $('#walk_step_display').text(Math.round(length));
                             });
      $('#walk_step').change();
    });
})();
