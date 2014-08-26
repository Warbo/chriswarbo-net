(function() {
   var avoid_searchers = [];
   var avoid_lines     = [];
   var gradient        = [];
   var best_x          = 0;
   var best_y          = 0;
   var best_fit        = 0;
   var best;
   var w;
   var h;
   var length;

   $(function() {
       w = $('#avoid_playfield').width();
       h = $('#avoid_playfield').height();
       $('#avoid_playfield').svg({onLoad: avoid_init});
       $('#avoid_playfield').click(function(){
                                     setTimeout(function() {
                                                  avoid_step(w / 2, h / 2);
                                                }, 10);
                                   });

       $('#avoid_step').change(function(){
                                 length = $('#avoid_step').val();
                                 $('#avoid_step_display').text(Math.round(length));
       });
       $('#avoid_step').change();
     });

  function avoid_init(svg) {
    _.times(Math.round(Math.random() * 100 + 1),
            function(){
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
                                $('#avoid_playfield').svg('get')
                                                     .rect(    l*w,     t*h,
                                                           (r-l)*w, (b-t)*h,
                                                           {'fill-opacity': 0.02,
                                                            'fill': 'white'});
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
    $('#avoid_playfield').svg('get').rect(0,0,w,h,{'fill': 'black'});
    _.each(gradient, function(a){a(0, 0, true);});
  }

  function avoid_step(x, y) {
    var angle = Math.random() * 2 * Math.PI;
    var new_x = x + (length * Math.cos(angle));
    var new_y = y + (length * Math.sin(angle));

    // Avoid places we've already been
    if ($.inArray([Math.round(new_x), Math.round(new_y)],
                  avoid_searchers) != -1) {
      setTimeout(function(){ avoid_step(x, y, n); }, 0);
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
      avoid_lines.push($('#avoid_playfield').svg('get')
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
        $('#avoid_playfield').svg('get').remove(best);
      }
      best = $('#avoid_playfield').svg('get')
                                  .circle(new_x, new_y, 5, {fill: 'lime'});
    }
    // Clean up old lines
    if (length != 1.0) {
      while (avoid_lines.length > 10) {
        $('#avoid_playfield').svg('get').remove(avoid_lines.shift());
      }
    }
    avoid_searchers.push([Math.round(new_x), Math.round(new_y)]);
    while (avoid_searchers.length > 10000) {
      avoid_searchers.shift();
    }
    setTimeout(function(){ avoid_step(new_x, new_y); }, 10);
  }

  function fitn(x, y) {
    return _.reduce(_.map(gradient,
                          function(a) {
                            return a(x, y, false);
                          }),
                    function(a,b) { return a + b; });
  }
}());
