window.onload = function() {
    // Report results
    JSC.on_report((function() {
        var results = document.getElementById('results');
        return function(s) {
            results.innerHTML += '<pre>' + s.replace('&', '&amp;')
                                            .replace('<', '&lt;')
                                            .replace('>', '&gt;') + '</pre>';
        };
    }()));

    // Random input from the range 0-4
    var input = function(n) {
        return function() { return Math.floor(Math.random() * n); };
    };

    // Tests

    [['No exceptions',
      function(verdict, p) {
          try {
              var machine = bf(2, p, input(2), function(x) { });
              machine(); machine();
              return verdict(true);
          }
          catch (e) {
              console.log(e);
              return verdict(false);
          }
      }],
     ['Output in range',
      function(verdict, p) {
          var overflow = false;
          var machine = bf(
              2,
              p,
              input(2),
              function(x) {
                  var x_overflow = x < 0 || x > 1;
                  if (x_overflow) console.log('Overflow: ' + x);
                  overflow = overflow || x_overflow;
              });
          machine(); machine();
          return verdict(!overflow);
      }],
    ].forEach(function(pair) {
        JSC.test(pair[0],
                 pair[1],
                 bf_prog,
                 function(p) {
                     return 'log(length) = ' + bf_length(p).toString(2).length;
                 });
    });
};
