<html>
  <head>
    <script type="text/javascript">
      (function() {
        var bigrams = function(s) {
          return (s.length < 2)? []
                               : [s.substr(0, 2)].concat(bigrams(s.substr(1)));
        };
        var jaccard = function(x, y) {
          var bx = bigrams(x);
          var by = bigrams(y);
          return intersect(bx, by).length / union(bx, by).length;
        };
        var intersect = function(x, y) {
          return x.filter(function(elem) {
            return y.indexOf(elem) != -1;
          });
        };
        var unique = function(x) {
          return x.filter(function(elem, i) { return x.indexOf(elem) === i; });
        };
        var union = function(x, y) {
          return unique(x.concat(y));
        };
        var posts = [
          $for(posts)$ "$url$",
          $endfor$
          "index.html"
        ];
        var path = (window.location.search.indexOf('id=') === -1)
          ? 'index.html'
          : window.location
                  .search
                  .substr(1)
                  .split('id=')[1]
                  .split('&')[0]
                  .replace(/[^a-z]/g, '');
        var scores = posts.map(function(name) {
          return jaccard(name.substr(5).replace(/[^a-z]/g, ''), path);
        });
        var best = scores.reduce(function(x, y) { return Math.max(x, y); }, 0);
        console.log(posts[scores.indexOf(best)]);
        window.location.href = posts[scores.indexOf(best)];
      })();
    </script>
    <meta http-equiv="refresh" content="1;url=/blog.html">
  </head>
  <body>
    This blog has moved. If you're not redirected, check the
    <a href="/blog.html">archive</a>.
  </body>
</html>
