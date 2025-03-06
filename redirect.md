---
extra_head:
  <meta http-equiv="refresh" content="1;url=./blog/" />

title: Blog moved
---

This blog has moved. If you're not redirected, check the [archive](./blog/).

<!-- TODO:

/index.php?page=cedi&type=misc&id=1%2F3%2F6%2F10 ->  /projects/arduino
/index.php?page=cedi&type=misc&id=1%2F4%2F28%2F29 -> /projects/optimisation
/index.php?page=cedi&type=misc&id=1%2F4%2F28%2F29%2F46 -> /projects/optimisation/levin.html

We moved git2html pages in 2017, but some things were linking to specific files:
/git/foo/branches/... -> /git/foo/git/branches/...

-->

```{.unwrap pipe="bash | pandoc -f markdown -t json"}
shopt -s nullglob
echo '<script type="text/javascript">var posts = ['
for F in "$blogPages"/*.html
do
    NAME=$(basename "$F")
    echo "\"./blog/$NAME\","
done
echo '"index.html" ];</script>'
```

```{.unwrap pipe="pandoc -f markdown -t json"}
<script type="text/javascript">//<![CDATA[
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
    var blog = function(id) {
      var scores = posts.map(function(name) {
        return jaccard(name.substr(5).replace(/[^a-z]/g, ''), id);
      });
      var best = scores.reduce(function(x, y) { return Math.max(x, y); }, 0);
      return best;
    };

    var haveParam = function(p) {
      return window.location.search.indexOf(p) !== -1;
    };
    path = haveParam('page=cedi')
      ?
      : haveParam('id=')
        ? window.location
                .search
                .substr(1)
                .split('id=')[1]
                .split('&')[0]
                .replace(/[^a-z]/g, '')
        : 'index.html';

    console.log(posts[scores.indexOf(best)]);
    window.location.href = posts[scores.indexOf(best)];
  })();
//]]></script>
```
