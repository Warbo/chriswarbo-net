---
title: Blog
packages: [ 'showPosts' ]
content_classes: h-feed
extra_head: |
  <link rel="alternate" type="application/rss+xml" href="blog.rss"
        title="ChrisWarbo.net RSS feed"/>
  <link rel="alternate" type="application/atom+xml" href="blog.atom"
        title="ChrisWarbo.net Atom feed"/>
  <style type="text/css">
    .rant-toggler {
      margin-bottom : -30px;
      opacity       : 0.3;
      text-align    : right;
    }
  </style>

dependencies: [ 'rants' ]
---

Here you can find all of my previous posts. These are also available as a feed,
in [Atom](/blog.atom) format and [RSS](/blog.rss) format.

```{.unwrap pipe="sh | pandoc -t json" id="posts"}
export BASE_DIR='blog'
export STRIP_PREFIX='root/rendered/blog/'
find root/rendered/blog -type f -o -type l | grep -v "index.html" |
                                             sort -r              |
                                             showPosts
```

<!-- We strip leading spaces with sed to avoid becoming a code block -->

```{.unwrap pipe="sed -e 's/^ *//g' | pandoc -f markdown -t json"}
<script type="text/javascript" id="rantScript">
  document.addEventListener('DOMContentLoaded', function() {
    function insertAfter(existing, newNode) {
      existing.parentNode.insertBefore(newNode, existing.nextSibling);
    }

    var switcher = document.createElement('p');
    switcher.classList.add('rant-toggler');

    var toggle = (function() {
      var visible = true;
      var rants   = Array.prototype.slice.call(
        document.getElementsByClassName('rant'));

      return function toggler() {
        rants.map(function(x) {
          if (visible) {
            x.classList.add('hidden');
          } else {
            x.classList.remove('hidden');
          }
        });
        switcher.innerHTML = visible ? 'Show rants' : 'Hide rants';
        visible = !visible;
      };
    }());

    switcher.addEventListener('click', toggle);
    insertAfter(document.getElementById('rantScript'), switcher);

    toggle();
  });
</script>
```
