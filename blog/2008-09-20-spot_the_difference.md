---
title: Spot The Difference
dependencies: [ 'static/images/tronguy.jpg', 'static/images/tabposition.png',
                'static/file2img.sh' ]
---
Here's Google Chrome on Fedora:

![Chrome](http://farm4.static.flickr.com/3292/2860981151_d7f6d5b387_o.png)

Here's Firefox with a "Google Chrome theme" on Fedora:

![Firefox with Chrome Theme](http://blog.titax.fr/public/blog/chromifox.png)

What's the WTF-were-you-thinking mistake? Can't figure it out? How about if I tell you it also applies to the default Firefox theme on OSX:

![Firefox on OSX](http://farm3.static.flickr.com/2290/2088075583_bdac0801ed.jpg)

But not in the default theme for Windows:

![Firefox on Windows](http://mozillalinks.org/wp/wp-content/uploads/2008/02/vista_theme.png)

Or Ubuntu:

![Firefox on Ubuntu](http://mozillalinks.org/wp/wp-content/uploads/2007/11/screenshot-firefox-3-beta-1-review-mozilla-links-minefield-1.png)

Still can't see? How about an analogy:

```{.unwrap pipe="sh | pandoc -t json"}
./root/static/file2img.sh "Analogy" < ./root/static/images/tronguy.jpg
```

Still don't understand? Let me spell it out:

```{.unwrap pipe="sh | pandoc -t json"}
./root/static/file2img.sh "Tab position" < ./root/static/images/tabposition.png
```

Chrome gets this (as does Opera, but I don't give a shit because their browser is still proprietary), but this supposedly-Chrome theme (as well as the OSX theme) have made the bad UI of Firefox EVEN WORSE. The default Windows and Gnome themes have tabbed pages, with some unknown, magical force changing the address bar when switching tabs. This is BAD since it requires either reverse-engineering, or study of the code, to find out the behaviour. However, with those 'upside-down' tab themes the tabs are now controlling the address bar, with an unknown, magical force changing the page content accordingly. This is EVEN WORSE since it is the page contents that people actually care about! Hype Firefox all you want, but I prefer looking at Web SITES to looking at Web BROWSERS.

In short: EPIC FAIL
