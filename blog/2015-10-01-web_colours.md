---
title: Fixing Web Colours
---

I've been using white-on-black widget themes for years, and one persistent problem is HTML pages specifying either the foreground colour *or* background colour, but not both. This either results in black text on a default (in my case black) background; or default (in my case white) text on a white background.

There are a few ways to mitigate this. One is to not use system colours in the browser, so the defaults are black-on-white. That's bad, because I *want* the white-on-black; otherwise I wouldn't have bothered with the theme!

Another approach is to forbid all custom styles. That works quite well, but can mess up in a few cases, eg. images with transparent backgrounds which assume it's white (like the rendered LaTeX on Wikipedia). I'm also undecided about stripping away *all* styling ability of sites; after all, many sites assume the presence of CSS in order to fix their broken layouts.

I'm currently trying another solution. I've written [some Javascript](/git/warbo-dotfiles/git/branches/master/conkerorrc/fix_page.js) for Conkeror which, after every page loads, walks the DOM and compares the foreground and background colours for every element containing text. It measures the Euclidean distance between the colours in RGB space, to get a rough measure of their contrast. It then checks if inverting the text colour would increase the contrast, and if so, it performs the inversion.

This is working quite well on those sites which were completely illegible before. There are still some quirks though, where high-contrast text is getting inverted to be illegible; this is usually on elements with background images, which is a trickier problem to solve.
