---
title: Web Scraping
---

## Scraping in the Presence of Javascript ##

I recently wanted to scrape a Javascript-heavy Web site. Good Web sites provide
a meaningful structure in their HTML, which we can fetch with `curl` or `wget`
and query with tools like `xidel`. Unfortunately many crappy sites will have
poor page structure, and generate a lot of their content after the initial
request by using Javascript.

There are tools like PhantomJS which can be used to help scrape such sites, but
they're caught in an awkward position: they must be complicated beasts in order
to support the plethora of Javascript out there on the Web (especially since
Javascript is so brittle and Web APIs are so numerous and bloated); yet their
role as user agent less comprehensive than a Web browser.

For a while now I've been using Firefox to do such scraping, since being a
popular user agent means it's more likely that a site will work in it.
Unfortunately Firefox is a pain to control: we could use Selenium, but then we'd
have two problems (getting our scraper to connect to and control Selenium, and
getting Selenium to connect to and control Firefox; not to mention aligning the
versions of everything, and presumably the planets too...). I found it easier to
use a combination of Xvfb and xdotool to drive Firefox's developer console.

## Headless Browsers ##

Recently both Firefox and Chromium have gained support for headless browsing. On
the one hand this is good, since it means we don't have to fight with global
nonsense like conflicting Xvfb display numbers; but on the other it means giving
up xdotool as a means of controlling the browser.

I decided to give headless Chromium a go, since it can provide a control API
over a socket (it can also bind to a localhost port, but that's just introducing
another unnecessary global conflict!). After several attempts to pass in sockets
via file descriptors, trying to connect as a client and as a server, I gave up
and abandoned that approach completely; it seemed almost as bad as Selenium!

## Chromium REPL ##

There is actually a much nicer way to drive Chromium: using it as a Javascript
REPL via the `--repl` argument. This is very similar to the way I drove Firefox
from its developer console, but it uses a nice, local stdio interface rather
than requiring X11 hacks.

There is some cruft involved, like a lot of verbose messages which we may want
to filter out, but underlying that there's a simple mechanism to send in
snippets of Javascript and get back JSON. Importantly, we can do all of our
control flow, etc. from the "outside" (e.g. in bash) rather than having to write
*everything* in Javascript (like we would for PhantomJS, for example).

## Experience So Far ##

I've made some scrapers using a headless Chromium REPL. A couple of things I've
come across:

 - In general, it's best to do as little as possible in the browser. If we can,
   it's far easier to just dump out the page (e.g. by getting the `outerHTML` of
   the body) and process it with dedicated tools like Xidel.
 - Polling the browser, e.g. every second, seems to work better than sleeping.
   I'm not sure if Chromium blocks on stdin, but I certainly found that if I
   slept for 10 seconds then dumping out the page, some events hadn't fired;
   compared to sending `null` once per second for 10 seconds, *then* dumping out
   the page source.
 - Queries might be a good idea, e.g. checking whether particular elements have
   been added to the page yet, but I found it overkill. Sleeping works perfectly
   well for my needs :)
