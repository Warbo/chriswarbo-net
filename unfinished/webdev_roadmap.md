---
title: Simplified Roadmap to Becoming a Web Developer in 2019
---

IN REPLY TO https://news.ycombinator.com/item?id=18874028

I agree with others that this is scary and offputting. In particular I'd say that a lot of this stuff is legacy cruft: widely deployed and keeping existing people in work, but probably not the best idea for someone getting started today.
(It's interesting that YAGNI and KISS *do* appear in this roadmap, but don't seem to be followed!)

In particular we can now do so much with static HTML + JS that for the majority it's probably not worth going down the dynamic site route at all (LAMP, etc.). My "roadmap" would be to just serve static files straight from disk/RAM/CDN, whether it's a traditional Web site with content baked-in (maybe from a static site generator), or a JS-powered application which only needs a 'boot' script to start AJAXing to some API endpoints. Everything else can be done client-side.

In particular, trying to learn a full server-side language like Python or PHP, then a Web framework like Django or Laravel, giving them a suitable DB like Postgres, etc. is not only a really labour-intensive way to make HTML appear on a user's screen, but it also introduces its own problems which may bite further down the line (e.g. requiring layers of caching, maintaining state consistency, having a larger attack surface, etc.).

For informational sites my roadmap would be to serve static HTML files containing raw content (text + hyperlinks). That satisfies search crawlers and accessibility tools; gives a fallback to non-JS users For all progressive enhancement on top of that (styling, branding, layout, headers/footers/menus, media embedding, etc.) I'd recommend using Javascript, by having each HTML file embed a 'main' JS file. ; keeps content separate from presentation; keeps all styling, etc. in one place. If the site's really small, like a blog or some business info, those HTML files can be the storage format too. Larger/more-complex sites should generate the pages from some more appropriate source (e.g. a DB, business management application, whatever), but that generation is simplified by the bare-bones output.

Interactive/dynamic features obviously *do* require some server-side language (assuming our backend DB/whatever doesn't have an off-the-shelf HTTP endpoint already). However, we can use CGI or equivalent (AWS Lambda, etc.) to ignore the "Web" aspect and use any language we like to just read/write text via a stdio-like interface (which is easy enough for 'hello world'!). Since all of the HTML generation, etc. will be done on the client side, we only have to care about the raw information, so our only real concerns are what data we're exposing (i.e. permissions need to be checked server-side) and how we handle user input (i.e. validating/escaping needs to be done server-side).

Maybe we add rate limiters, URL routers, etc. at some point.
