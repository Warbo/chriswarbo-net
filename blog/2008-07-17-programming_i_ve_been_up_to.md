---
title: Programming I've Been Up To
---
I haven't posted in a while, and whilst I have a rather lengthy post brewing in
my Home folder I thought I'd make a shorter one for the time being.

I've been playing around in Python, C and C++ and aside from my usual
experiments I've been trying to push (literally) a new blogging, and general
notification, system by getting [XMPP](http://www.xmpp.org/rfcs/rfc3920.html)
[PubSub](http://www.xmpp.org/extensions/xep-0060.html) integrated into more
applications. The big problem I've found is that there is very little to work
with from an application point of view, most development efforts seem to be
web-focused.

After a little play around with [Gloox](http://camaya.net/gloox) it became
obvious that its PubSub support, whilst probably going to be awesome when it's
finished, is unusable at the moment (no amount of messing could even get their
example to work :( ). Since my C++ is laughably bad, Gloox would end up
releasing their PubSub support before I'd become capable of helping. Thus I
decided I'd write a PubSub-capable library in Python using
[xmpppy](http://xmpppy.sourceforge.net/) (both of which I'm very familiar with).

I've been working on it for a couple of weeks now and it seems to be taking
shape nicely. I started by essentially writing methods to send every sending
example given in the XEP-0060 specification. This I originally did by sticking
strings together since XML libraries are usually over my head, but now that
they're all there I've decided to take the plunge into using XML
programmatically and have learned to use
[elementtree](http://effbot.org/zone/element-index.htm) to rewrite these methods
(rewriting them as I test them).

In the past couple of days I've been working on dealing with the replies being
sent back from the server, and have implemented a way for replies to be sent to
handling functions defined within the original message's sending method. The
reply handlers add to and alter an XML tree containing all known servers, their
capabilities, their nodes, node configurations, subscribers, etc. which can be
used by applications as 'get' capability. After the handling functions have 100%
coverage I'd like to add a set of abstracted methods (true getters and setters)
to make using the library more Object Oriented, more Python-like and more
intuitive. This would allow the internal workings of the library (which are
probably a very poor design due to my inexperience) to be completely rewritten
again and again in future versions without breaking API compatibility.

Once the sending and receiving are working I'm thinking of making a translator
program which receives PubSub notifications and writes them to local files. In
this way it would be trivial to receive
[Atom-over-PubSub](http://www.xmpp.org/internet-drafts/draft-saintandre-atompub-notify-07.html)
in any current news reader capable of reading Atom (which is pretty much all,
and with
[converters](http://www.google.com/search?ie=UTF-8&amp;oe=UTF-8&amp;q=rss+atom+convert)
it is all). This would basically involve subscribing to some PubSub nodes,
logging in with the converter, giving it a file name to write to then adding
this file as an Atom feed in a news reader. This is obviously a temporary
solution since it still involves polling, but at least that polling is local and
not networked. After that I want to add support to an existing, or make my own,
offline blogging application. After that I have a few nice ideas bouncing around
in my head :)

For those who are bored by text manipulation I don't blame you :)
[Here](/git/python-random-walks/git/branches/master/MoonlightForest.py) is a
little Public Domain program I've written which draws pretty patterns in a
window instead :) Kind of looks like moonlight filtering down through clearings
in a forest canopy.... or thousands of simultaneous random walks brightening and
tinting every pixel they cross :) To run it you'll need
[Python](http://python.org/) and [Pygame](http://www.pygame.org/news.html)
installed, then run it however you'd run a Python program on your system :) As a
Python program it can be opened and changed with a text editor, so anyone
wanting to understand programming a little can play around with the settings
(the interesting ones have comments)

Oh, I've also discovered that my previous dislike of honey has changed, since
it's now awesome. That is all.
