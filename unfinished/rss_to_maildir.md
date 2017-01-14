---
title: Converting Feeds (RSS/Atom/etc.) to Maildir
---

For many years I've been using [Emacs](https://www.gnu.org/software/emacs) to
read my emails, and I use the same setup to read news as well (RSS, ATOM, etc.).
For all this time, I've struggled to get a nice workflow, something which
satisfies the following:

 - Reliable: I don't want to be restarting or recovering broken/stuck processes,
   or losing/corrupting messages.
 - Works offline: I shouldn't require an Internet connection to read my
   messages.
 - Native UI: I don't want to be stuck inside a Web browser.
 - Fast: I shouldn't have to wait for mail to download/open, or for programs to
   start/stop/refresh.
 - Keyboard driven: I don't want to use a pointing device.
 - Arbitrary extensibility (optional): This isn't necessary if the tool behaves
   exactly as I want it to; since that's unlikely, a scripting language or
   similar would be nice.

I haven't found a setup which satisfies all of these nicely; my previous setup
was *almost* there, and today I finally bit the bullet and implemented the
missing pieces. So far, I'm quite happy with the result!

tl;dr I've made
[my own version of feed2maildir](https://github.com/Warbo/feed2maildir), which
converts RSS feeds into maildir messages, without requiring any extra state
(e.g. config files or "last checked" databases) and avoiding duplicates.

## Previous Setup ##

### Mail Reader ###

I used to use [Gnus](https://www.emacswiki.org/emacs/GnusTutorial), but more
recently I switched to [mu4e](https://www.emacswiki.org/emacs/mu4e) since it's
*much* faster. Gnus uses Emacs Lisp to implement much/all of its functionality,
including downloading mail/posts, searching/organising messages, etc. This means
that the Emacs process itself is doing all of the work, which may be quite slow
since Emacs Lisp is a rather slow language. Also, since Emacs doesn't support
any concurrency mechanisms, any time we have to wait for something (either a
calculation, or just waiting for a server to send us a response), Emacs cannot
do anything else: the UI will freeze, and we can't perform any further
interactions until that task has finished.

To work around this, I used to run two Emacs processes: one for all of my
programming, commandlines, etc. and another just for Gnus. Since Gnus is pretty
slow, I would keep it running all the time, and manually refresh the messages
before starting to read them.

On the other hand, mu4e doesn't do much in Emacs Lisp at all. Instead, most of
the work is performed by a commandline program `mu`; mu4e just calls out to `mu`
as necessary and displays the results. This means I can use a single Emacs
process for everything, starting and stopping `mu4e` however I like.

### Fetching Mail ###

One way to speed up Gnus is to avoid going online: rather than having Gnus log
into IMAP servers or download RSS feeds, it's quicker to offload that into a
background process, storing messages on disk in a format like
[maildir](https://en.wikipedia.org/wiki/Maildir). This has the added advantages
that it works offline, and gives me an up-to-date backup of my email archive.
Coincidentally, `mu` operates on maildir archives, which made the transition
from Gnus completely painless!

For this, I've found the `mbsync` command provided by `isync` works quite
nicely; this can be run repeatedly using e.g. cron or systemd (I'm currently
using the latter, due to the tight integration in NixOS).

One thing to keep in mind is that the `mbsync` process can occasionally hang,
e.g. if it gets confused by network connectivity during a resume/suspend cycle.
Hence it's wise to always run it using `timeout`; a long timeout (e.g. an hour)
is fine, as long as hung processes eventually get killed.

I'm currently waiting 10 minute between checking my inboxen, and an hour between
a full download of all IMAP folders. These timers start *after* the previous
check has finished, which prevents multiple instances from clobbering each
other.

### Fetching News ###

I read "news", in various forms, from many places. As with my mail, I want
everything to be in maildir format, but this requires converting from formats
like [RSS](https://en.wikipedia.org/wiki/RSS) and
[Atom](https://en.wikipedia.org/wiki/Atom_(standard)).

There are several programs for doing this, e.g.
[feed2maildir](https://github.com/sulami/feed2maildir),
[universal aggregator](https://github.com/sloonz/ua),
[imm](https://github.com/k0ral/imm) and
[rssdrop](http://search.cpan.org/~acg/rssdrop-0.2/rssdrop). Unfortunately, none
quite fitted my use case; in particular, most are concerned with periodically
downloading feeds and checking against a database of known entries, or
last-checked times, etc. which I don't need or want. All I want is a robust rss
to maildir converter, which doesn't create duplicates; in fact, universal
aggregator seems to offer tools for doing this, but I couldn't figure out how to
build software written in Go :(

For a while I struggled along with imm, since it's written in Haskell and
therefore easy for me to hack on. Some problems I encountered were:

 - Taking a list of feeds from a config file, rather than when invoked.
   Thankfully the config file allows arbitrary Haskell to be executed, so I had
   it build the list dynamically by reading whatever was in the RSS cache.
 - It doesn't allow accessing local files. To work around this, I would start a
   local Web server in the RSS cache directory before invoking imm, have imm
   download feeds from `localhost`, then kill the server. This worked, but was a
   pretty ridiculous situation to be in!
 - imm is *very* fragile. In particular, it bails out when accessing HTTPS, or
   when certain punctuation appears in certain elements (e.g. titles), etc. To
   work around this, I'd download HTTPS feeds to the RSS cache and serve them
   locally. I'd also pass all feeds through a script for stripping out
   punctuation, etc.
 - Despite maintaining a database on disk, imm often outputs duplicate entries.
   It also mangles some fields, e.g. it doesn't seem to handle spaces in the
   `author` field, causing many messages to have uninformative `From` fields
   like `The`.

Whilst some of these workarounds are pretty silly, it was the last bullet point
that finally made me decide enough is enough, and I wouldn't put up with seeing
the same posts again and again.

## New Setup ##

My requirements were actually pretty straightforward: parse RSS data, generate
maildir entries, but skip any which are already present. This is a *subset* of
what these existing programs already do! The problem is, they don't *expose*
that functionality:

 - They parse RSS data, but only from URLs.
 - They can fetch data from arbitrary places, but only when they're written in
   some config file.
 - They perform redundancy checks, but against a separate database to the actual
   maildir entries.

Since these are all
[Free Software](https://www.gnu.org/philosophy/free-sw.en.html), I decided to
pick one which was closest to doing what I wanted, and [fork the project into my
own tool](http://chriswarbo.net/projects/repos/feed2maildir.html).

I chose [feed2maildir](https://github.com/sulami/feed2maildir), since that
seemed to be more robust at handling arbitrary input than imm (presumably thanks
to its use of [feedparser](https://pythonhosted.org/feedparser/)). It also
doesn't have a [needlessly complicated approach to
configuration](https://hackage.haskell.org/package/dyre) (allowing
arbitrary Haskell as config is great; but why compile it into the program, when
you could just `eval` it or read the stdout of a `runhaskell` process instead?).

The first change I made to the feed2maildir code was to remove the requirement
for a configuration file. Since we're calling `feed2maildir` from a script (to
handle scheduling, etc.), we can use that script to read/generate the required
feed details, in any way we like, and pass them on to the process via
commandline arguments. This meant the addition of a `-n` option for specifying
a feed's name, which is used as the `From` field of the resulting messages.

This takes us from a configuration file like:

```javascript
{
  "Feed1":       "http://example.com/feed.rss",
  "Useful News": "http://example.org/feed.rss",
  "Gossip":      "http://example.net/feed.rss",
  ...
}
```

and an invocation like:

```bash
feed2maildir -s -m ~/Mail/feeds
```

To *no* configuration file, and an invocation like:

```bash
feed2maildir -s -m ~/Mail/feeds -n "Feed1"       "http://example.com/feed.rss" \
                                -n "Useful News" "http://example.org/feed.rss" \
                                -n "Gossip"      "http://example.net/feed.rss" \
                                ...
```

Next, I threw away the ability to process multiple feeds at once. This
functionality can be implemented with a loop, so there's no need for the tool to
include it. This take us to an invocation like:

```bash
while read -r LINE
do
  NAME=$(echo "$LINE" | cut -f1)
   URL=$(echo "$LINE" | cut -f2)
  feed2maildir -s -m ~/Mail/feeds/"$NAME" -n "$NAME" "$URL"
done < ~/.feeds
```

This uses a configuration file, but it has *nothing* to do with the tool; it's
just a convenience for our script. It also allows each feed to have a different
maildir path.

Next, I ripped out all of the feed fetching code. If we're calling the tool once
per feed, we might as well send the data into the
[standard input](https://en.wikipedia.org/wiki/Standard_streams), since that
lets us fetch the data any way we like: from disk, from the Web (e.g. via
[wget](https://www.gnu.org/software/wget/) or [curl](https://curl.haxx.se/)),
programatically, or any other source we can imagine.

This gives us an invocation like:

```bash
while read -r LINE
do
  NAME=$(echo "$LINE" | cut -f1)
   URL=$(echo "$LINE" | cut -f2)
  curl "$URL" | feed2maildir -s -m ~/Mail/feeds/"$NAME" -n "$NAME"
done < ~/.feeds
```

The next issue is preventing duplicates. feed2maildir attempts this using a
separate database, which stores the last-checked time for each configured feed:
when checking for updates, we update the time in the database; when we read the
feed, we ignore any posts from earlier than the last-checked time.

Storing a separate database seems wasteful to me, as well as the fact that it
may become out-of-sync from the maildir and that last-checked times may not bear
any relation to whether we've seen a post before. For example, if a film is
available on
[iPlayer](http://www.infradead.org/get_iplayer/html/get_iplayer.html) then its
release date is used as the post date. In this case, feed2maildir would only
acknowledge films released *since the iPlayer feed was last checked*, which is
very unlikely to find anything.

Instead, I ripped out the database in favour of reading the maildir directory
itself. Each message is given an extra
[header field](https://en.wikipedia.org/wiki/Email#Message_header), which I've
called `X-feed2maildirsimple-hash`. This contains the
[SHA256](https://en.wikipedia.org/wiki/SHA-2) of various identifying fields,
like the feed name, author, ID tag and title. When we process a feed, we begin
by reading all of the existing hash information from the given maildir, then we
skip any posts with matching hashes. This mechanism is extensible: each field's
hash is stored separately, a field is only included if it's present in that
post, and we only skip posts where the fields present on *both* have matching
hashes; this means we can add new fields without breaking the detection of old
messages without those fields.

This is working for me in place of the complicated imm-based setup I used to
use, and as always I've
[released all code](http://chriswarbo.net/projects/repos/feed2maildir.html) as
Free Software, in case it's useful to anyone.
