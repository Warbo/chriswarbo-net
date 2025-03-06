---
title: Web Hosting With IPFS and Nix
---

## Intro ##

I'm a big fan of
[pure functional programming](https://en.wikipedia.org/wiki/Functional_programming),
the [Nix build system/package manager](https://nixos.org/nix) and more recently
the [IPFS immutable, distributed cache](https://ipfs.io). I've just migrated
[this Web site](/) to IPFS, and this describes how I did it, why I did it and my
experience so far.

This Web site's been through many changes over the years, starting out on
[blogspot.co.uk](http://blogspot.co.uk), then self-hosted as a dynamic site
(using [ocPortal](http://ocportal.com)) on an
[Amazon EC2](https://aws.amazon.com/ec2/) server, and more recently as a
[static site](https://en.wikipedia.org/wiki/Static_web_page).

Sites like blogspot.co.uk are "Software as a Service" (SaaS; or, as Richard
Stallman refers to them, SaaSS:
["Service as a Software Subtitute"](https://www.gnu.org/philosophy/who-does-that-server-really-serve.en.html)).
These can be useful, but it's dangerous to rely on them without some tangible,
commercial agreement in place. The question to ask is what impact might there be
if the service disappeared overnight?

If this site disappeared, it would cause me some significant short-term pain
since I currently host many software repositories at [/git](/git); I have
multiple clones/backups of these, but pushing them somewhere else and updating
the URLs I use would be frustrating. Longer-term, there are
[several links scattered around the Web](https://www.google.co.uk/search?q=chriswarbo.net+-site:chriswarbo.net)
which would break; not many, but in places which I find important, e.g.
discussions I've been involved in; answers on sites like
[stackexchange](http://stackexchange.com/users/474782/warbo); etc.
I've even found references to my source code repositories, so some people
clearly found them useful for something.

Since I don't have permission to change many of these, and likely am unaware of
many others, there's no way I could fix such references.

That was one motivation for getting my own domain name ([chriswarbo.net](/)) and
self-hosting. Even as I've changed the underlying software, layout, etc. of the
site, I've tried to keep old URLs working via redirects.

Making the site dynamic, where pages are built on-demand by a piece of software
rather than being stored as-is on disk, is a common approach on the Web these
days. Software like [Wordpress](https://wordpress.org),
[Drupal](https://www.drupal.org), [MediaWiki](https://www.mediawiki.org), etc.
do this, and it's pretty easy to:

 - Add features, like comments and user accounts
 - Manage the site via a Web interface, e.g. writing blog posts via a Web form

After doing this for a few years, I found I was spending more time tending to
the site than actually adding new content. The programs which run a dynamic site
are directly exposed to the (bots controlled by) malicious spammers, crackers
and other baddies which inhabit the Internet. It's easy for these programs to be
exploited via some known or unknown security vulnerability, so it's important to
monitor the server for nefarious activity, and to keep all of the software
involved in running the site up to date and locked down.

This was the major reason I switched to a static site. All of the content on
this site is generated once, by me, then copied over to the server. The software
that runs on the server only has to copy the data from disk to the socket that's
requested it; there are far fewer ways this can go wrong compared to a dynamic
site, so the "attack surface" is much lower, and I don't need to perform as much
maintenance.

With the maturation of [Javascript](https://en.wikipedia.org/wiki/JavaScript),
lots of the logic and interaction which previously had to be done on a server
using a dynamic site, can now be performed by (
[some](https://www.w3.org/wiki/Graceful_degradation_versus_progressive_enhancement))
Web browsers instead. This removes a lot of the need for dynamic sites and hence
static sites seem to be getting popular again.

Since static sites are so simple to run, many SaaS providers offer to host them.
For example, I could use [Amazon's S3 system](https://aws.amazon.com/s3) instead
of my EC2 server; or I could use a service like
[Cloudflare](https://blog.cloudflare.com/secure-and-fast-github-pages-with-cloudflare).
However, this brings up the problems of SaaS again: whilst I'm not the most
competent Web master, I'd rather make mistakes *myself*, and be able to fix
them, than hand the reigns over to a third party, which may screw up in ways
that are beyond my control. (At least, for personal things; professionally, I'd
offload as much work as possible to those who are more experienced, *provided we
have a contract in place*).

There is another way, though.

## IPFS ##

IPFS is a distributed, content-addressed cache, which is accessible over HTTP
via "gateways". I think of IPFS as being like
[BitTorrent](https://en.wikipedia.org/wiki/BitTorrent): to get a file from
IPFS, pieces of it are fetched from anyone who has a copy, and reassembled to
get the result.
[Content-addressing](https://en.wikipedia.org/wiki/Content-addressable_storage)
means that the "name" or "address" of a file is derived from its content (via
[cryptographic hashing](https://en.wikipedia.org/wiki/Cryptographic_hash_function));
even though we might fetch it from multiple unknown places, we can be confident
that the result is what we asked for by deriving an address from what we were
given, and seeing if it matches the address that we asked for.

The nice thing about IPFS is that it decouples addresses from physical machines.
As long as [chriswarbo.net](/) points to a particular Amazon EC2 server, I have
to make sure that system stays up; even if its only job were to farm the
requests out to a pool of other machines. This is why we have servers in the
first place; if I were to host the site from my laptop instead, it would become
unavailable whenever it's suspended, or travelling without Internet access. If I
used some other machine on my home network, some unrelated activity (e.g.
gaming) might bring down the site, e.g. if I'm forced to reboot or a bug crashes
the system.

With the traditional Web, we're forced to *either* dedicate an entire machine to
*only* serving our Web site, just in case anything else breaks it; *or* we can
use a machine to its full potential, running the risk that everything might come
crashing down.

With IPFS, this false dichotomy disappears; since anyone can contribute to
hosting a site, we don't need a dedicated server machine. Sure, it's good to
know that there are a few reliable machines seeding the content just in case
nobody else is, but as long as there are *several* machines doing the hosting,
there's no need to be paranoid about any one of them breaking. For example, if
one of my home machines breaks, it's quite likely because I've been fiddling
with it over SSH; in which case, my laptop will be online and contributing to my
hosting.  Conversely, when my laptop's not available due to being suspended or
offline, it's not very likely that I'll be messing with the setup on my other
machines.

With this reasoning out of the way, I'll describe how I went about switching my
Web site to IPFS.

## Switching to Nix ##

The first thing I did was to switch the build system away from
[make](https://www.gnu.org/software/make/) and use Nix instead. I think of Nix
as "make done right", and the existing setup was convoluted enough that I didn't
want to faff around with doubly-escaped references, recursive invocations, etc.

Switching over to Nix was pretty straightforward. It exposed a few hidden
assumptions I'd been making in the build process, along with some occasional
invalid HTML in my posts. Fixing these up was pretty easy, since the modularity
of the build process pinned down where problems occur quite nicely.

### Repo Pages ###

One particular issue I ran into was generating
[pages from my git repositories](/projects/repos/). With make, I was fetching a
directory listing of [chriswarbo.net/git](/git), checking each
repo for when it was last modified and comparing these to the pages we already
had, to see if they needed regenerating.

Under Nix, our builds take place in isolated environments; there's no "previous
build" for us to check, and whilst it would be easy enough to look outside the
build environment for such a thing, it wouldn't smell right.

The problem with checking latest versions during a build script is that Nix has
no idea that the build depends on these external changes, and hence it won't
rebuild the results when needed. The safe solution is to use
`builtins.currentTime` as an input to the build; since that will always differ
between invocations, the result will always get rebuilt. For trivial things,
this can work well enough, but building these pages is *slow*.

Instead, I used a Nix function I've written called `latestGit`, which checks out
the latest revision of a given repository URL. This works in two phases: the
first uses `builtins.currentTime` to avoid caching, and outputs the latest
commit ID (configurable, but defaults to `HEAD`); the second phase uses this ID
as input, and outputs the repo contents at that commit. This second phase gets
cached, since the contents of a commit doesn't change. This content is used as
input to the repo pages, and hence a typical build will check for the latest
commit ID, and use the cached page for that ID.

Unfortunately, given the number of git repos to check, it can be pretty slow to
even *check* for the latest versions; each repo gets checked separately, and
each check starts a new shell in a fresh build environment.

After trying a few different approaches, I'm now fetching the latest commit IDs
*before* invoking Nix to perform a build; storing them as JSON in the
environment, and reading them out in Nix (using `builtins.getEnv` and
`builtins.fromJSON`). If a repository doesn't have a commit in the environment,
we fall back to `latestGit` as described above. This fetching takes place in a
bash script called `render`, which already takes care of things like locking the
build process behind a mutex, updating IPNS (see below), etc. By using a single,
existing shell process, without a specially-crafted environment, it only takes a
couple of seconds to get all of the IDs.

## Switching to IPFS ##

### Absolute Links ###

Sites hosted on IPFS can use absolute links, but it makes more sense to make
things relative. Since addresses are derived from the content, changing the site
will give it a different address, and it would be nice if old versions of the
site (which remain available as long as somebody bothers to host them) contained
links to themselves, so we could browse them as normal, rather than getting sent
to the latest version at every click.

My first step was to add a test script which checks for the presence of absolute
URLs (in `href` and `src` attributes of `a`, `img`, `script`, etc. elements). I
first tried to replace all occurrences of absolute links with relative ones, but
that got quite confusing, especially for auto-generated pages and redirects. Now
I have a simple post-processing script which runs over the whole site, after
page generation but before testing, which replaces absolute URLs with relative
ones using `xmlstarlet`. This works nicely, once I made it robust to the use of
namespaces.

### IPNS and Keys ###

Since each version of the site gets a different IPFS address, based on its
content, we need some way to distinguish the "latest" version. We can do this
using IPNS, the Inter-Planetary Name Space/Service. This is like DNS, but uses
cryptographic keys for the address (public) and the permission to update that
address (private). After adding a new version of the site to IPFS, we can do:

``` bash
ipfs name publish HASH-OF-LATEST-VERSION
```

This will point the address of our public key to the new version we just added.
However, this has a problem. An IPFS node will generate a keypair as part of its
initialisation. If I point `chriswarbo.net` to the IPNS address of this node's
public key, then:

 - I won't be able to update that IPNS address from another node/machine
 - I won't be able to update the IPNS name at all if my machine dies!

There are tools to help with this: one is `ipfs-key`, which will generate a
keypair as *files*, which we can back up; the other is `ipns-pub`, which can
update any IPNS address if we pass it the private key file. I couldn't get these
to build, since I'm not particularly experienced with Go :(

Thankfully, since version `0.4.5` the main `go-ipfs` implementation can now do
these things itself, without requiring such external tools. Rather than passing
files around, we add keys to a "keystore" (a simple name/value DB) using
arbitrary names (nothing to do with the DNS, IPFS or IPNS names!). We can then
use those names in our command invocations to publish to the corresponding IPNS
record.

Keys generated by the `ipfs key gen` command are named by default. The
associated key file will be in the `keystore` directory of your IPFS node, which
you can copy out and back up. The node's default key (generated during
`ipfs init`) is called `self`, and doesn't currently live in the `keystore`
directory. Instead, a base64 version is kept in the `config` file, under
`Identity.PrivKey`. If you want to back that up you can decode the base64 and
write it to a file in the `keystore` directory; don't call it `self` as that's
reserved!

I've successfully taken the private key from my laptop's node, copied it to a
file in my laptop's `keystore`, and in to the `keystore` of a separate machine
(a raspberrypi), and have verified that I can publish to the IPNS name from
both; hence future-proofing my IPNS and DNS addresses against machine failure.

It should go without saying, but I'll say it anyway: the key files are *private*
so they should not be copied around willy nilly; they shouldn't be checked into
version control; they shouldn't be published to IPFS; etc. Treat them like your
passwords, or password manager database: make sure you have backups, but only in
safe, secret locations.

Note that IPNS names expire periodically (every 24 hours), so you should have a
cron job or script which periodically reinserts the latest version, e.g.

```
ipfs name publish "$(ipfs resolve -r /ipns/my.domain)"
```

### Git Repos ###

As mentioned in the introduction, I host git repositories on my site at
[`/git`](/git). I really don't want to break these URLs, but since IPFS
generates a whole new address when any content changes, this requires adding a
whole new version of the site to IPFS whenever any repo gets updated.

This can be *very* slow; Nix will cache all of the intermediate build products,
so *generating* the site is quite fast if only a few things have changed, but
recursively *adding* the result to IPFS can take a while, as it hashes
everything from scratch.

Since we know that most of the site is unchanged, this is quite wasteful. We
only need to add the changed repo directory, and regenerate those MerkleDAG
nodes which point to it (i.e. the hashes of the directories above the repo).
Since each repo has a location like `/git/foo.git`, only the hashes for `/git`
and `/` need regenerating after a repo update; crucially, the site pages and all
of the other repos can use their existinge hashes.

Tracking and caching such changes is *exactly* what Nix is good at; so I've
altered the site's build scripts to not only generate the required pages and git
repo directories, but also to add the individual repos and top-level
pages/directories to IPFS. By "packaging" the IPFS hashes, we can reuse cached
versions, and make simple Nix functions to combine them together using calls to
`ipfs object` (recreating what `ipfs add` would do internally). The result is an
IPFS hash for the new site build, suitable for publishing to the IPNS name,
without waiting for the whole MerkleDAG to be regenerated.

One thing to keep in mind when using `go-ipfs` is that the version used by your
`ipfs ...` commands should be the same as that of any running daemon. To ensure
this, my build scripts will look for and use the system's IPFS binary
(`/run/current-system/sw/bin/ipfs`) if it exists; this is impure, but solves
more problems than it creates :)

### DNS and Gateways ###

It's all well and good to host a *copy* of a site on IPFS, and if it's a new
site then you may be happy to give out the IPFS/IPNS URL. On the other hand, if
your site has previously been hosted via HTTP then there are probably links
floating around which you'll want to redirect to the IPFS version. There are two
ways to do this, which both involve
[updating your DNS record](https://ipfs.io/ipfs/QmNZiPk974vDsPmQii3YbrMKfi12KTSNM7XMiYyiea4VYZ/example#/ipfs/QmRFTtbyEp3UaT67ByYW299Suw7HKKnWK6NJMdNFzDjYdX/websites/README.md).

The first change is to add a `dnslink` attribute to a `_dnslink` subdomain. That
contains the path to your site, which might be `/ipfs/something` if it's never
going to be updated, or more likely a `/ipns/something` path so it won't need
changing as updated versions are pushed. With this in place, you should be able
to use a path like `/ipns/your.domain` and IPFS will resolve it by checking your
DNS record. For example, `ipfs get /ipns/chriswarbo.net` will download a copy of
this site over IPFS. These `dnslink` entries will also be picked up by browser
extensions; although the [Firefox version](https://addons.mozilla.org/en-GB/firefox/addon/ipfs-gateway-redirect/)
won't redirect to the `dnslink` unless an "experimental" option is ticked
(presumably because it's easy to serve content that's completely different to
where the `dnslink` points).

Now that the IPFS network can query our domain, we need to ensure that clients
accessing the domain via HTTP, without an addon, are given the right content.

One way is to use a "gateway" to translate between HTTP and IPFS. Gateways serve
a similar role to the dumb disk-to-socket server I currently use, but they
decouple Web servers from Web sites: clients can choose to access an IPFS site
via any gateway (unless the operator has imposed restrictions), and gateway
servers can choose to host any IPFS site (see "pinning" below).

To send existing DNS/HTTP clients to an IPFS site, we unfortunately have to
choose *one* particular gateway to send them through; this introduces a single
point of failure: if that gateway server goes down or the operators change its
functionality. The most popular gateway at the moment is
[`ipfs.io`](https://ipfs.io), which we could redirect clients to [using our DNS
record](https://ipfs.io/ipfs/QmNZiPk974vDsPmQii3YbrMKfi12KTSNM7XMiYyiea4VYZ/example#/ipfs/QmRFTtbyEp3UaT67ByYW299Suw7HKKnWK6NJMdNFzDjYdX/websites/README.md).
The gateway will see which the client has come from, look up its `dnslink` and
serve the relevant content.

### Mirroring an IPFS Site on HTTP ###

Rather than sending clients to a gateway, I'm still serving HTTP requests to
`chriswarbo.net` with a static file server (of course, everyone's free to browse
my site via an IPFS gateway if they like; I'm just not directing people to one
by default). I could avoid relying on SaaS by running my own gateway, but:

 - That's dynamic code, which I want to avoid on my server for reasons talked
   about above.
 - I'm not in a position, resource-wise, to be proxying arbitrary requests
   through my server.
 - It's easier, simpler and more secure to use a server that's only capable of
   offering the files I want, rather than attempting to lock-down a
   general-purpose gateway (e.g. with a URL-filtering reverse proxy).

My current setup will push to both IPFS and my HTTP server, and update the IPNS
name. This ensures the content's always in sync, and hence I can treat HTTP as
a legacy fallback for those not browsing via IPFS.

### Sync Troubles ###

One approach I tried was to use the FUSE filesystem offered by `go-ipfs`,
running my HTTP server straight out of the relevant `/ipns` directory. Whilst
that works for a time, it's unfortunately not very stable, presumably due to the
resource requirements of the IPFS daemon compared to what my puny server can
offer.

Rather than relying on the IPFS daemon being up 24/7, it's instead much easier
to run `ipfs get /ipns/chriswarbo.net` to grab a copy of the latest site, then
move it into place. The downside to this is having multiple copies of the site
on disk (since it's in the site's IPFS datastore too). For small sites this
won't usually matter, although the pages autogenerated from my git repos can be
quite large.

In the end, I've fallen back to using `rsync` to push updates, as it handles
in-place updating more gracefully. Note that we must ignore timestamps, and rely
on checksums instead, if we're copying data out of Nix; for reproducibility, Nix
sets all timestamps to 1970-01-01.

### NAT, Firewalls, etc. ###

One annoyance with IPFS is, like pretty much all peer-to-peer software, it can
be tricky to connect two machines directly if they're both behind a firewall
and/or NAT. This can make it frustrating to send data directly between, e.g. my
laptop and Web server.

The standard workaround would be to use SSH tunnels, but this doesn't seem to
work for IPFS as-is. We can set up a tunnel for each node's port 4001, but this
won't be picked up by `ipfs swarm connect`: whichever node which receives that
command will connect correctly, but the other end will get a bogus port,
presumably a result of SSH magic. I can't see a way to force *both* ends to use
particular ports, and simple experiments with trying to add/get data across the
two nodes shows that they're clearly not connecting directly.

As a workaround, I've written
[a script which will transfer IPFS blocks over SSH](/git/chriswarbo-net/git/branches/master/static/ipfs-blocksend.raw.html).
Note that some IPFS commands don't preserve data through a round trip, e.g.
`ipfs object get <foo> | ipfs object put`, since we can get double-encoding
issues ([probably a bug](https://github.com/ipfs/go-ipfs/issues/1724), but in
any case it doesn't currently work). I've found that
`ipfs block get <foo> | ipfs block put` *does* preserve the data, so the
hashes remain intact, and we can use this to send new site versions across to
the server.

Of course, it would be nice if we only had to send the changes. We can do that
by using a command like `ipfs refs local` which will tell us what blocks a node
is already storing. We can remove these from the output of
`ipfs resolve -r /ipns/chriswarbo.net; ipfs refs -r /ipns/chriswarbo.net` to get
only the blocks which need to be transferred. We then loop over these with
`ipfs block get`/`ipfs block put` to do the transfer.

With these blocks transferred, we can "pin" the new version of the site; this
ensures that those blocks will be kept on the server (unpinned blocks may be
garbage collected to free up space) and hence it will act as a seed for the
site.

## Conclusion ##

Overall I'm quite impressed with the stability, documentation, community, etc.
around IPFS. I've encountered a few difficulties, but those have been easy
enough to work around with scripts, and often caused by the legacy issues of
keeping my old URLs working :)

For more information, check out [this site's source code](/git/chriswarbo-net).
