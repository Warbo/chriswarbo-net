---
title: Git in Nix via IPFS
---

I noticed that the
[git-hashing](https://nix.dev/manual/nix/2.26/development/experimental-features#xp-feature-git-hashing)
feature has been added to Nix (although 2.26
[has a bug](https://github.com/NixOS/nix/issues/12295) which I had to build from
master to avoid). This feature allows Nix to hash directories in the same way
that [Git](https://git-scm.com/) would, rather than using its custom
[NAR hashing](https://flox.dev/blog/hash-collision/) approach (which archives
the directory contents into a
[NAR](https://nix.dev/manual/nix/2.26/command-ref/new-cli/nix3-nar.html) file,
and takes the hash of that). A few reasons I can think of for why this is
important:

 - Compatibility with existing systems. Lots of data is already stored in Git,
   and we already know the hashes of that data: it's the IDs that we use to look
   it up. Having Nix use these existing hashes to verify data means we don't
   need to specify *extra* hashes that aren't known; and we don't have to waste
   time calculating such hashes, which may be infeasible for huge repositories.
 - Distributing/deduplicating data. Git gives each directory (AKA "tree") its
   own hash, based on its contents. Trees can be nested by referring to their
   children by their hash. This is very useful in Git, since directories which
   remain unchanged between versions will re-use the same tree IDs. Whilst the
   `git-hashing` feature in Nix is a modest step, future improvements in this
   direction could be used to e.g. deduplicate store contents, or avoid having
   to download sources if they can be assembled from existing trees.

## git-on-ipfs ##

As a use-case for playing around with `git-hashing`, I wanted to see if I could
use the *same* hash to:

 1) Identify a directory/tree in a Git repository
 2) Distribute that directory using IPFS
 3) Fetch that directory into Nix

It turns out that all of the required pieces are out there, though I've not seen
them put together in this way before.

Here's a quick demo of what's now possible.

### Git objects ###

There are lots of great resources explaining how git works, but for our purposes
we need to know (a) how Git objects relate to each other, and (b) how they are
stored on disk.

Here's a simple Git repo to work with:

```{.sh}
$ mkdir -p A/AA
$ echo 'foo' > B
$ echo 'bar' > A/AA/AAA
$ echo 'baz' > A/AB
$ git init --quiet
$ git add A/AA/AAA
$ git commit -m 'Add AAA'
[master (root-commit) b588940] Add AAA
 1 file changed, 1 insertion(+)
 create mode 100644 A/AA/AAA
$ git add A/AB B
$ git commit -m 'Add AB and B'
[master 5c56457] Add AB and B
 2 files changed, 2 insertions(+)
 create mode 100644 A/AB
 create mode 100644 B
```

Let's show that last commit (which we can reference as `HEAD`):

```{.sh}
$ git rev-parse HEAD
5c5645719b42456ef8f20c68fdde81c1745d102f
$ git cat-file commit 5c5645719b42456ef8f20c68fdde81c1745d102f
tree 7f9566d72742f2a66ffa8d236965d86ffd2d0940
parent b5889400673e46b460af044b45a501f6bdc247ab
author Chris Warburton <chriswarbo@gmail.com> 1741914955 +0000
committer Chris Warburton <chriswarbo@gmail.com> 1741914955 +0000

Add AB and B
```

It contains two references; the `parent` is the previous commit (which doesn't
contain any `parent` of its own, since it was our first commit):

```{.sh}
$ git cat-file commit b5889400673e46b460af044b45a501f6bdc247ab
tree 8e82045fc031685a8cc1205c299838401d22dd08
author Chris Warburton <chriswarbo@gmail.com> 1741914955 +0000
committer Chris Warburton <chriswarbo@gmail.com> 1741914955 +0000

Add AAA
```

These `parent` references represent version history, branching, merging, and all
the things that Git is known for. However, we'll be ignoring all of that today,
instead focusing on the `tree` references, since that's what Nix's `git-hashing`
feature uses.

#### Trees and blobs ####

Here's the `tree` for that `HEAD` commit:

```sh
$ git ls-tree 7f9566d72742f2a66ffa8d236965d86ffd2d0940
040000 tree c00f6075c62addbd5a89b16c1d6c54b29793bbad	A
100644 blob 257cc5642cb1a054f08cc83f2d943e56fd3ebe99	B
```

Notice that a Git tree *only* stores references to other things: in this case
another `tree` called `A` and a `blob` called `B`, with their associated
permissions. A Git blob stores the contents of a single file, and a Git tree
therefore represents a directory. We can use the `git archive` command to get a
TAR file containing a particular tree.

A tree may contain the same blob/(sub)tree multiple times, as long as they have
different names; and a blob/tree can appear in multiple trees. Storing data in
this way allows Git to re-use existing files and folders, rather than storing
copies: when a file is changed, a new blob is created with that content, the
trees which referenced it are replaced by new trees referencing the new blob,
and any trees referencing those trees are replaced, etc. All of the *other*
blobs can remain as-is, along with any tree that *doesn't* contain the edited
file (either directly or indirectly); so the overall impact is quite limited.

For completeness, here's the rest of the tree structure of the `HEAD` commit:

```{.sh}
$ git ls-tree c00f6075c62addbd5a89b16c1d6c54b29793bbad
040000 tree c85ed5e98d6dd95e17d7efce8d73453086330466	AA
100644 blob 76018072e09c5d31c8c6e3113b8aa0fe625195ca	AB

$ git ls-tree c85ed5e98d6dd95e17d7efce8d73453086330466
100644 blob 5716ca5987cbf97d6bb54920bea6adde242d87e6	AAA
```

As a comparison, here's the tree structure of the parent commit (notice that it
uses some of the same blobs and trees as the `HEAD` commit):

```sh
$ git ls-tree 8e82045fc031685a8cc1205c299838401d22dd08
040000 tree a28a8e73ffa278c71485e91b30fd0182f9c5ab73	A

$ git ls-tree a28a8e73ffa278c71485e91b30fd0182f9c5ab73
040000 tree c85ed5e98d6dd95e17d7efce8d73453086330466	AA

$ git ls-tree c85ed5e98d6dd95e17d7efce8d73453086330466
100644 blob 5716ca5987cbf97d6bb54920bea6adde242d87e6	AAA
```

Finally, here are the blobs:

```sh
$ git cat-file blob 257cc5642cb1a054f08cc83f2d943e56fd3ebe99
foo

$ git cat-file blob 76018072e09c5d31c8c6e3113b8aa0fe625195ca
baz

$ git cat-file blob 5716ca5987cbf97d6bb54920bea6adde242d87e6
bar
```

If you're playing along at home, you'll get different commit hashes (due to the
inclusion of timestamps, etc. which won't match mine), but all of your blob and
tree hashes should be the same as those shown here!

#### Git object storage ####

Git can store objects in a few different ways, including compressed together in
"pack files", or in some external "alternate" directory. We'll limit ourselves
to the default storage method, which is to write individual files in the repo's
`.git/objects` folder. Each object is stored with a filename that matches its
ID (though they're "sharded" into subfolders based on the first two characters,
to avoid breaking the filesystem with too many files in one folder!)

```{.sh}
$ find .git/objects -type f
.git/objects/5c/5645719b42456ef8f20c68fdde81c1745d102f
.git/objects/c0/0f6075c62addbd5a89b16c1d6c54b29793bbad
.git/objects/25/7cc5642cb1a054f08cc83f2d943e56fd3ebe99
.git/objects/c8/5ed5e98d6dd95e17d7efce8d73453086330466
.git/objects/b5/889400673e46b460af044b45a501f6bdc247ab
.git/objects/8e/82045fc031685a8cc1205c299838401d22dd08
.git/objects/76/018072e09c5d31c8c6e3113b8aa0fe625195ca
.git/objects/57/16ca5987cbf97d6bb54920bea6adde242d87e6
.git/objects/a2/8a8e73ffa278c71485e91b30fd0182f9c5ab73
.git/objects/7f/9566d72742f2a66ffa8d236965d86ffd2d0940
```

The contents of these files match what we saw above, with some caveats:

 - The files on disk are compressed with zlib.
 - Each file (when uncompressed) starts with a "header", saying what type of
   object it is (blob, commit, tag or tree; I'm ignoring tags), its length
   (written in base-10 with ASCII digits), and a NUL byte.
 - Tree objects reference IDs written as raw bytes (i.e. base-256, where `a` is
   the digit for 97, or 0x61 in hex); whilst commit objects reference IDs
   written in ASCII (i.e. base-16/hex, where `a` is 10, or 0xa in hex)

The ID of a commit object is just the SHA1 hash of that uncompressed content,
written in base-16/hex encoded:

```{.sh}
$ while read -r OBJECT
> do
>   printf '%s: %s\n' "$OBJECT" "$(< "$OBJECT" zlib-flate -uncompress | sha1sum)"
> done < <(find .git/objects -type f)
.git/objects/5c/5645719b42456ef8f20c68fdde81c1745d102f: 5c5645719b42456ef8f20c68fdde81c1745d102f  -
.git/objects/c0/0f6075c62addbd5a89b16c1d6c54b29793bbad: c00f6075c62addbd5a89b16c1d6c54b29793bbad  -
.git/objects/25/7cc5642cb1a054f08cc83f2d943e56fd3ebe99: 257cc5642cb1a054f08cc83f2d943e56fd3ebe99  -
.git/objects/c8/5ed5e98d6dd95e17d7efce8d73453086330466: c85ed5e98d6dd95e17d7efce8d73453086330466  -
.git/objects/b5/889400673e46b460af044b45a501f6bdc247ab: b5889400673e46b460af044b45a501f6bdc247ab  -
.git/objects/8e/82045fc031685a8cc1205c299838401d22dd08: 8e82045fc031685a8cc1205c299838401d22dd08  -
.git/objects/76/018072e09c5d31c8c6e3113b8aa0fe625195ca: 76018072e09c5d31c8c6e3113b8aa0fe625195ca  -
.git/objects/57/16ca5987cbf97d6bb54920bea6adde242d87e6: 5716ca5987cbf97d6bb54920bea6adde242d87e6  -
.git/objects/a2/8a8e73ffa278c71485e91b30fd0182f9c5ab73: a28a8e73ffa278c71485e91b30fd0182f9c5ab73  -
.git/objects/7f/9566d72742f2a66ffa8d236965d86ffd2d0940: 7f9566d72742f2a66ffa8d236965d86ffd2d0940  -
```

### Generating git-hashed Nix outputs ###

Now we've seen how Git represents directories, and works out their hashes, let's
write a Nix derivation which uses the `git-hashing` feature to verify its
output.

First we'll put this in `nixpkgs.nix`, so we don't have to repeat it over and
over:

```nix
with {
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs2411";
    url = "https://github.com/nixos/nixpkgs/archive/62c435d93bf046a5396f3016472e8f7c8e2aed65.tar.gz";
    sha256 = "sha256:0zpvadqbs19jblnd0j2rfs9m7j0n5spx0vilq8907g2gqrx63fqp";
  };
};
import nixpkgs { config = {}; overlays = []; }
```

Now we can write the following function in `git-example.nix`:

```nix
with { pkgs = import ./nixpkgs.nix; };
{ tree }: pkgs.runCommand "git-tree-example"
  {
    inherit tree;
    repoData = ./.git;
    buildInputs = [ pkgs.git pkgs.iputils ];
    outputHashAlgo = "sha1";
    outputHashMode = "git";
    outputHash = tree;
  }
  ''
    # Prove we're online
    ping -c 1 chriswarbo.net > /dev/null

    ln -s "$repoData" .git
    mkdir "$out"
    git archive "$tree" | tar -x -C "$out"
  ''
```

This runs a `ping` command to check whether it has network access, and therefore
whether it's a fixed-output derivation (i.e. Nix will validate its output using
the specified `outputHash`). This is completely unnecessary, but it shows that
a real implementation is able to download any data it needs; rather than
hard-coding our example `.git` folder like I've done here!

What's interesting about this function is that its `tree` argument is used for
two things: it identifies the tree we want to extract (via `git archive`), and
it acts as the hash that Nix uses to validate the output. Hence we can run this
same function on any of the tree objects in our repo (including any of the
sub-trees!), *without* having to calculate any separate NAR hashes:

```sh
$ nix-build --argstr tree 7f9566d72742f2a66ffa8d236965d86ffd2d0940 git-example.nix
this derivation will be built:
  /nix/store/jzbbv4b9ancxygq2a37k77y2n9rsgil2-git-tree-example.drv
building '/nix/store/jzbbv4b9ancxygq2a37k77y2n9rsgil2-git-tree-example.drv'...
/nix/store/j9j8w6lbdf0sm209alp3imzaffdfxc0i-git-tree-example
$ find result/
result/
result/A
result/A/AA
result/A/AA/AAA
result/A/AB
result/B

$ nix-build --argstr tree 8e82045fc031685a8cc1205c299838401d22dd08 git-example.nix
this derivation will be built:
  /nix/store/c4j89q43yrak4l5rafdk7ddmc8pyxgw2-git-tree-example.drv
building '/nix/store/c4j89q43yrak4l5rafdk7ddmc8pyxgw2-git-tree-example.drv'...
/nix/store/7l36hn6i221dm8pv51llvr00w0nnzvar-git-tree-example
$ find result/
result/
result/A
result/A/AA
result/A/AA/AAA

$ nix-build --argstr tree a28a8e73ffa278c71485e91b30fd0182f9c5ab73 git-example.nix
this derivation will be built:
  /nix/store/pbparsl4wmz3fs00gs0kf2cvzi46rvsr-git-tree-example.drv
building '/nix/store/pbparsl4wmz3fs00gs0kf2cvzi46rvsr-git-tree-example.drv'...
/nix/store/bfd0qg5pp4bckkrqdrfccxqsb5sss1w5-git-tree-example
$ find result/
result/
result/AA
result/AA/AAA

$ nix-build --argstr tree c00f6075c62addbd5a89b16c1d6c54b29793bbad git-example.nix
this derivation will be built:
  /nix/store/ygqzpicdfzpxqj2vjnmxg4cxabnr4d1a-git-tree-example.drv
building '/nix/store/ygqzpicdfzpxqj2vjnmxg4cxabnr4d1a-git-tree-example.drv'...
/nix/store/ppzs062qrz75994kn5fi4khh13qkdlhf-git-tree-example
$ find result/
result/
result/AA
result/AA/AAA
result/AB

$ nix-build --argstr tree c85ed5e98d6dd95e17d7efce8d73453086330466 git-example.nix
this derivation will be built:
  /nix/store/4aj4slm05vxblvxn6g0c22nizfgk95dh-git-tree-example.drv
building '/nix/store/4aj4slm05vxblvxn6g0c22nizfgk95dh-git-tree-example.drv'...
/nix/store/76dqa2m7fl3c159v9l7prxwhgl2vkzjg-git-tree-example
$ find result/
result/
result/AAA
```

### Fetching git-hashed directories from IPFS ###

Now we're ready to have some fun: instead of limiting ourselves to a handful of
Git trees from our little example repo, we're going to fetch them from
[IPFS](http://ipfs.io/)! In theory, IPFS allows us to host our data on any
machines we like, and fetch them anywhere else, without worrying about
single-points-of-failure like domain names expiring. There are still some
practical issues keeping us from that content-addressed utopia, but for now I'm
going to show that we can at least get a few steps closer with Nix.

#### Putting Git objects into IPFS ####

IPFS works in a similar way to Git, but it makes some details more explicit and
granular. What Git calls an "object", IPFS calls a "block". Blocks can form a
DAG (directed acyclic graph) by referring to each other; like how Git commits
refer to their parents and trees, and Git trees refer to (sub)trees and blobs.
There's a fuzzy border between the world of blocks (IPFS, InterPlanetary
FileSystem) and the more abstract world of DAGs (IPLD, InterPlanetary Linked
Data), mediated by [codecs](https://github.com/multiformats/multicodec) which
can parse the contents of a block to find the links it contains (unlike Git,
which hard-codes a few object types and their encodings).

Thankfully, there's an existing codec for parsing Git objects, so we can just
dump our repo contents into an IPFS node like
[Kubo](https://docs.ipfs.tech/install/command-line/):

```sh
$ for OBJECT in .git/objects/*/*
do
  < "$OBJECT" zlib-flate -uncompress |
    ipfs block put --cid-codec git-raw --mhtype sha1 --allow-big-block
done
baf4bcfbfptcwilfrubkpbdgih4wzipsw7u7l5gi
baf4bcfcxc3fftb6l7f6wxnkjec7knlo6eqwypzq
baf4bcfc4kzcxdg2civxpr4qmnd655aobororaly
baf4bcfdwagahfye4luy4rrxdce5yvih6mjizlsq
baf4bcfd7svtnoj2c6ktg76unenuwlwdp7uwqsqa
baf4bcfeoqicf7qbrnbnizqjalquzqocadurn2ca
baf4bcffcrkhhh75cpddrjbpjdmyp2amc7hc2w4y
baf4bcffvrckaazz6i22gblyejnc2kapwxxbepky
baf4bcfgab5qhlrrk3w6vvcnrnqowyvfss6j3xli
baf4bcfgil3k6tdln3fpbpv7pz2gxgrjqqyzqizq
```

Since a Git blob stores the entire contents of a file, they can be arbitrarily
large. IPFS nodes tend to reject blocks over 1MiB
[for security
reasons](https://discuss.ipfs.tech/t/supporting-large-ipld-blocks/15093),
preferring to split files into a DAG of smaller chunks; we don't want that,
since it would result in hashes that don't match our Git IDs, so we'll force it
to accept our blobs using `--allow-big-block`.

Kubo also defaults to using SHA256, but again that would give different hashes
to our Git IDs, so we tell it to use SHA1.

#### Content IDs ####

The [CIDs](https://docs.ipfs.tech/concepts/content-addressing/) we got above
don't look like any of the Git object IDs we saw before, but they're just
encoded differently. Kubo has defaulted to printing them in base-32 (indicated
by the initial `b`), but we can convert them to
[other bases](https://github.com/multiformats/multibase) instead; like the
base-16 used by Git:

```sh
$ for CID in \
baf4bcfbfptcwilfrubkpbdgih4wzipsw7u7l5gi \
baf4bcfcxc3fftb6l7f6wxnkjec7knlo6eqwypzq \
baf4bcfc4kzcxdg2civxpr4qmnd655aobororaly \
baf4bcfdwagahfye4luy4rrxdce5yvih6mjizlsq \
baf4bcfd7svtnoj2c6ktg76unenuwlwdp7uwqsqa \
baf4bcfeoqicf7qbrnbnizqjalquzqocadurn2ca \
baf4bcffcrkhhh75cpddrjbpjdmyp2amc7hc2w4y \
baf4bcffvrckaazz6i22gblyejnc2kapwxxbepky \
baf4bcfgab5qhlrrk3w6vvcnrnqowyvfss6j3xli \
baf4bcfgil3k6tdln3fpbpv7pz2gxgrjqqyzqizq
do
  ipfs cid format -b base16 "$CID"
done
f01781114257cc5642cb1a054f08cc83f2d943e56fd3ebe99
f017811145716ca5987cbf97d6bb54920bea6adde242d87e6
f017811145c5645719b42456ef8f20c68fdde81c1745d102f
f0178111476018072e09c5d31c8c6e3113b8aa0fe625195ca
f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940
f017811148e82045fc031685a8cc1205c299838401d22dd08
f01781114a28a8e73ffa278c71485e91b30fd0182f9c5ab73
f01781114b5889400673e46b460af044b45a501f6bdc247ab
f01781114c00f6075c62addbd5a89b16c1d6c54b29793bbad
f01781114c85ed5e98d6dd95e17d7efce8d73453086330466
```

The results still look different from our Git IDs, but notice that they all
begin with the same pattern. That encodes
[extra information](https://cid.ipfs.tech/), which IPFS uses to process
CIDs and blocks:

 - `f` tells us the whole CID is encoded in base-16
 - `01` tells us this is version 1 of the CID format (CIDv1)
 - `78` indicates these are `git-raw` blocks (the codec we gave to `block put`)
 - `11` indicates these CIDs are using SHA1
 - `14` is the length of the hash (which is 20 bytes, in decimal)

If we ignore this initial pattern of `f01781114`, we see that the remaining
parts *do* match up with our Git IDs!

#### Gateways and CARs ####

Our Git objects are now living in our local Kubo node. There are many ways to
fetch them back out, including a CLI, a FUSE filesystem, and a HTTP API; but
we'll use the HTTP Gateway, since that will also work on remote hosts:

```sh
$ wget 'http://localhost:8080/ipfs/f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940'
--2025-03-15 14:17:43--  http://localhost:8080/ipfs/f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940
Resolving localhost (localhost)... ::1, 127.0.0.1
Connecting to localhost (localhost)|::1|:8080... failed: Connection refused.
Connecting to localhost (localhost)|127.0.0.1|:8080... connected.
HTTP request sent, awaiting response... 301 Moved Permanently
Location: http://baf4bcfd7svtnoj2c6ktg76unenuwlwdp7uwqsqa.ipfs.localhost:8080/ [following]
--2025-03-15 14:17:43--  http://baf4bcfd7svtnoj2c6ktg76unenuwlwdp7uwqsqa.ipfs.localhost:8080/
Resolving baf4bcfd7svtnoj2c6ktg76unenuwlwdp7uwqsqa.ipfs.localhost (baf4bcfd7svtnoj2c6ktg76unenuwlwdp7uwqsqa.ipfs.localhost)... ::1, 127.0.0.1
Reusing existing connection to localhost:8080.
HTTP request sent, awaiting response... 200 OK
Length: 65 [application/octet-stream]
Saving to: ‘f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940’

f017811147f9566d72742f2a66f 100%[=========================================>]      65  --.-KB/s    in 0s

2025-03-15 14:17:43 (2.80 MB/s) - ‘f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940’ saved [65/65]

$ sha1sum < f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940
7f9566d72742f2a66ffa8d236965d86ffd2d0940  -
```

We can successfully retrieve individual blocks, but that's not enough to
reconstruct a whole directory. Thankfully, IPFS understands the relationships
between these blocks (since we used the `git-raw` codec), which we can explore
using the `ipfs dag get` command, e.g.:

```sh
$ ipfs dag get f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940/A/hash/AA/hash
{"AAA":{"hash":{"/":"baf4bcfcxc3fftb6l7f6wxnkjec7knlo6eqwypzq"},"mode":"100644"}}
```

To fetch an entire DAG from a HTTP Gateway we need to ask for a
[CAR](https://ipld.io/specs/transport/car/) (Content Addressed Archive), using
an `Accept: application/vnd.ipld.car` HTTP header and a `?format=car` query
string:

```sh
$ curl -L -H 'Accept: application/vnd.ipld.car' \
  'http://localhost:8080/ipfs/f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940?format=car' \
  > f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940.car

  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   103  100   103    0     0  19788      0 --:--:-- --:--:-- --:--:-- 20600
100   401  100   401    0     0  34884      0 --:--:-- --:--:-- --:--:-- 34884
```

We can inspect and manipulate the resulting CAR file using tools like
[go-car](https://github.com/ipld/go-car/tree/master/cmd/car):

```sh
$ car ls f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940.car
baf4bcfd7svtnoj2c6ktg76unenuwlwdp7uwqsqa
baf4bcfgab5qhlrrk3w6vvcnrnqowyvfss6j3xli
baf4bcfgil3k6tdln3fpbpv7pz2gxgrjqqyzqizq
baf4bcfcxc3fftb6l7f6wxnkjec7knlo6eqwypzq
baf4bcfdwagahfye4luy4rrxdce5yvih6mjizlsq
baf4bcfbfptcwilfrubkpbdgih4wzipsw7u7l5gi
```

Lo and behold, we've downloaded precisely those blocks needed to reconstruct the
tree ID we requested!

#### Fetching from IPFS in Nix ####

We can now replace our silly Nix example with a more realistic alternative,
using `nixpkgs.fetchurl` to obtain a CAR file of the requested tree ID, and
putting the `git archive` shenanigans in a `postFetch` script. We'll also give a
list of public IPFS gateways as fallbacks, in case there isn't a gateway running
on `localhost:8080`:

```nix
with rec {
  pkgs = import ./nixpkgs.nix;
  go-car = pkgs.buildGoModule {
    name = "go-car";
    modRoot = "cmd";
    subPackages = [ "car" ];
    vendorHash = "sha256-woC3y3F+JFwhHvEhWRecTRPzXAyElvORXefIjbOIpHE=";
    src = pkgs.fetchFromGitHub {
      owner = "ipld";
      repo = "go-car";
      rev = "bb5432c1de5582e4b1f2859b429062d86ac71dab";
      hash = "sha256-E2B59qUkBEHiF8+HEaRjPtt0UFIB6zr7zeeWS0iM7HQ=";
    };
  };
};
{ tree }: (pkgs.fetchurl {
  name = "tree-${tree}";
  urls = map (base: "${base}/ipfs/f01781114${tree}?format=car") [
    "http://127.0.0.1:8080" # Try default local gateway first, if available
    "https://ipfs.io"
    "https://dweb.link"
    "https://cloudflare-ipfs.com"
    "https://gateway.pinata.cloud"
    "https://ipfs.infura.io"
  ];
  curlOptsList = ["-H" "Accept: application/vnd.ipld.car"];
  recursiveHash = true;
  downloadToTemp = true;
  hash = builtins.convertHash {
    hash = tree;
    toHashFormat = "sri";
    hashAlgo = "sha1";
  };
  postFetch = ''
    PATH="${pkgs.git}/bin:${go-car}/bin:${pkgs.kubo}/bin:${pkgs.qpdf}/bin:$PATH"

    # Extract blocks from CAR into an empty git repo's objects dir
    git init --quiet
    while read -r CID
    do
      FULL=$(ipfs cid format -b base16 -f '%D' "$CID")
      DEST=".git/objects/$(echo "$FULL" | cut -c-2)/$(echo "$FULL" | cut -c3-)"
      mkdir -p "$(dirname "$DEST")"
      car get-block "$downloadedFile" "$CID" | zlib-flate -compress > "$DEST"
    done < <(car ls "$downloadedFile")

    mkdir "$out"
    git archive ${pkgs.lib.escapeShellArg tree} | tar -x -C "$out"
  '';
}).overrideAttrs (old: { outputHashMode = "git"; })
```

This function takes a Git `tree` ID, like before, but uses it to:

 - Download a CAR of the directory contents from IPFS, and copy the blocks to
   the `objects/` folder of an empty Git repository
 - Extract the tree contents into the derivation's output directory
 - Verify the output

Here are all the Git trees from above:

```sh
$ nix-build --argstr tree 7f9566d72742f2a66ffa8d236965d86ffd2d0940 fetch-git-tree.nix
this derivation will be built:
  /nix/store/srx7d2bxksyf7cw2vxf5rs8rk82x1ln1-tree-7f9566d72742f2a66ffa8d236965d86ffd2d0940.drv
building '/nix/store/srx7d2bxksyf7cw2vxf5rs8rk82x1ln1-tree-7f9566d72742f2a66ffa8d236965d86ffd2d0940.drv'...

trying http://127.0.0.1:8080/ipfs/f017811147f9566d72742f2a66ffa8d236965d86ffd2d0940?format=car
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   401  100   401    0     0  72226      0 --:--:-- --:--:-- --:--:-- 80200
/nix/store/wy9jkh6ffsma4z9fisi06klzvkgak3c1-tree-7f9566d72742f2a66ffa8d236965d86ffd2d0940
$ find result/
result/
result/A
result/A/AA
result/A/AA/AAA
result/A/AB
result/B
$ nix-build --argstr tree c85ed5e98d6dd95e17d7efce8d73453086330466 fetch-git-tree.nix
this derivation will be built:
  /nix/store/fmi6ch8pr1saxa6s1ijr8if135cq75ph-tree-c85ed5e98d6dd95e17d7efce8d73453086330466.drv
building '/nix/store/fmi6ch8pr1saxa6s1ijr8if135cq75ph-tree-c85ed5e98d6dd95e17d7efce8d73453086330466.drv'...

trying http://127.0.0.1:8080/ipfs/f01781114c85ed5e98d6dd95e17d7efce8d73453086330466?format=car
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   147  100   147    0     0   5817      0 --:--:-- --:--:-- --:--:--  5880
/nix/store/i81wxk1z7x09v85cmwb3lzgkndh2s0d3-tree-c85ed5e98d6dd95e17d7efce8d73453086330466
$ find result/
result/
result/AAA
$ nix-build --argstr tree a28a8e73ffa278c71485e91b30fd0182f9c5ab73 fetch-git-tree.nix
this derivation will be built:
  /nix/store/16zh26mp4xik42zhfl2h6ndks1pga67f-tree-a28a8e73ffa278c71485e91b30fd0182f9c5ab73.drv
building '/nix/store/16zh26mp4xik42zhfl2h6ndks1pga67f-tree-a28a8e73ffa278c71485e91b30fd0182f9c5ab73.drv'...

trying http://127.0.0.1:8080/ipfs/f01781114a28a8e73ffa278c71485e91b30fd0182f9c5ab73?format=car
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   209  100   209    0     0  19186      0 --:--:-- --:--:-- --:--:-- 20900
/nix/store/xhqs43bism8pb0068ylhvmqhg835g9qm-tree-a28a8e73ffa278c71485e91b30fd0182f9c5ab73
$ find result/
result/
result/AA
result/AA/AAA
$ nix-build --argstr tree 8e82045fc031685a8cc1205c299838401d22dd08 fetch-git-tree.nix
this derivation will be built:
  /nix/store/wlls925n38h7gzhafid2f528wqbym9rq-tree-8e82045fc031685a8cc1205c299838401d22dd08.drv
building '/nix/store/wlls925n38h7gzhafid2f528wqbym9rq-tree-8e82045fc031685a8cc1205c299838401d22dd08.drv'...

trying http://127.0.0.1:8080/ipfs/f017811148e82045fc031685a8cc1205c299838401d22dd08?format=car
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   270  100   270    0     0  33750      0 --:--:-- --:--:-- --:--:-- 38571
/nix/store/c0b6pag9s3n293w6h4m05i6hanjvnyzd-tree-8e82045fc031685a8cc1205c299838401d22dd08
$ find result/
result/
result/A
result/A/AA
result/A/AA/AAA
$ nix-build --argstr tree c00f6075c62addbd5a89b16c1d6c54b29793bbad fetch-git-tree.nix
this derivation will be built:
  /nix/store/n14q5ckr8i12q8iiyp60w3rl0ijyrx2p-tree-c00f6075c62addbd5a89b16c1d6c54b29793bbad.drv
building '/nix/store/n14q5ckr8i12q8iiyp60w3rl0ijyrx2p-tree-c00f6075c62addbd5a89b16c1d6c54b29793bbad.drv'...

trying http://127.0.0.1:8080/ipfs/f01781114c00f6075c62addbd5a89b16c1d6c54b29793bbad?format=car
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   275  100   275    0     0  11693      0 --:--:-- --:--:-- --:--:-- 11956
/nix/store/gjy7vn0yr18vmg5aadbjkk2l4z9zrb1a-tree-c00f6075c62addbd5a89b16c1d6c54b29793bbad
$ find result/
result/
result/AA
result/AA/AAA
result/AB
```

### Conclusion ###

Nix, IPFS and Git were built to solve different problems, but they involve
similar ideas (e.g. Merkle trees) so it's natural to try and make them fit
together in complementary ways. There is some work left to do, which is clear
from the widespread use of Nix functions like `fetchFromGitHub`, which avoid Git
altogether in favour of dumb HTTP fetching with separate (NAR hash) validation.
Hopefully features like `git-hashing` will lead to nicer workflows, and allow
integration with more systems which support Git's formats (like IPFS).

Whilst the experiments on this page certainly show promise, it's important to
point out some of their problems. In particular, the data we fetched from IPFS
was already present in our local Kubo node, which made the download trivial: to
make it work on remote systems, or via public gateways, we would probably need
to announce ourselves as providers of those blocks; this may become intensive
for those with many large repos, since a single snapshot of the contents can be
spread across many trees/blocks. Alternatively, we could make sure to gather the
required blocks into our IPFS node beforehand, e.g. using
[GraphSync](https://ipld.io/specs/transport/graphsync/); although that seems to
be deprecated.

Overall, I like the idea of having my Nix expressions identify their sources by
content ID rather than a location-based URL; for those content IDs to match the
existing storage method of their code repositories; and for Nix to verify their
contents without needing any further information. This seems much nicer than
[my previous approach](/blog/2017-07-22-git_repo_pages.html) of pushing Git
clones into IPFS via its UnixFS encoding, and using NAR hashes in all of my Nix
expressions.
