---
title: Deleting files
---

While trying to free up space on my NixOS installation, I ran into the following
situation.

There is a directory containing `n` files, which we can list in alphabetical
order. Some files are in use, so can't be deleted, but we don't know which ones;
we want to delete all of the files which aren't in use.

We can delete any set of files at once, but we pay a large constant cost. If any
file in the chosen set is in use, none will be deleted.

What strategy can we follow which will delete all of the required files, using
the fewest delete operations?

# Trivial Solutions #

If we try to delete all of the files at once, we use the fewest number of calls;
but we fail to delete *any* files if there is one or more in use, so on its own
this isn't actually a solution.

If we delete each file individually, we're guaranteed to remove all the required
files, but this fails to utilise any batching, giving us O(n) time.

# Smarter approaches #

One approach which leaps out as a programmer is a divide-and-conquer approach:
we try to delete the whole lot, and see if that works. If not, we split our list
of files into two halves and try again with those. This is elegant and
recursive.

In the worst case, every other file is in use: no grouping of neighbouring files
will work, so we end up deleting each one individually, which takes O(n) time as
mentioned above, but we also have an O(log(n)) factor as we split up the list
into smaller and smaller pieces, giving a worst time complexity of O(n*log(n)).

The best case is O(1), since it's the case where all files can be deleted, and
our first call does the lot.

I think this is the best approach, but would love to know if there's something
better!
