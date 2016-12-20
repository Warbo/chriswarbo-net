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
files, but this is the largest number of deletionwe get a linear cost. In fact, we *must* call delete individually for
each
