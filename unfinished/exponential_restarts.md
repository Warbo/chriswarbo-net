---
title: Exponential Restarts
packages: [ 'matplotlib' ]
---

Let's say we have some task to perform, like winning a computer game. We don't
know how long it will take, but every second we spend playing the cake will get
us one second closer to having won the game. Seems pretty straightforward:

```{pipe="cat > head.py"}
#/usr/bin/env python3
import io
import matplotlib.pyplot as plt
import numpy             as np
import sys

plt.rcParams['axes.xmargin'] = 0
plt.rcParams['axes.ymargin'] = 0

```

```{pipe="cat > show"}
#/usr/bin/env bash
set -e
set +o posix  # Needed for process substitution

CODE=$(cat)
python3 < <(cat head.py; echo "$CODE")
B64=$(base64 -w0 < "$1.svg")
echo "<img src='data:image/svg+xml;base64,$B64' />" | pandoc -f html -t json
```

```{pipe="sh"}
chmod +x show
(source "$stdenv/setup" && patchShebangs .)
```

```{.unwrap pipe="./show line"}
xs = [x * 0.01 for x in range(300)]

plt.plot(xs, [x * 0.01 for x in range(100)] + [1 for _ in range(200)])
plt.xticks([0, 1, 2], ['0', 'T', '2T'])
plt.yticks([0, 1, 2], ['0', 'T', '2T'])
plt.xlabel(r'$B_1$')
plt.ylabel('Time taken')

plt.axhline(y=1., color='k', linestyle='dashed')
plt.axvline(x=1., color='k', linestyle='dashed')

plt.text(0.1, 2.1, 'Performing task')
plt.text(1.1, 2.1, 'Finished')

plt.savefig('line.svg')
```

Here $T$ is the time it takes to win the game, which we don't know in advance.
We also don't have any progress indicator: the end of the game comes as a
surprise.

Now imagine this task needs some equipment that we don't have; for example, the
game might need a particular console, or access to some online account. The
problem is, we need to arrange to use that equipment *up front*, according to
the following rules:

 - When we book the equipment, we need to specify how long we want it for.
 - We must wait-out *all* of the time we've booked; even after our task is done.
 - If it turns out that we didn't book enough time, we'll have to book some more
   time to try again.
 - Each attempt has to start from scratch (e.g. the game doesn't allow saving)

We'll start by booking $B_1$ secs: this is the number
we need to choose. If it turns out that $B_1$ is not enough time for our task,
we will need to book more time, which we'll call $B_2$ secs; if *that's* not
enough then we'll book $B_3$ secs; and so on for arbitrarily-high $B_N$.

We can use the following chart to help us decide on a value for $B_1$:

```{pipe="./show lowerbound"}
bestCase = lambda needed: lambda booked: \
  (needed + booked) if booked < needed else booked

T = 1.0
heights = lambda xs: list(map(bestCase(T), xs))

points  = lambda low, high: np.arange(low, high, 0.01)

tooSlow = np.arange(0., 1., 0.01)
enough  = np.arange(1., 3., 0.01)

id    = lambda x: x
const = lambda x: lambda y: x

area = lambda low, high, lower, upper, **kwargs: plt.fill_between(
  list(map(lower, points(low, high))),
  list(map(upper, points(low, high))),
  **kwargs
)

area(0., 3., const(0), const(1)       , color='green' )
area(0., 1., const(1), lambda x: x + 1, color='red'   )
area(1., 3., const(1), id             , color='orange')

plt.xticks([0, 1, 2], ['0', 'T', '2T'])
plt.yticks([0, 1, 2], ['0', 'T', '2T'])
plt.xlabel(r'$B_1$')
plt.ylabel('Time taken')

plt.axhline(y=1., color='k', linestyle='dashed')
plt.axhline(y=2., color='k', linestyle='dashed')
plt.axvline(x=1., color='k', linestyle='dashed')
plt.axvline(x=2., color='k', linestyle='dashed')

plt.text(0.1, 2.1, 'Performing task')
plt.text(0.1, 1.1, 'Wasted time')
plt.text(1.1, 1.1, 'Waiting time')

plt.savefig('lowerbound.svg')
```

The horizontal axis shows the amount of time we can choose to book $B_1$;
note that it is shown relative to $T$, which we don't know! The vertical axis
shows the amount of time it may take to complete our task, for each choice of
$B_1$ (also relative to $T$). A few features stand out:

 - The plot never goes below $T$. This is because, regardless of what we choose
   for $B_1$, we must wait $T$ secs for our task to finish.
 - The point ($T$, $T$) is the best case, where we book exactly the amount of
   time we need.
 - The diagonal line for $B_1 > T$ shows that booking *too much* time is
   wasteful, as we have to wait it out. The extra time is shown by the
   triangular region labelled "waiting time".
 - Once this line reaches ($2T$, $2T$) and above, *most* of the time will be
   spent waiting around after the task has finished.
 - When $B_1 < T$, the booked time is completely wasted and we'll have to try
   again. The overhead this adds to our total time is shown in the triangular
   region labelled "wasted time".
 - Retrying requires us to choose another time, $B_2$, which follows the same
   chart (except shifted vertically by the wasted time $B_1$). This gives a
   *range* of possible times, shows by the shaded region in the top-left.
 - When $B_1$ is close to zero, we've only wasted a small amount of time, so
   it's still *possible* to finish in around $T$ secs if we choose a good value
   for $B_2$ (equal to $T$, or slightly above). This is still wasteful, since we
   should try to choose a good value for $B_1$ and avoid the restart!
 - When $B_1$ is slightly below $T$, we've wasted the maximum possible time
   before restarting. Even if we pick a good value for $B_2$, we'll take twice
   as long as necessary.

Since we don't know $T$, the best we can do is minimise the amount of wasted
time. The worst case for restarting is when $B_1$ is *slightly* below $T$; to avoid this so we
want our guess The interesting dynamic is that when $B_1$ is *too small* it is *entirely*
wasted (requiring another attempt), but *bounded* (each retry wastes less than
$T$ secs); on the other hand, when $B_1$ is *too big* our task will complete
successfully (no retries needed), but there is *no limit* on how much time is
wasted (we could book a million years, for a task which only takes a few secs!).

We can solve this problem by splitting it into two smaller problems (this is
often called *induction*, or *divide and conquer*):

 - Choosing some initial guess: what should we pick for $B_1$?
 - Choosing how to handle restarts: if a guess $B_N$ was too small, what should
   we pick for $B_{N+1}$?

We'll start by tackling the second part. Our guess for $B_{N+1}$ should be as
big as possible, to minimise the chance of another restart; but we also want to
avoid the

Let's say we make a guess, which we'll call $G$:
This is a line when $B_1 ≥ T$, since
we will definitely finish at the indicated time; when $B_1 < T$ we have a shaded
area, since the total time depends on more choices ($B_2$, B_3$, etc.).

Note that the total time never goes below $T$, since that's how long our task
inherently takes; our decisions can only *add delay*.


Before we've made any choices, the quickest we can possibly finish our task
is within $T$ secs. We're unlikely to hit that best-case, so we need to compare
the cases of booking too much time and too little time. In particular:

 - When $B_N$ is too small, the fastest we can hope to finish is $T + B_N$ secs
   (our next attempt might only take $T$ secs, but we must add on the $B_N$
   we've already wasted). Hence if we're going to underestimate, it's better to
   *massively* underestimate:
    - The best we can hope for is that $B_N = 0 + \epsilon$ (where $\epsilon$
      represents some arbitrarily-small value). This would give a fastest
      possible time of $T + \epsilon ≈ T$
    - The worst that could happen is that $B_N = T - \epsilon$, where we came
      arbitrarily-close to finishing but didn't quite manage to. This would give
      a fastest possible time of $T + T - \epsilon = 2T - \epsilon ≈ 2T$
 - When $B_N$ is too big, we want our overestimate to be as *close* as possible
   to $T$:
    - The best we can hope for is $B_N = T + \epsilon ≈ T$
    - There is no bound on how long we may need to wait after the bad the worst-case scenario may be, since we're
      free to choose any ridiculously huge number we like for $B_N$

Although there is no bound on how long we may need to wait after, arge we can choose $B_N$ We see that something interesting happens around $B_N = 2T$

 - If $B_1$ is only *slightly* smaller than $T$, then fraction
 - If $B_1 ≥ 2T$ we could $\frac{B_1}{2}$

We can learn a bit more by using a common trick called *induction*: we imagine
that mathematics make some observations:

 -
The best case is when `B1 = T`, since our task will finish on our first attempt,
and we don't need to wait for any
The plot below shows how our choice of `B1` affects how fast we can *possibly*
finish our task (i.e. it shows a lower bound) plot a *lower bound* on the amount of

In the second case we need to book more time, which we'll call `B2`. We face the
same choice as before, except we now have some extra information: we know that
`B1` is not long enough. Hence we should choose `B2` to be longer than `B1`, but
how much longer? If `B2` isn't long enough, we'll have to book even more time as
`B3`, and so on for `B4`, `B5`, etc. until we eventually reach `T`.

We can pick the numbers `B2`, `B3`, etc. before we start (since we'll get no
progress information that could help us choose); yet this seems like a  How can we make *all* of these decisions? We can use a common trick from
mathematics and programming called *induction*: rather than having to pick all
of these numbers `B1`, `B2`, `B3`, etc. we instead potentially *unlimited* number We don't know how many times We need to pick each of can try to solve use a mathematical technique figure out what to do base need to decide two things:

 -
not enough has been wasted, since we  will need to try again, by booking more time `B2`, whic
We've had to wait for `T` secs (we don't need to use all of the time we
booked). We've had to pay for `B1` secs of equipment time complete. The total amount of If that turns out
to not be enough, we have to book more time, which we'll call `B2`. Note that we
can assume `B2 > B1`, since we have to start from scratch and know po we need to start again
How can we finish our task reasonably quickly, without paying for lots of time
we don't use?

Let's call the *actual* time required `T` (e.g. how long our cake *should* be in
the oven). The amount of time we *book* will be called `B1`; if `T > B1` we'll
need to try again, and will call the second amount `B2`; if that's not enough
we'll have to try again, which we'll call `B3`; and so on.

As per Levin Search. Show some graphs.

Drawback: takes twice as long to run, although it's still linear time.
Benefit: Works with up-front timeout.
