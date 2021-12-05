---
title: Exponential Polling
packages: [ 'matplotlib' ]
---

## Waiting for Tasks ##

Let's say we start some task, like a software update, or we ask a friend to
run an errand. Ideally, we would get notified when the task is finished, which
we can represent in a diagram like this (with time increasing to the right):

```
Time →

┌Start                                    ┌Finished
┆                                         ┆
┢━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┪
┃Task                                     ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃Wait                                     ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┩
                                          ┆
                                          └Notified
```

We stop waiting as soon as the task has finished, since we get notified. If this
isn't possible, we will have to repeatedly check (or "poll") to see if the task
has finished yet. If we know roughly how long it will take, we can start polling
just before we expect it to finish, and try again and again until the task has
finished:

```
Time →

┌Start                                    ┌Finished
┆                                         ┆
┢━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┪
┃Task                                     ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╇━━┓
┃Wait                                     ┊  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━┯━━━━━━━━┯━━━━━┷━━┩
                           ┆        ┆        ┆
                           └Poll    └Poll    └Poll
```

In this example, we will only find out that the task has finished on our third
poll, since that's the first one to occur once the task has finished. This
causes us to wait slightly longer than necessary; but not too much, since we had
a good guess about when it might finish.

Both of those scenarios are common and useful; but we're going to consider what
happens in the unfortunate situation that we have *neither* a notification or a
good guess about how long it will take. In this case, the best we can do is poll
over and over to see whether it's finished yet.

The question we want to answer is, how often should we poll our task?

## Avoiding Wasted Time #

We can't guarantee we'll find out *immediately* after a task has finished (i.e.
that the `Wait` bar will equal the `Task` bar, in the above diagram), since that
would require polling *infinitely* often. Instead, we can require our polling
strategy results in a "reasonable" waiting time; but what counts as
"reasonable"? This depends on how long the task will take: for a task which
takes ten minutes, finding out the next day does not seem reasonable; yet that
might be perfectly reasonable for a year-long task.

Hence **there is no *fixed* polling interval which will work well for many
different situations**

Instead, we'll define whether a waiting time is "reasonable" *in terms of* the
time taken by the task; i.e. at the *relative* sizes of the `Task` and `Wait`
bars in the diagram above.

A particularly simple definition is to allow waiting up to twice as long as the
time taken by the task; in other words, the amount of "wasted" time after the
task has finished can be the same as the "productive" time before it finished.
This seems "reasonable", since it avoids *most* of the waiting time being wasted
(in the worst case, we would only waste *half* of the waiting time).

```
Time →

┌Start                                    ┌Finished
┆                                         ┆
┢━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┪
┃Task                                     ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃Wait                                     ┆                                         ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━┯━━━━━━━━┯━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┩
                           ┆        ┆        ┆
                           └Poll    └Poll    └Poll
```


the same amount of time before and after the
twice as long as the task takes. ensure we don't waste make these equal (that's valid since they are both time intervals). Hence for tasks which take ten minutes, we want
to find out they've finished within the following ten minutes (i.e. within
twenty minutes of starting the task); for tasks which take a year, we want to
find out they've finished withing the following year (within two years of
starting the task).

This definition of "reasonably soon" has a nice interpretation: most of the time
will be spent performing the task, rather than waiting. In the worst case, which
still fits this definition of "reasonably soon", we will end up waiting twice as
long as necessary: if the task took time `T`, we will spend another `T` waiting
until we poll it. after the same time
we will only find out that the task has finished
"reasonably soon" after the task has finished, then most of the time
case, where we find out as late as possible, we half of our time "wasted"

that case, for a task which takes ten minutes
it is "reasonable" to find out it's finished up to ten minutes later (i.e. we
should perform a poll some time between ten and twenty minutes after starting);
for tasks which take a year it is "reasonable" to find out they've finished up
to a year later (i.e. we should perform a poll some time between one and two
years after starting).

using a *formula*. We'll write `T` to
represent the time taken by the task, and `R` to represent the period of time
afterwards which still counts as "reasonably soon".

Notice that `T` and `R` both represent time intervals, so we can start by trying
the simplest possible formula:

```
R = T
```

This says once a task has finished, we'll find out about it "reasonably soon" as
long as we poll at some time between `T` and `T+R`

the task has finished, so we
can find the know when the task has finished

We want to ensure:

 - We don't waste too much of our own time checking it again and again. For
   example, checking something every minute would prevent us from going to the
   bathroom, making ourselves some lunch, etc.
 - Once our task has finished, we should poll it reasonably soon after. For
   example, if we only poll once per day, but the task only takes 10 minutes, we
   would have wasted 23 hours and 50 minutes!

The central question is what counts as "too long". This will vary depending on
how long the task itself takes: wasting a day is bad for a 10 minute task, but
reasonable for a task that takes a year.

Let's use `T` to represent the time taken by our task. We don't know what `T`
will be beforehand, but we want to ensure we don't wait longer than `2T` before
polling: any longer than that would mean *most* of our time was wasted.

Surprisingly, this simple constraint is enough to figure out a polling strategy!
In order to avoid waiting for `2T`, we need to consider the worst-case scenario,
which is when we poll *just* before the task finishes; i.e. after `T-ε`, for
some arbitrarily-small value `ε`.

In this case, *all* of time until the next poll will be wasted (except for `ε`,
which is negligible). At this point we've been waiting for `T-ε` (although we
may have polled multiple times so far); if we wait *longer* than this amount
before we next poll, our total waiting time will violate the `2T` constraint.

Hence to avoid violating the `2T` constraint, the time until the next poll
cannot be longer than the total time we've waited so far. We *could* poll more
often than this, but that effort might as well be spent doing something else
while we wait.

Going the other way, we find that having a poll at some time `t` requires an
earlier poll to occur at time `t/2`. On its own this leads to an *infinite*
amount of polling early-on, since we don't know how fast the task might finish;
hence any choice we make, no matter how small, might be more than `2T`
(this is similar to Zeno's Paradox).

We can prevent this unlimited amount of polling by choosing some fixed amount of
time until our first poll, which we'll call `P`. In this case we won't spot
anything until `P` time has elapsed, so we might end up wasting most of that
initial time (i.e. if the task finishes much faster than `P`). That's fine if we
choose a small amount of time, that's probably much shorter than required, and
we wouldn't mind wasting in any case. For many everyday examples we might choose
`P = 1sec` as our minimum time; if we're writing a low-latency computer system,
we might choose something smaller like `P = 1μsec`.

Our first poll will occur after this minimum waiting time, then the rest will
follow at successive powers of two, e.g. `P`, `2P`, `4P`, `8P`, `16P`, `32P`,
`64P`.

If we choose `P = 1sec`, we've reached a protasec within 7 polls (AKA a minute).
That gives an average time between polls of about 9sec, yet we will spot
faster tasks much earlier; e.g. a task that takes 3sec would be spotted by the
poll at 4sec, only 1sec later (wasting only $\frac{1}{4}$ of the overall time).

This pattern keeps going; the 13th poll occurs after a deftersec (AKA an hour);
the 18th poll after a day; the 23rd after the first month; and 27 polls is
enough to cover a whole year. This pattern of doubling can keep going as long as
we need, doubling the interval between polls each time, giving us a logarithmic
number of polls to perform, and exponentially-increasing time intervals to spend
doing other things.

```{pipe="python > /dev/null; pandoc -t json" .unwrap}
#/usr/bin/env python3
import io
import matplotlib.pyplot as plt
import numpy             as np
import sys

plt.rcParams['axes.xmargin'] = 0
plt.rcParams['axes.ymargin'] = 0

xs = list(range(24))
plt.plot(xs, [x**2 for x in xs])
plt.xticks(xs, ['0', 'P'] + [str(n) + 'P' for n in xs[2:]])
plt.xlabel('Poll number')
plt.ylabel('Time of poll')

plt.savefig('polls.svg')
```

```{pipe="sh | pandoc -f html -t json" .unwrap}
B64=$(base64 -w0 < polls.svg)
echo "<img src='data:image/svg+xml;base64,$B64' />"
```
