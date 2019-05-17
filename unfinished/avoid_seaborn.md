---
title: Avoid Seaborn
---

Python has some really nice libraries for drawing charts, like
[matplotlib](https://matplotlib.org). There are other libraries available, some
of which are built on matplotlib, and seaborn is an example of this. It offers a
few types of chart that don't come out-of-the-box with matplotlib (e.g. swarm
plots, which I wanted to use) as well as other things like colour schemes.

However, my advice is to **avoid seaborn**: using seaborn can easily lead to
silent data corruption (which, thankfully, I happened to spot by eye in my swarm
plot). By itself that is simply unfortunate: all software has problems, after
all. My issue is with the attitude of the developer: after I
[reported this issue](https://github.com/mwaskom/seaborn/issues/1409), along
with a minimal example of Python code to reproduce the problem and some
optional Nix code to reproduce the exact versions of Python, matplotlib,
seaborn, etc. that demonstrate the problem, they simply closed the issue with
some insulting remarks along the lines of "RTFM".

After commenting for clarification, asking whether the developer acknowledged
that data corruption is a serious problem for a scientific visualisation
library, regardless of whether this particular case is unfixable, etc. they
responded with more insults and "locked" the GitHub issue. I sent a followup
email, which was again met with insults.

My email promised not to contact them again, and I haven't. Some time after,
seaborn was brought up during [a discussion of Python visualisation libraries on
Hacker News](https://news.ycombinator.com/item?id=18469930). I left [a comment
recommending to avoid seaborn](https://news.ycombinator.com/item?id=18474971),
explaining the issue I had, the developer's hostility and refusal to acknowledge
that their software can easily misrepresent data without any warning, and how
that is a problem for the library regardless of whether the potential victims
were using it properly or not (that's what warnings are for, after all). I also
included a link to the GitHub issue, but kept the email private.

Today I also saw a mention of seaborn on [a lobste.rs
thread](https://lobste.rs/s/4ccc9b/from_gnuplot_matplotlib_pandas) and gave a
link to my HackerNews comments above. Checking the links, I found that the
developer has now *deleted* the GitHub issue (yet another reason to [avoid
external issue trackers!](/blog/2017-06-14-artemis.html)).

Hence I've now made this page, which nobody else can delete, which I can link
people to when recommending against seaborn. Unfortunately I can't find any
archived copy of the deleted GitHub issue text (another reminder that we should
strive to own our own data, rather than be held to ransom by external silos!); I
would appreciate if anyone who finds a copy [email it to me](/contact.html) so I
can include it here.

The following is what I've managed to recover of the original discussion from
the notification emails that GitHub sent:

> Date: Fri, 20 Apr 2018 06:45:40 -0700
> From: Michael Waskom <notifications@github.com>
> To: mwaskom/seaborn <seaborn@noreply.github.com>
> Cc: Warbo <chriswarbo@gmail.com>, Author <author@noreply.github.com>
> Subject: Re: [mwaskom/seaborn] Swarmplot shifting errorbars (#1409)
>
> Closed #1409.

> Date: Fri, 20 Apr 2018 10:02:50 -0700
> From: Michael Waskom <notifications@github.com>
> To: mwaskom/seaborn <seaborn@noreply.github.com>
> Cc: Warbo <chriswarbo@gmail.com>, Author <author@noreply.github.com>
> Subject: Re: [mwaskom/seaborn] Swarmplot shifting errorbars (#1409)
>
> > The plot has a single, absolute coordinate system: the axis labels.
>
> This is incorrect in several ways, the most important of which being that the
> tick labels are distinct from the tick values, which I think you are confused
> about.

> Date: Fri, 20 Apr 2018 13:45:39 +0000 (UTC)
> From: Michael Waskom <notifications@github.com>
> To: mwaskom/seaborn <seaborn@noreply.github.com>
> Cc: Warbo <chriswarbo@gmail.com>, Author <author@noreply.github.com>
> Subject: Re: [mwaskom/seaborn] Swarmplot shifting errorbars (#1409)
>
> Swarmplot positions are 0-based.


> Date: Mon, 23 Apr 2018 09:02:04 +0100
> To: mwaskom@nyu.edu
> Subject: Re: [mwaskom/seaborn] Swarmplot shifting errorbars (#1409)
>
> Hi Michael, I hope I'm not being too forward by replying to this issue
> via email. Don't worry, there won't be any further followups, since
> you're attitude has been quite effective in alienating me from ever
> attempting to contribute to this project again.
>
> I'm sorry if my attempt to describe this issue came across as obnoxious.
> That certainly wasn't my intention. Being a gatekeeper for such a
> widely-used project, especially one with a double-whammy of steep
> learning curve (programming API, rather than e.g. GUI) and non-expert
> audience ("non-programmers", e.g. students, scientists, etc.) I can
> imagine you've received a whole bunch of inept, entitled and maybe even
> abusive communications. I understand that could certainly jade one's
> perspective when receiving unsolicited bug reports from out of the blue.
> One problem with text-based communication is that it doesn't convey the
> author's tone: in this case what you inferred to be "yelling" and an
> "obnoxious rant" was nothing more (or less) than an attempt to describe,
> as precisely as I could, the issue I came across (as I understand it, at
> least).
>
> Technical communication can get quite tricky when most of the words
> necessary to describe the things involved (e.g. "plot", "line", "axis",
> "point", etc.) also correspond to particular concrete software concepts
> (e.g. the classes and methods in seaborn and matplotlib, which attempt
> to model those "real-world" things). Hence my rather elaborate attempt
> to make it clear that I was not using those terms to refer to the
> particulars of the current software implementation.
>
> This was not an attempt to get "free tech support", and whilst I know
> that such requests are all too common, it can be damaging to jump to
> such conclusions with flimsy evidence. In this case, I stumbled into a
> problem while trying to render a graph for a PDF, and worked around it
> by subtracting 1 from the errorbar's x coordinates. With that PDF
> finished (attached, in case you're interested!), there's no "tech
> support" to be had. This bug report and subsequent attempts at
> clarification came only from a sincere desire to understand, and
> possibly contribute in some small way, to a project that I had found
> very useful over the years.
>
> Once my PDF was finished, I thought I'd try (out of curiosity, a wish to
> learn more about the tools I use, and the hope to help others who may
> end up in the same situation) to figure out the "proper" way to describe
> such graphs (rather than mangling the data to "fake it"). After spending
> the better part of a day on this, and after asking for help in #python
> for a couple of days in a row without success (my freenode username is
> 'warbo', if you're interested in the logs), I concluded that either I'd
> stumbled into a bug/edge-case, or maybe I could prevent users in the
> future from sinking as much time into similar scenarios. At the very
> least I could maybe write a blog post detailing the problem I had and
> how to make it work.
>
> Hence why I decided to sink a little more time into this, by making a
> minimal working example and reporting the issue I faced, with as much
> detail as I could, in the hope that I would either be (unlikely) helping
> find a real bug or (much more likely) be finding the right way to handle
> the API in this case, and hopefully create something that will come up
> in Google for those who get stumped by a similar thing. As it turns out,
> I'm still none the wiser about which of these situations applies.
>
> To go to all this effort, only to be told to "spend some time trying to
> learn" has taken the wind out of my sails. Maybe this is a bug, maybe it
> isn't; I'm pretty sure it's at least a lack of documentation, but
> there's no way for me to patch that without actually knowing how to
> solve the issue (which I still don't). Either way, I can't bring myself
> to care about it any more. It's been somewhat eclipsed by the deeper
> issue of how hostile the project appears to be to sincere attempts at
> contribution. This is my attempt at 'bug reporting' the latter.
> Hopefully it has highlighted the problem, or maybe it will be seen as
> another "obnoxious rant". Either way, it's soured my opinion of the
> project enough to avoid any future interaction.
>
> > You've missed something basic, and I'm sorry about that
>
> Despite all of my efforts, both in playing with seaborn/matplotlib and
> trying to explain my conundrum in IRC, on github, etc. I still don't
> actually understand what it is that I've apparently missed. If it were
> something basic, then it's rather distressing that nobody in #python
> could figure it out either. I trawled through both the official
> documentation and a plethora of tutorials and blog posts before
> reporting, but to no avail. Plus I've been using seaborn for a few
> years, matplotlib for even longer, and python for well over a decade,
> and I still don't even know *where* this problem lies. It could be with
> the way the API is invoked, or the way that the components are
> interacting, or the way that the library is modelling the domain, etc.
> All I know is that drawing data with the same (x, y) coordinates in two
> different ways (swarmplot and errorbar), with everything else left as
> the defaults, results in an image which is objectively wrong, i.e. with
> marks appearing at two distinct locations. Based on the ticks and labels
> automatically chosen by the library, it is the errorbars/line which is
> wrong; but of course it may also be the case that the points and ticks
> are also wrong, and it's merely coincidence that the swarmplot happened
> to line up. In any case, and regardless of the reason (e.g. '0-based
> positions') this is data corruption, and it is silent: there are no
> warning messages, no exceptions thrown, no indication that anything is
> amiss. The impact is potentially large, since the user, and those
> reading their work, may draw incorrect conclusions (e.g. if eyeballing a
> red-shift). The only reason I noticed that my own plots were wrong is
> that I knew my data had a peak at position 13, yet the line in my plot
> peaked at 14.
>
> So despite spending days, with help from others, attempting to solve or
> track down this issue; despite describing in a bug report, as precisely
> as I could, the nature of the problem that I encountered; despite many
> years of experience as a user of this particular project, years of
> experience as a professional FOSS developer, and with a few years' worth
> of computer science PhD research under my belt, I am told that I have
> "missed something basic" and accused of being a freeloader.
>
> I dread to think of how similar interactions come across for those who
> are inexperienced, or where English isn't their native language. Perhaps
> many of the 'obnoxious' interactions which have thickened your skin
> were, like mine, genuine attempts to communicate a perceived problem.
>
> I hope, for the sake of the remaining users, that this might serve as an
> example that we're not all bad.
>
> --
> Kind regards, and thanks for your attention
> Chris Warburton


> From: Michael Waskom <notifications@github.com>
> To: mwaskom/seaborn <seaborn@noreply.github.com>
> Cc: Warbo <chriswarbo@gmail.com>, Author <author@noreply.github.com>
> Subject: Re: [mwaskom/seaborn] Swarmplot shifting errorbars (#1409)
>
> Just FYI, that obnoxious rant is not a very effective way to ask for free tech
> support. You've missed something basic, and I'm sorry about that, but I'm sure
> that if you spend some time trying to learn about the tool that you're using
> instead of yelling on the internet, you'll figure it out.

> From: Michael Waskom <mwaskom@nyu.edu>
> Date: Wed, 25 Apr 2018 10:05:59 -0400
> Subject: Re: [mwaskom/seaborn] Swarmplot shifting errorbars (#1409)
> To: Chris Warburton <chriswarbo@googlemail.com>
>
> It's clear that you do not value your time very highly. That's fine. But I
> would encourage you to consider how other people do value their own time,
> and to think about why you apparently feel so entitled to make wasteful
> demands on it.
