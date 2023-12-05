---
title: Dear World
---
Have you *STILL* not realised that [everything you ever put online is up for
grabs](http://dereknewton.com/2011/04/dropbox-authentication-static-host-ids/)?
Just because someone spouts some bullshit phrases like "privacy settings",
"passwords", "secret questions", etc. doesn't change this fact. What we do know
is:

1) It's not known if computer security is even [possible in
theory](http://en.wikipedia.org/wiki/P_versus_NP_problem).
2) Even if computer security turns out to be possible, then implementing it
correctly is still [a difficult
problem](http://en.wikipedia.org/wiki/Rice%27s_theorem).
3) Even if computer security is possible, and someone manages to implement it
correctly, you have no idea if they're lying unless you have
[full access to all parts of the code and
environment](http://www.gnu.org/philosophy/free-sw.html).
4) Even if computer security is possible, and someone manages to implement it
correctly, and this can be verified, then you sure as hell shouldn't give them
any stuff you care about, because they'll put in in a system that's
theoretically sound, verified, correct and proven to be able to stop you ever
getting it back. In short,
[you'd be fucked](http://ascii.textfiles.com/archives/1717).

Whilst all software fails the first test, and most software fails the second
test, Dropbox also fails the third and fourth. Don't use it, or anything like
it, for anything private. If you still want to upload stuff then there are
[plenty of Free Software
programs](https://web.archive.org/web/20110819130319/http://sourceforge.net/search/?fq[]=trove:251)
for doing so, ie. they pass tests 3 and 4.

Yes, I know that banks, credit card companies and the like send transactions
over the Internet, but that doesn't mean that doing so is secure. It means that
doing so is cheap. So cheap, in fact, that they can afford to pay for insurance
to cover [all of the fraud that happens on the system every
day](http://www.fraud.org/internet/intstat.htm).

Update: DropBox are now being [investigated for false
advertising](http://hardware.slashdot.org/story/11/05/15/2157202/Dropbox-Accused-of-Lying-About-Security).
What's the false claim they made? That they're secure. Specifically, DropBox
claimed that their employees cannot access any of the data stored in their
system, which turns out to be complete bollocks. The investigation is being made
since DropBox has provided a simple, quick, insecure system but presented it as
a simple, quick, secure system; whilst companies attempting to create real
secure systems, which inevitably end up less simple and quick than DropBox, have
been left in the dust by DropBox.

The moral of the story is to run your own clustered filesystem, or if speed
isn't your main concern then use FreeNet.
