That's exactly what I was referring to as "trivial things".

Non-trivial tooling would be things like:

 - Logging results of all test runs to a queryable database (smolder seems to do
   this)

  - Bonus points for aggregating and tagging results from different systems,
    e.g. dev's laptops, CI servers, etc.

 - Integration/interaction with VCS:

  - Browsing/querying test results based on branch/commit/patchset

  - Identifying commits which introduce a pass/failure

  - Using blame info to identify which code affects which tests

  - Alerting the commiter and the test's author about failures

 - Integration/interaction with bug trackers

  - Associating tests with bugs and bugs with tests

  - Changing the status of bugs based on test results (e.g. fixed in dev, fixed
    in staging, fixed in master)

  - Opening bugs when tests start failing (after querying if there's a relevant
    bug already open)

 - Notification/alerting integration. This could be built on VCS and bug tracker
   integration, for committers, assignees, test authors, etc.

I'm of the opinion that automated testing is vastly under-utilised. It's mostly
used as a way to check software builds before they're pushed, which is fine; but
more philosophically, automated tests are falsifiable experiments about the
state of the world (although I would distinguish between "inline" checks, like
assertions and design-by-contract, which alter a system's behaviour; versus
standalone test suites, which measure and predict behaviour of other
systems). As a simple example, autoconf's compatibility checks are an automated
test suite; if they use TAP,

That's important for online systems, which are highly coupled (e.g. a service
might disappear without warning, or change its request format, etc.); but it's
also useful for simple things like autoconf-style compatibility checks.

partly because tests are stuck in their own silos without easy
inter-operability. TAP offers a basic improvement for this, but is in a bit of a
chicken-and-egg situation. The point of automated tests is to tell us when
something is broken.
