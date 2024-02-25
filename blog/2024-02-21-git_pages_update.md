---
title: Git pages update
---

The [git pages on this site](/projects/repos) hadn't been updated in a while,
since I switched my hosting from EC2 to S3, and hence lost the ability to SSH
into them. I set up HTTP forwarding to
[my github account](https://github.com/warbo), which at least kept the repos
accessible, but their associated static pages were no longer being generated via
post-receive hook.

I've finally got around to implementing a new update process for those pages:
I've ripped all of the SSH commands out of [my pushGitPages
script](/git/warbo-utilities/git/branches/master/raw/pushGitPages.sh.raw.html),
and am now using [rclone](https://rclone.org) to mount the /git folder locally.
This has the advantage of not needing extra bare repos, like I had on my
Thinkpad (which is nice since my Pinephone doesn't have as much storage), whilst
not being too tied-in to AWS: I can easily replace the S3 mount with something
else, like an SSH mount on my LAN, if the need arises.

This also means that the "view source" links on recent pages of this site should
work. Paths which have been around for a while were still resolving, albeit to
an old commit; newer paths weren't resolving at all. Now they should all work 🤞
