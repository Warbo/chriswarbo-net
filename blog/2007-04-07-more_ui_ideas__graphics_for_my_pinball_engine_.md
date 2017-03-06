---
title: More UI ideas. Graphics for my pinball engine!
dependencies: [ 'static/file2img.sh', 'static/images/UpgradeIssuesMockup.png' ]
---
I've got my pinball physics thing somewhat working now, and have even given it some graphical output (which meant learning AWT :( ), which is nice. I have also got another couple of user interface ideas.

## Upgrade Checker

Based on this [Ubuntu Wiki spec](https://wiki.ubuntu.com/PainlessUpgrade) I came up with this as a quick mock up design:

```{.unwrap pipe="sh | pandoc -t json"}
./root/static/file2img.sh "" < ./root/static/images/UpgradeIssuesMockup.png
```

I don't know whether I will try and implement this myself or whether it will inspire someone else to. Of course if someone has a more elegant idea then I am happy for mine to be superceded.

## Software Installer

The idea behind this one is simple, namely that Gentoo-style "do this then do that" is often unneeded, since if one step always follows another, why not automate all of them into one step, since no decisions are being made? The classic one addressed here is the `./configure && make && make install` one for installing software from source. If most source compiles with those commands then why not automate them? I haven't got any GUI mockups yet, but I am thinking of linking apt-file into the `./configure` bit, to try and find needed files (perhaps this would require the user to copy `File not found: XXXXX.so` filenames into a box or something, since it would be tricky to parse the output of every configure script accurately looking for needed files). Obviously checkinstall would be used instead of make install, and perhaps a new directory could be made specifically for containing sources so they can be recompiled later when versions change (ie. automatically set required versions to equal, then if they are updated the tool would simply rebuild the package for the new version in the background (and make it version `X.X_custom2` or something). More to come, hopefully. I am working hard on this Java now I have something to show for it too.
