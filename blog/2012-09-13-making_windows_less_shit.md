---
title: Making Windows Less Shit
---
At work I've got a dual monitor setup, with two 1080x1920 screens side by side,
and unfortunately I'm stuck on Microsoft Windows (or, if I'm being particularly
witty, micro$haft winblows).

To make the experience more bearable I'm using a tiling window manager called
PWT ([Python Windows Tiler](http://code.google.com/p/python-windows-tiler/));
this feels similar to dwm and co. on X, and has the fewest sore
points out of the Free Software tiling window managers for Windows that I've
tried. It crashes occasionally, but doesn't do anything persistently wrong when
it's running.

Since I'm not completely insane, and don't want to be, I of course don't do any
development on Windows. For that, I run a fullscreen Cygwin/X window across both
monitors and tunnel the display over SSH to a GNU/Linux box. This, however,
causes a problem with PWT: every time I switch desktop, PWT resizes Cygwin to
fit a single monitor.

I've looked through PWT's sources and it seems to rely on Windows' API for its
geometry information, which makes it tricky to change. As an alternative I've
come up with a script for [AutoHotKey](http://www.autohotkey.com") (an
excellent piece of Free Software for working around some of Windows' crap). I
have PWT configured to use Alt as its hotkey, and I keep Cygwin/X on the second
desktop. This means that Alt-2 (which AutoHotKey calls `!2`) switches to
Cygwin/X. The following script assigns a new hotkey for Alt-2 which forces
Cygwin/X to go full-screen again:

```
; "!2::" means "Run the following when Alt-2 is pressed"
!2::
    ; We've intercepted this Alt-2 keypress, but we still want PWT to switch
    ; desktop, so send another Alt-2 keypress (which won't be caught by us,
    ; since we aren't listening when we're in the middle of doing something)
    Send !2

    ; Now wait for the Cygwin/X window to become active
    WinWaitActive, Cygwin/X

    ; "restore" the window, if it's maximised
    WinGetActiveTitle, Title
    WinRestore, %Title%

    ; Get the display dimensions
    SysGet, X1, 76
    SysGet, Y1, 77
    SysGet, Width, 78
    SysGet, Height, 79

    ; Drag the Cygwin/X window to fill both monitors
    WinMove, %Title%,, X1, Y1, Width, Height

    ; Tell it to redraw itself
    WinSet, Redraw,, %Title%

    ; The X display doesn't realise that it's been resized, so we click the
    ; maximise button
    MouseClick, left, 2100, 10
    ; Then click the restore button. This forces it to redraw to the new size.
    MouseClick, left, 1040, 10

    ; Don't intercept any more Alt-2 presses until Cygwin/X becomes inactive
    WinWaitNotActive, Cygwin/X
```

I hope this comes in handy for someone :)

My other AutoHotKey uses are for automating some repetitive work processes, and
to remap the Caps Lock key to be a Control key.

What I really want now is a reliable way to stop Windows taking over the Super
key, so I can use it in PWT instead of Alt. At the moment I have some hotkey
clashes between PWT and Emacs :(
