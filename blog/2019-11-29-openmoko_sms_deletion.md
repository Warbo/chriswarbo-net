---
title: Deleting SMS from SIM on OpenMoko
---

I had an annoying problem recently with my [OpenMoko FreeRunner](
https://en.wikipedia.org/wiki/Neo_FreeRunner#Neo_FreeRunner): I wasn't
receiving SMS messages, and the home screen was showing the confusing message
`Texto Plein`. I don't recall this ever happening before, despite using this
'phone for a decade.

I thought I'd write up how I solved this, to help anyone else who may encounter
this problem in the future (which, let's face it, is probably just me ;) )

## Identifying the Problem ##

The string `Texto Plein` didn't look English to me. Since my FreeRunner is using
the [QtMoko](http://qtmoko.sourceforge.net/) distribution (essentially
[Debian](https://debian.org) plus
[QtExtended](https://en.wikipedia.org/wiki/Qt_Extended)) I tried searching
Google for the query `"texto plein" qtmoko`. This gave exactly one hit, which is
[the source code of the theme](
https://github.com/RobertZenz/qtmoko/blob/master/etc/themes/faenqo/home.xml)
(yet another reason to use FOSS!).

Searching for `Texto Plein` in this file I saw these lines (which I've
cleaned up and indented):

    <text rect="0,0,0x9pt"
          align="hcenter"
          bold="yes"
          color="#ffffff"
          outline="#000000"
          transient="yes"
          active="expr:@/Telephony/Status/SMSMemoryFull > 0 &amp;&amp; @/Communications/Messages/NewMessages == 0">
      Texto plein
    </text>
    <text rect="0,50%,0x9pt"
          align="hcenter,bottom"
          bold="yes"
          color="#ffffff"
          outline="#000000"
          transient="yes"
          active="expr:@/Telephony/Status/SMSMemoryFull > 0 &amp;&amp; @/Communications/Messages/NewMessages > 0">
      (Texto plein)
    </text>

Ignoring all of the styling, the important part seems to be the `active`
attribute, which in both cases contains

    expr:@/Telephony/Status/SMSMemoryFull > 0

(Side note: one of the reasons I'm not partial to XML is that despite all of the
ceremony and hoop-jumping-through it requires, the benefits are often lost due
to the reliance on weird sub-languages contained in string attributes. It's a
"path of least resistance" for smuggling through structured data without having
to invent a verbose bunch of tags, schemas, etc. Even the XML standards
themselves are guilty of this, with XPath being a classic example. In this case
it's made even worse by the need to XML-encode the `&&`! I think s-expressions
make this less tempting, since they're already terse.)

From this, it looks like our SMS memory is full. Despite deleting all of the
messages in the QtMoko messaging program, the `Texto Plein` warning remained. It
turns out the real problem is that the message storage on my SIM card is full!

## False Starts ##

Looking through the QtMoko changelog, mailing list and bug tracker there are
some mentions of SIMs getting full; with different QtMoko versions opting to
delete from the SIM or not, as different users complain.

My QtMoko version was out of date, so I tried installing the latest version. It
didn't help.

I tried connecting to the modem via a serial terminal, and sending the
appropriate ["`AT` commands"](
https://www.diafaan.com/sms-tutorials/gsm-modem-tutorial/at-cmgd/). Whilst I
could talk to the modem and run certain commands successfully, the SMS deletion
commands gave errors.

## The Solution ##

One annoyance with QtMoko is that the Qt application takes over complete control
of the GSM modem, and as far as I can tell it doesn't have a simple commandline
or scripting method.

In contrast, the "official" OpenMoko operating systems used a much more
extensible/hackable approach: producing the
[freesmartphone.org platform](http://www.freesmartphone.org) which listens on
[DBus](https://www.freedesktop.org/wiki/Software/dbus) for
[specified commands](
http://www.freesmartphone.org/specifications). I installed the
[SHR](http://wiki.openmoko.org/wiki/SHR) system on to a spare microSD card and
used that to boot my phone.

After stepping through the first-run wizard, I set a root password, opened a
terminal to give myself a static IP address via `ifconfig` and logged in from my
laptop over a USB cable (this is *far* nicer than picking at keys on the
touchscreen, and also lets me use Emacs to run the terminal :) ).

It turns out that I didn't even need to mess with DBus directly, since SHR comes
with a handy (Python-based?) commandline program called
[`cli-framework`](http://wiki.openmoko.org/wiki/Cli-framework) which sets up all
of the boilerplate, allowing the calls from the `freesmartphone.org` spec to be
called directly:

```
$ ssh root@phone
 ____  _  _  ____        ___  __  ____  ____
/ ___)/ )( \(  _ \ ___  / __)/  \(  _ \(  __)
\___ \) __ ( )   /(___)( (__(  O ))   / ) _)
(____/\_)(_/(__\_)      \___)\__/(__\_)(____)
root@om-gta02:~# cli-framework
DBus Exception. dbus-hlid not installed
DBus Exception. dbus-hlid not installed
DBus Exception. dbus-hlid not installed
failed to connect to bluez
freesmartphone.org interactive command line
>>> dir()
[   'INTROSPECT',
    'MainLoop',
    '__builtins__',
    '__doc__',
    '__file__',
    '__name__',
    '__package__',
    '__version__',
    'atexit',
    'bluez_manager',
    'bus',
    'code',
    'console',
    'dbus',
    'dbus_hlid',
    'dbus_to_python',
    'devaudio',
    'devidle',
    'devrtc',
    'events',
    'framework',
    'frameworkiface',
    'getInterface',
    'getObject',
    'getObjectsForInterface',
    'gps',
    'gpsaccuracy',
    'gpsposition',
    'gpssatellite',
    'gpstime',
    'gsm',
    'gsmcall',
    'gsmcb',
    'gsmdata',
    'gsmdebug',
    'gsmdevice',
    'gsmhz',
    'gsmmonitor',
    'gsmnetwork',
    'gsmpdp',
    'gsmphone',
    'gsmrtc',
    'gsmsim',
    'gsmsms',
    'gsmtest',
    'handler',
    'leds',
    'mainloop',
    'muxer',
    'network',
    'os',
    'phone',
    'pim',
    'pimc',
    'pimcq',
    'pimm',
    'pimmq',
    'pims',
    'pp',
    'pprint',
    'preferences',
    'prettyPrint',
    'readline',
    'rlcompleter',
    'runmainloop',
    'sys',
    'testing',
    'timealarm',
    'types',
    'ubxdebug',
    'usage',
    'usageiface']
```

It looks like we want `gsmsim` which, according to [the spec](
http://www.freesmartphone.org/specs/org.freesmartphone.GSM.SIM), provides a
[`GetSimInfo`](
http://www.freesmartphone.org/specs/org.freesmartphone.GSM.SIM/#GetSimInfo)
method:

```
>>> gsmsim.GetSimInfo()
{   'imsi': '...',
    'issuer': 'unknown',
    'phonebooks': 'emergency aux:BD fixed aux:DC dialed received aux:LR aux:MT aux:AD contacts aux:SD missed aux:LM aux:AF own aux:UD',
    'slots': 10,
    'used': 10}
```

I've elided my serial number, but we can see I have `10` SMS `slots` in total,
and all `10` of them are `used`.

Now let's use the [`DeleteMessage`](
http://www.freesmartphone.org/specs/org.freesmartphone.GSM.SIM/#DeleteMessage)
method:

```
>>> gsmsim.DeleteMessage(1)
>>> gsmsim.GetSimInfo()
{   'imsi': '...',
    'issuer': 'unknown',
    'phonebooks': 'emergency aux:BD fixed aux:DC dialed received aux:LR aux:MT aux:AD contacts aux:SD missed aux:LM aux:AF own aux:UD',
    'slots': 10,
    'used': 9}
```

This seems to work! Let's do the rest (they turn out to count up from `1`):

```
>>> gsmsim.DeleteMessage(2)
>>> gsmsim.DeleteMessage(3)
>>> gsmsim.DeleteMessage(4)
>>> gsmsim.DeleteMessage(5)
>>> gsmsim.DeleteMessage(6)
>>> gsmsim.DeleteMessage(7)
>>> gsmsim.DeleteMessage(8)
>>> gsmsim.DeleteMessage(9)
>>> gsmsim.DeleteMessage(10)
>>> gsmsim.GetSimInfo()
{   'imsi': '234103410021965',
    'issuer': 'unknown',
    'phonebooks': 'emergency aux:BD fixed aux:DC dialed received aux:LR aux:MT aux:AD contacts aux:SD missed aux:LM aux:AF own aux:UD',
    'slots': 10,
    'used': 0}
```

We've freed up the SIM's memory, so now we can switch back to the QtMoko
microSD, and hopefully it won't happen again!
