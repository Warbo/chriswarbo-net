---
title: Useful info I learned from playing with GSM
---

**NOTE: This has been sat in my `unfinished` folder for a few years. It uses an
OpenMoko Freerunner, which I've now mostly replaced with a PinePhone.**

## General Setup ##

The following assumes serial access to a GSM modem. My phone runs Debian
([QtMoko](https://github.com/radekp/qtmoko)) and starts an OpenSSH server at
boot, so I've been accessing the modem directly from Bash running on the
phone. It's probably possible to do something similar from a PC via a USB cable
or Bluetooth, but I don't know how since I've not needed to. Likewise I don't
know whether serial access can be shared through non-standard Linux setups
(e.g. Android). I don't know or care about proprietary systems (e.g. iOS).

Once you've got serial access, write down the relevant device name somewhere
handy, since we'll be using it a lot. In my case this was `/dev/ttySAC0`.

## GSM Basics ##

GSM modems can be controlled by sending so-called ["`AT`
commands"](https://www.developershome.com/sms/atCommandsIntro.asp) over a serial
line.

Many programs can communicate over serial; I was happy piping `echo` commands
into `socat` (Debian package name `socat`). I also played with the more
specialised `gsmctl` program (Debian package name `gsm-utils`).

**Note:** One reason I was happy with `echo` and `socat` is that I use
`shell-mode` in Emacs to run my terminals. This integrates well with TRAMP for
SSH (tab-completion, opening remote files, etc.) and makes it easy to edit,
search, find/replace, etc. my previous commands and their output. I imagine that
a normal terminal would be more painful to use, in which case you might want to
look for something more than `echo` ;)

Since we're using serial, we might as well set the sending and receiving baud
rate to a conservative 9600, to rule out a potential source of problems. The
command I've been piping into is the following:

    socat - file:/dev/ttySAC0,crtscts,crnl,ispeed=9600,ospeed=9600

As well as setting the baud rates, this ends each line with a carriage return
and newline (AKA "carriage return linefeed", or CRLF). This is unlike
the default Unix/Linux convention of just using newline (AKA linefeed). The `-`
tells `socat` to read from its standard input.

To avoid spurious errors I've been turning the GSM modem off and on before
sending commands, and waiting 2 seconds between commands. The general script
I've been using looks like the following (I call it `go`), although this `/sys`
path is specific to the OpenMoko FreeRunner, yours will probably differ:

```
#!/bin/sh
set -e
echo "OFF"
echo 0 > /sys/bus/platform/devices/gta02-pm-gsm.0/power_on
sleep 2
echo "ON"
echo 1 > /sys/bus/platform/devices/gta02-pm-gsm.0/power_on
sleep 2
{
    for C in "$@"
    do
        sleep 2
        echo "SENDING $C" 1>&2
        echo "$C"
    done
    sleep 4
} | socat - file:/dev/ttySAC0,crtscts,crnl,ispeed=9600,ospeed=9600
```

Here `for C in "$@"; do ...; done` will loop through each argument of the script
and send them as `AT` commands to the modem.

## `AT` Commands ##

`AT` commands get their name from the characters `AT` that they begin with.
Later extensions begin with `AT+`. This prefix *must* be included when sending
commands!

**Note:** Some sites/listings (e.g.
[developershome.com](https://www.developershome.com/sms)) leave off the prefix,
saying things like "Syntax of the `+CMGD` `AT` Command". In this case the
bytes we should send over the serial line are *not* `+CMGD`, they are `AT+CMGD`.

Some useful things to know, that may not be immediately obvious:

 - Commands can be given in upper or lowercase. I stick to uppercase.

 - Sending the command `AT` on its own acts like a test; the modem should
   respond `OK`:

```
# ./go 'AT'
OFF
ON
^@^@^@AT-Command Interpreter ready
SENDING AT
AT
OK
```

 - Some commands require parameters. These are separated from the command using
   `=` and (if there are multiple parameters) from each other using `,`.

 - Parameters can often take the special value `?` which queries for the
   available values. For example, the `AT+CPMS` command tells the modem which
   "storage area" to use for SMS reading, writing and sending. We can query for
   the possible areas like this:

```
# ./go 'AT+CPMS=?'
OFF
ON
^@^@AT-Command Interpreter ready
SENDING AT+CPMS=?
AT+CPMS=?
+CPMS: ("ME","SM"),("ME","SM"),("ME","SM")

OK
```

Here we see `(...),(...),(...)` which tells us that there are three parameters.
Each parameter can either be `"ME"` or `"SM"` (including the quotes!). In this
case these represent the modem's storage and the SIM card, respectively.

 - We can choose between multiple levels of error verbosity using the `AT+CMEE`
   command. This can take values `0` (just say `ERROR`), `1` (give a numeric
   error code) or `2` (give a meaningful error message). We might as well set
   this to `2` every time, by editing our script to always send `AT+CMEE=2`

 - SMS can be set to [text mode or "PDU" (data)
   mode](https://www.diafaan.com/sms-tutorials/gsm-modem-tutorial/at-cmgf). Text
   mode is usually easier to understand, so we can have our script set that by
   default using `AT+CMGF=1`

Now try sending [some
commands](https://www.engineersgarage.com/tutorials/at-commands-gsm-at-command-set),
good luck!
