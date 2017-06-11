---
title: Arduino JSON Client
---
Arduino is a liberally licensed microcontroller system. Since I'm a fan of
Object-Oriented, duck-typed languages (EDIT: Wow, shows how long ago this was
written!) I find it pretty tedious writing C to control my Arduino. Thus I've
written this relatively simple Arduino "sketch" which polls the USB line for
input, processes it, and returns output over the USB. The format for this
communication is relatively verbose JSON, as I do not want to fill this with
unreadable, unmaintainable junk like hard-coded enumerations. Messages currently
take the following form:

```javascript
{"read": {"pin":4, "type":"digital" },
 "read": {"pin":2, "type":"analogue"},
 "write":{"pin":3, "type":"digital", "value":1}}
```

This sketch has been written with the goal of loading it onto my Arduino and
never having to replace it; ie. all of my Arduino programming can be done in a
more comfortable language like Python, and all pin usage is done over USB.

The code can be found in [Git](/git/arduino-json-client) and is in the Public
Domain.
