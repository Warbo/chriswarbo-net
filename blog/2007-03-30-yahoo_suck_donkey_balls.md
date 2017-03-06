---
title: Yahoo suck donkey balls
dependencies: [ 'static/file2img.sh', 'static/images/crapoo.png' ]
---
I've had it with Yahoo. They really do suck donkey balls. Why? Firstly they turned POP mail access into a "premium service", so people who want to access Yahoo's mail service have to use their proprietary, shitty (more on this later) web-browser-based interface, rather than a proper email program. This has forced me to use [FetchYahoo](http://fetchyahoo.sf.net) for the past few years, but this tool is a really kludgy hack to get around Yahoo's obfuscation of users' mail, and thus works with varying degrees of success. FetchYahoo is also only useful to dump my Yahoo mail into a local spool, so I can't send emails from within an email program unless I use my Gmail account via SMTP.

Therefore I am often forced to use said shitty interface. I use 2 Yahoo accounts (and one Gmail account, because Google don't do any of this Yahoo crap), so I have to log out of one before I can get into the other. This seems fair enough for a crappy web-browser based interface, because after all email has nothing to do with a web browser and shouldn't be in there at all. What does annoy me though, is that when I click "Sign Out"..... My inbox refreshes. That's right, Yahoo don't let me log out! Yahoo also uses cookies to keep me signed in, so I have to close the browser tab, delete my Yahoo cookie, then reload the page just so I can log in another account.

Also Yahoo's web design sucks really badly. Take a look at this:

```{.unwrap pipe="sh | pandoc -t json"}
./root/static/file2img.sh "Crapoo!" < ./root/static/images/crapoo.png
```

I don't know about you, but I can't tell the difference between `#ffffff` and `#ffffff` very easily, so I think it's a bad idea to put white text on a white background.

Why do I use Yahoo email at all you might ask. Well, like most proprietary software, Yahoo's web-browser-based email interface used to operate quite well when I got it, ie. I used to use an email reader via POP and SMTP, but then once the manufacturer had all of my data under their control they locked it down to only their own proprietary storage system (most likely built on Free Software components might I add, since I know Yahoo run their servers on BSD and Apache, and most storage/database things these days are some kind of MySQL or PostgreSQL, with PHP and things used to tie it together). So they have my email, and I have told countless people, companies and websites over the years to contact me through Yahoo's third party service, so Yahoo will keep receiving my data no matter how many times I pull it off their servers with FetchYahoo.

How about mail forwarding, so it gets sent to my Gmail account? Well, that would be great, but that is also a "premium feature" now, and I am not giving tribute to the Yahoo corporation in order for temporary release of my chains. No, I'm going to get my Freedom by my own means.

Why does Google = Freedom? Well, it doesn't. But if Google ever start hoarding my data to force me to look at adverts (which I block most of anyway) then I will just get a low power computer like an OpenBrick and keep it online 24/7, use DynDNS to give it a constant name, then I can use my regular user's mail account instead of relying on someone else to look after my data (and hope they are not breaching my privacy by reading it).

PS: I have contacted Yahoo about their web-browser-based email interface looking like a pile of incredibly pale shit, but apparently my setup is "not supported", so it looks like yet another corporation is propping up Microsoft's monopoly without offering a simple plan B for those customers they discriminate against (ie. letting me access my email via POP)
