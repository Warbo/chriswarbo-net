Since I've now been working with ocPortal long enough to get to know its internals pretty well, and which bits are a joy to use and which aren't, I thought I'd write down a few thoughts about what I think works badly, which areas would benefit from attention, what features I'd like to see and what may be possible in the future. Due to ocPortal's nature, as a layer on top of stubborn databases, flaky libraries and inconsistent languages, which needs to offer flashy, dynamic, user-editable coolness in a reliable way, this list will inevitably include low-level and high-level details. My bias is, of course, on the internals, but I am also a Web user so I care about the user-facing parts too :)

The Good (the majority of ocPortal)

Before I get into a rant about the less brilliant parts of ocPortal, I thought I'd make it absolutely clear that these are specific examples picked out because of their annoyingness; they aren't anything like a representative sample of ocPortal as a whole. ocPortal contains loads of really cool features, which make it a really nice platform to code on.

Some examples of this are the "require" systems, which allow you to say "require_code('a')", "require_lang('b')", "require_javascript('c')" and "require_css('d')". This will tell ocPortal that, before your code runs, the page should have access to the code in 'a.php', the language strings in 'b.ini', the Javascript in 'c.js' and the style information in 'd.css'. The major problem this solves is that it's often a bad idea to include some dependency "just in case" it's not been included yet, because this can cause a load of errors about conflicts where the dependency tries to overwrite a previously included version of itself. With ocPortal, these headaches never occur. A similar feature is the "extra_head" function, which allows things (for example, raw Javascript, metadata tags, etc.) to be written into the page's <head></head> section at any point during page generation, rather than having to specify them all at the start.

A related feature is ocPortal's fallback system. Code requested by "require_code" will be taken from the "sources_custom" folder if possible, and if there's nothing which matches then ocPortal falls back to the regular "sources" folder. The same happens for themes, where the "templates_custom" folder of a theme will be checked first, falling back to the "templates" folder, then if there is still no match the "templates_custom" folder of the default theme is checked, and finally the "templates" folder of the default theme is used. The same applies to the "css_custom" and "images_custom" folders. Languages also use fallbacks, going from "lang_custom" of the desired language, to "lang" for the desired language, to "lang_custom" for the default (currently English) and finally to the usual "lang" folder for the default language (English). This makes it very easy to specify, for example, tweaks to a theme (in the theme's *_custom folders), specify a theme's standard behaviour (the theme's non-custom folders), specify new functionality available to all themes (the *_custom folders of the default theme) and keep the original, known-to-work ocPortal defaults intact (the non-custom folders of the default theme). The same is true for the sources and sources_custom, although a little magic here allows you to specify only the additions/changes you want to make, rather than having to make a complete copy of the file first.

ocPortal's abstractions are also nice to work with, namely the database and forum drivers. They are rather leaky and can easily be abused (for example dumping raw SQL into the parameters of a nice abstract function), but as long as this is avoided on the whole, then it can be a handy thing to keep in place when you're in a pickle and need a bit of a hacked solution. It's like the UNIX philosophy that if you take away people's ability to do stupid things, you also take away their ability to do clever things (otherwise known as "giving you enough rope to hang yourself ;) ).

There's a lot more I could write about cool features of ocPortal, like the Tempcode templating language, the hooks system, the AJAX workflows, etc. but I am writing this to talk about what *can* be done, rather than what is *already* done. Let's move on.

The Bad (things which aren't the best way to solve the problems they're for)

I mentioned in the above section that I could have gone on about ocPortal's hooks being a good thing. They are. However, they are also "bad" in that there is the potential for them to be so much more. OK, a little background: a lot of systems in ocPortal aren't really suited to being written in one big chunk, the classic example being site-wide search which has to include bits from every other system. To handle these cases, ocPortal has a feature called "hooks"; instead of putting the desired behaviour in a few strategic places, we instead put a call to every one of the hooks we have for that system. So for example, rather than having a large chunk of search code which will look in the galleries, the downloads, the forums, the news, the catalogues, the users, and so on, we instead say "grab all of the search hooks and run each one". Then, in the "hooks/systems/search" folder of sources (or sources_custom) we can put a new file to deal with galleries, one to deal with downloads, and so on. Since the search says "all of the search hooks", rather than some specific list, we can add new hooks into the folder whenever we like and they'll be used everywhere that the built-in ones are.

Hooks rely heavily on a methodology called "metaprogramming", where a language manipulates some text, adding bits on, replacing bits, etc. just like a regular program, but the text it's manipulating is actually code written in that language, which then gets run. Metaprogramming is really cool, but can get quite confusing quite quickly, so it's usually reserved for when nothing else is available. ocPortal hooks are created through metaprogramming as PHP objects, which are then asked to perform their task. Now, the problem with hooks is that this metaprogramming magic must be invoked each time we want to use a hook or hooks, and there's a lot of duplication in the hooks' contents.

When we design a system we usually try to keep related things together, but we inevitably end up having some similar code split up into various disparate places. With hooks, to me, the problem is that the search hooks have a gallery search. The introspection hooks have gallery introspection. The configuration hooks have gallery configuration. And so on. I think a better Object Oriented way to do this would be to generate large, amalgamated, Katamari-style objects to represent each part of ocPortal (galleries, downloads, catalogues, etc.) which a hook may want (each being defined via a hook, of course), and generate their methods from the current hooks (eg. gallery->search, downloads->search, downloads->introspection, etc.). This also makes it unnecessary to specify which hooks you want to instantiate before using them (an annoyance with the current model), as instead you can just ask ocPortal for a specific hook object (eg. 'search/galleries'), or all objects used by a set of hooks (eg. 'search'). This can be encapsulated behind a single function call like "require_object". Then the methods can be generated and called via proxy methods on the objects. For example if we ran "$galleries = require_object('search/galleries'); $galleries->search($params);" then the call to "search" is intercepted, a relevant hook is looked for and instantiated, then called with $params. That keeps down the overhead of having to generate objects with all hooks instantiated in them to start with. Performing a complete search of the site would be as simple as "$site_objects = get_objects('search'); $results = array(); foreach ($site_objects as $obj) { $results = array_merge($results, $obj->search($params)); }". We could even recurse through all of the hooks, bunging the methods into the generated objects. For example if we were writing part of the gallery system then we might access our desired hooks via "$objects = get_objects(); $search_results = $objects['galleries']->search($params); $relevant_configs = $objects['galleries']->configs();". Here I've used the convention that "hooks/foo/bar.php" will be at the key "bar" in the objects array, and will have the method "foo".

The Ugly (things which are unfortunate, and waiting to be fixed)

At the moment ocPortal has a few historic systems which aren't actually used. For example, there is infrastructure to create new configuration options. This is actually useless, however, since the configuration options are designated via hooks. Such implementation changes have left various cruft around, and this can be confusing for programmers as they wonder why calls to these things don't do as they expect.

Some parts of HTML and its kin are a little annoying, for which various workarounds can be used. Unfortunately, ocPortal uses Flash to work around some of these, for example the limitations of file uploading (a form must be submitted (ie. a new page loaded), no progress information is given, etc.). This is unfortunate because the only viable runtime for Flash is Adobe's (formerly Macromedia's) and this isn't Free Software. Another example is video and audio playing, where the pending HTML5 standard defines how to embed such things, but makes no requirements for the formats to use. The current formats most used are h.264, which gives high quality video, is supported by many proprietary vendors like Apple, but Free Software implementations of it are illegal in many countries due to patents; Ogg Theora, which is a medium quality codec (used to be poor, but has improved a lot recently) which has been the format of choice for Free Software for years, and is thus supported by browsers like Firefox easily; and WebM, a new format pushed by Google, which is patent-free (as far as we know) and supported by Free Software browsers. Until this settles down, it is difficult to make a HTML5 video site which will work on the majority of machines, without keeping at least 3 copies of each video (which is an awful lot of space) and potentially infringing patent law when converting to h.264. This unfortunately makes Flash a more widespread option than the standard, for the moment at least. It is only a matter of time before these things get replaced, but I would like to see it sooner rather than later, especially since I refuse to comply with Adobe's licensing terms.

The Future

These are ideas for where ocPortal can go. They're not a roadmap, they're not a vision, they're just things I've noticed and thought about, along with explorations of what new possibilities we would have if we implemented any of these.

WebDAV: WebDAV is a filesystem which runs over HTTP. On UNIX systems we're used to accessing everything via a file descriptor, and interfacing with all of this via some address underneath /. More recently, thanks to FUSE, we've become used to the idea of mounting filesystems which transparently encrpyt their contents, which transparently compress their contents, those which offer access to machines over a network, those which auto-generate contents from some source such as WikipediaFS, and so on. Now, spare a minute for those poor folk still stuck on legacy operating systems, which can't even handle ext2 by default. They can, however, mount WebDAV. That makes WebDAV, as a standard, a much better filesystem concept than an ocPortal-specific filesystem akin to WikipediaFS which very few users would be able to use.

By having ocPortal available via a filesystem like WebDAV, we can remove the barrier which prevents UNIX awesomeness from being unleashed on it. With an ocPortal filesystem, auto-generated and managed by PHP server-side code rather than being a direct representation of some disk's contents, we can allow users new ways to interact with a site (mount a site with your login credentials and have access to all of the files you're allowed to view, organised neatly into folders, which are populated automatically, and offer several concurrent hierarchies to choose from (eg. "images/dave/october" and "images/october/dave" could both generate the same results)), we can allow administrators new ways to manage their site (copying CSVs directly into and out of the site, dragging RSS in to post news, etc.) and we allow developers new ways to customise a site (direct manipulation of a site's source with native programming tools, with the changesets being available directly to the site's PHP).

This last point is what intrigues me the most. Since all writes to the site will be handled by PHP, we can do what we like with them. A nice way to handle them would be to have the whole site in a distributed version control system like Git, and have the changes saved as commits as they are made. This would let site admins roll back changes very easily, whilst also allowing changes to ocPortal sites to cross-polinate as users can pull changesets from each others' sites (if this is specifically enabled by the admin, of course). ocPortal change from being broadcasted from a restricted SVN repository to those sites which poll the central server; into being a true community of shared code, with no barriers to entry.

There would be no need to keep backups of things which are removed (like addons), since they can be restored from the site's revision history or pulled in from the ocPortal network. Indeed, the entire concept of addons can be restructured into Git changesets which, along with themes, can spread through the network without the need for a central repository. ocPortal.com would be a showcase of suggestions, rather than a key piece of infrastructure.

There are lots of services out there which would enhance various parts of ocPortal, even if they remain as separate servers. The obvious one is email. Forum threads should generate mailing lists and emailing the list should populate the correct forum (there are PHPBB mods which do this, but I've not tried them). All content, like blogs, news, galleries, etc. should be emailed out to those who want it (in the body if possible, or else as an attachment) whilst comments can be posted by sending a reply. Interacting with an ocPortal site should be, as far as possible, doable via email.

If a site is being hooked into an email server, then there should be the ability to auto-generate email accounts for users. This is less standardised, but some reference implementation could be written, eg. against SquirrelMail. This would only need to tie together the account credentials, it wouldn't need any interface work since a direct link to the mail frontend can be given.

The same goes for XMPP/Jabber. There is limited support in ocPortal at the moment for using XMPP in the chat rooms. I think this should be available as standard, such that every chat room is discoverable via XMPP service discovery on the site's URL. Going further, the site can offer content via Publish/Subscribe and allow all of the same kind of interactions that are possible via email.

A nice feature would be for ocPortal to seed its content via Bittorrent, offering Magnet links via the site and using the Distributed Hash Table o

Sprinkle more metadata around the site. Templates would become much more 
difficult to edit if RDFa were used, but we can put a separate rdf file, 
referenced in the head of the relevant pages. Profile pages should have a 
Friend-of-a-Friend rdf file for each user as a bare minimum. Generic concepts 
like "image", "video", etc. can have their type defined via a reference to 
DBPedia, along with a reference to the original URL of the content (rather 
than the page representing it). Put in relevant redirects so that, for 
example, no RDF concept has a URL which actually resolves to anything, unless 
that concept is a Web page. For the rest, add redirections based on the 
useragent type. For example, we may define a person as site.com/users/bob. 
That URL should never have an associated page. However, accessing the address 
with a browser should be detected and redirected to Bob's profile page. An RDF 
agent can be redirected to Bob's RDF document. Sites may have an RDF endpoint, 
but that's not too important.

An ocPortal account made on one site should work on another ocPortal site, if 
the admins agree to this. This could be OpenID, ie. scrap ocPortal-specific 
users in favour of everyone being identified by OpenID.

We should be able to email the site new news, blogs, images, etc. as well as 
send them in via Jabber.

We should allow sites to use client-side cryptography. Thus the site is merely 
a host; the user's browser does the encrypting via Javascript before 
uploading, and does the decrypting after downloading.

Hook into available metadata if something has it available. For example EXIF 
data in an image, semantic Web search on a URL. Display this alongside regular 
ocPortal fields.

Add OpenCollaborationServices support. Bit of a moving target, but would allow 
ocPortal sites to be used as sources in "Get Hot New Stuff" apps.

Look into PSYC for inter-server communications (eg. syncing).

Users from one ocPortal site should be able to message any member of another 
ocPortal site. Goes with the single-sign-on/OpenID and autogenerating 
email/Jabber accounts above.

ocPortal should have no dependency on Flash.

ocPortal should have no dependency on Javascript (ie. anything do-able with 
Javascript should be do-able without).

More coherent Javascript management. Currently Javascript works on DOM 
elements in a procedural way. Would be nicer to have an Object Oriented 
approach, where the objects are whatever the hell we want them to be, and 
there may be some DOM objects associated with an object if we like; the 
objects should sort out all of the logic amongst themselves.

The hooks system is OK, but if there are hooks for something then the old 
codepaths should be removed to prevent confusion. Would be nicer if we had 
dynamic objects which we can keep adding methods/properties to (even if it's 
via some awkward technique like mixins) rather than having to look up the same 
indices in a load of hooks. Don't know how this will work exactly yet.

There is a lot of interest in Diaspora, GNUSocial, StatusNet, ownCloud, etc. 
which are trying to be Free social networks or replacements for specific 
services. They have the ideas right, some have protocols, but none have any 
kind of decent sites coming out of their HTML generators. Could we usurp them 
by taking the existing awesomeness of ocPortal sites and making them support 
these protocols (like OStatus)? Would be nice, in that we'd get publicity, 
we'd get new features, plus everyone would get ocPortal as a kind of uber-
Diaspora/GNUSocial/StatusNet/ownCloud. CPAL terms are like AGPL, which helps 
the cause.

Should templating be overhauled? Do we want a text replacement system, or do 
we want a generative language? Do we want to work at the string level, or at 
the token level? What's wrong with XML? What's wrong with s-expressions? Would 
be nice to have the minimum amount of syntax possible (break the symmetry at 
the function/directive level, which is PHP anyway). s-expressions would be 
good for this. Similarly, if we want to overhaul templating, we could make 
quite a nice generative language for CSS with bugger all syntax and much more 
flexibility and extensibility. Would be a job to get working in a way that Web 
people would accept though.