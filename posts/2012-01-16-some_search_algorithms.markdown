---
title: Some Search Algorithms
---
*Search* is, loosely speaking, a computer running backwards. Our most familiar example of search is keyword search engines on the Web, for example Google. What is it that Google does which constitutes search? At its heart, Google contains (a much more sophisticated version of) the following program:

```
get_keywords(url):
  page = download(url)
  text = strip_markup(text)
  words = text.split_at(" ")
  keywords = words.remove(get_common_words())
  return keywords
```

This pseudo-code says that to get the keywords for a given URL, we download the page at that URL, strip out all of the funky HTML markup to get the raw text, split up the text to get the words then throw away the common words to get the keywords. However, when we run a query through Google we don't want the keywords of a URL, we want the URLs that have our keywords. We want to run the above function backwards!

If we say that programs have the general form `y = p(x)`, ie. a program p turns xs into ys, then search has the form `x = search(p, y)`. Search turns ys back into xs, given the program p. In the case of Google, x are URLs, y are keywords and p is the get_keywords program above. By using search, Google can tell you which URLs are associated with which keywords.

So, how does search work?

It essentially boils down to trial and error, or as my secondary school Maths teachers would insist, trial and improvement. The reason our keywords-to-URLs searches are so accurate is because Google is running [b]billions[/b] of URL-to-keywords searches all of the time! These searches are carried out by programs called "bots", which run a program similar to `get_keywords` above; in addition to the keywords on a page, they also look for more URLs, then they run get_keywords on those URLs, and so on. Google store the results of these searches in a massive database (called BigTable) so that performing a keyword search becomes a simple matter of looking up those keywords' URLs from the database (of course, what made Google famous was also *the way they order these results*, using "PageRank").

Now, the obvious problems with this approach are the following: 1) What if there are URLs out there which are exactly what you're after, but Google hasn't finished reading their keywords yet? 2) How does this technique scale?

The answer to (1) is that there seems to be a social consensus that if it isn't on Google, it doesn't exist. In other words, Google slyly get away with skipping billions upon billions of URLs from their results (eg. the "deep Web") because their users follow the self-fulfilling prophesy of: If it's not on Google, it's shit; Google manage to index all of the good stuff.

Whilst Google have made a tidy sum by running URLs-to-keywords backwards, it turns out that search can apply to all kinds of problems. In fact, it's pretty easy to make a simple search algorithm that's generic enough to invert all kinds of programs! The trick, of course, is to do so as efficiently as possible.

I've been implementing a few simple examples that follow a rough gradient from simple & shit to sophisticated & elegant. You can check out the growing collection on my [page caption="Wiki"]cedi:misc:1/4/28/29[/page]. NOTE: You'll need Javascript enabled if you want to run them, and the graphics are SVG.
