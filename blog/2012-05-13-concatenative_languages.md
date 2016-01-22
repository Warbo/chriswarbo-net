---
title: Concatenative Languages
---
I&#39;ve been looking into the rather pompous-sounding genre of <a href="http://en.wikipedia.org/wiki/Concatenative_programming_language" rel="" target="_blank" title="concatenative programming languages: (this link will open in a new window)">concatenative programming languages</a> recently. They are called &quot;concatenative&quot; (essentially &#39;joining together&#39;) because for any 2 programs A and B, the result of run(AB) is the same as the result of run(run(A)run(B)). This also works in reverse, so we can split any program A at any point; if we call the portion on the left side B and the right side C, then run(run(B)run(C)) = run(A). One consequence of this property is that we can keep splitting up our programs until we have one part for every available processor (which could be <a href="http://en.wikipedia.org/wiki/Graphics_processing_unit" rel="" target="_blank" title="a lot more: (this link will open in a new window)">a lot more</a> than you may think!), run them all at the same time one-per-processor, then concatenate the results and run again (perhaps also in parallel). This is a lot nicer than current mainstream parallel programming, which I&#39;ve had <a href="http://chriswarbo.net/git/gpu-simulations" rel="" target="_blank" title="first-hand experience with: (this link will open in a new window)">first-hand experience with</a>!<br />
<br />
This remarkable property of concatenative languages is due to them working in a subtley different way than more familiar languages. In most languages, we create self-contained bundles of usefulness called functions, and we <b>apply</b> these functions to things. For example, let&#39;s say we have a &quot;plus&quot; function and a &quot;square&quot; function, we can define &quot;the square of the sum of 2 numbers&quot; by making a function which takes 2 numbers as input, let&#39;s call them &quot;x&quot; and &quot;y&quot;, and uses the other 2 functions to create the output. We write this as &quot;&lambda;x.&lambda;y.square(sum(x)(y))&quot;. Because we apply functions to (explicit) inputs, we call these <b>applicative functions</b>.<br />
<br />
In concatenative languages, we don&#39;t have any (explicit) input. Instead of shuffling around values and applying functions, instead we don&#39;t have any values and the entirety of our program is the creation of one big function. Rather than applying our functions to arguments, we <b>compose</b> them together to form aggregate functions. Thus we say that concatenative languages use <b>compositional functions</b>. In contrast to our applicative example, &quot;the square of the sum of 2 numbers&quot; is simply &quot;sum square&quot;. It stands in contrast to the applicative case, since the actual numbers themselves don&#39;t appear in our definition, and rather than reducing down our hierarchy of functions through applying them, instead we&#39;re building up new, larger functions.<br />
<br />
Just as a Universal computing language is possible with just two applicative functions, known as <a href="http://en.wikipedia.org/wiki/SKI_combinator_calculus" rel="" target="_blank" title="S and K: (this link will open in a new window)">S and K</a>, along with parentheses; there is likewise a Universal computing language with just two compositional functions, known as Cake and K, along with quotes. Brent Kerby <a href="http://tunes.org/~iepos/joy.html" rel="" target="_blank" title="explains this quite nicely: (this link will open in a new window)">explains this quite nicely</a>.<br />
<br />
Whilst we can create these functions in languages like Cat and Joy, based on the existing library of functions built in to those languages, to me that defeats the point. Why use a multitude of complex functions to build a couple of simple ones? The converse is, of course, the reductive ideal: make an entire language using just these 2 functions. That&#39;s what I&#39;ve done. The language itself doesn&#39;t seem worthy of giving a name, so I&#39;ll just call it Stack.<br />
<br />
Stack programs are strings of the symbols &quot;k&quot;, &quot;c&quot; (the functions), &quot;[&quot; and &quot;]&quot; (the quotes). These get compiled into a Javascript function, which takes an array of functions as its input and produces an array of functions as output. These arrays are accessed in a &quot;last in, first out&quot; (LIFO) manner, which is why I&#39;ve called it <a href="http://en.wikipedia.org/wiki/Stack_(abstract_data_type)" rel="" target="_blank" title="Stack: (this link will open in a new window)">Stack</a>.<br />
<br />
Here&#39;s a Stack interpreter. Note that it&#39;s easy to get a stack underflow error, due to having too few functions on the stack. It&#39;s also possible to have too many levels of recursion, which causes Javascript to die. This second error is unfortunate, and may be worked around in future versions. Also note that there&#39;s no Currying, and thus the concatenative property doesn&#39;t hold in its entirety.<br />
<br />

<script type="text/javascript" src="/js/jquery.js"></script>
<script type="text/javascript" src="/js/stack.js"></script>

<form action="" id="stackform" type="get">
    <label for="text">Try typing some Stack here:</label>
    <br />
    <textarea cols="80" id="text" rows="30" type="text"></textarea>
    <br />
    <button id="run">Run!</button>
</form>
<script type="text/javascript"> // <![CDATA[
    $('#run').click(function() {
        var formatted = s(
            $('#text').val()
        )([]).toString();
        $('#output').text(formatted);
        return false;
    });
// ]]> </script>
<a id="output" style="border: solid white 1px; display: block;">&nbsp;</a>
