---
title: Closures vs. First-class functions vs. Higher-order functions
---
This is a reply to <a href="http://notes-on-haskell.blogspot.com/2007/02/whats-wrong-with-for-loop.html">http://notes-on-haskell.blogspot.com/2007/02/whats-wrong-with-for-loop.html</a> which tries to make the point that closures are a good thing, but ends up not mentioning closures at all in the argument ;) My reply was too long for Blogger, so I've posted it here, since I think it's interesting on its own too.

As has been stated above, the article uses first-class and higher-order functions, without actually making use of closures.

The difference is that closures contain an environment, which can be modified by the code in the closure. Thus calling a closure over and over with the same arguments won't always give you the same results.

My Java's a little rusty, so I'll give some examples in Python. Let's say we want to sum a list of numbers. The first-class function approach, shown in the article, relies on the concept of a "sum" function for adding values, and a "reduce" function for walking the list:

```python
def sum(x, y):
    return x+y

total = reduce(sum,my_list)
```

The way we would approach this with closures would be to define an "accumulate" closure. This is like the "sum" function but instead of taking 2 arguments and returning their sum, it takes 1 argument and adds this to its own internal state. We can then use "map" to apply it to my_list:

```python
def make_accumulator():
    running_total = 0
    def acc(a):
        running_total += a
        return running_total
    return acc

accumulator = make_accumulator()

map(accumulator, my_list)

total = accumulator(0)
```

Python's scoping rules are a little weird, so I'll walk through this. First we create a first-class function object and call it "make_accumulator". Whenever this function is called, it creates 2 objects in its internal namespace; a number object called "running_total" and a function object called "acc".

Crucially, Python's name resolving works from the inside out: any code can access variables defined in a 'parent' namespace (as long as this name hasn't been overridden in the local namespace), but cannot access any local namespaces defined inside it (eg. the namespace of an inner function).

Thus "acc" has complete access to the "running_total" variable, and thus the function is free to increment running_total by whatever argument it is passed.

"acc" isn't yet a closure, since the body of "make_accumulator" is also free to change the value of running_total, although in our case it simply returns the "acc" function object.

Next we call "make_accumulator" and bind the function it returns to the variable "accumulator". Once "make_accumulator" finishes then the "acc" functions obtain complete control over running_total. If we don't bind the returned function, however, the "acc" closure just gets garbage collected and is useless. However, if we bind it, like we do to "accumulator", we end up with a function with internal state. (Note that calling "make_accumulator" again will create new, independent instances of "running_total" and "acc", so our closures retain complete control of their own instances of running_total).

With this closure in hand we then run every value of my_list through it using "map". This returns a list of each intermediate result, but we don't care about them (except for the last one) so they're discarded.

To recover the final result we call the closure again, but with an identity value (0 is the identity for addition). This gives us our result.

Note that closures don't have to return their state when called. For example we could make a closure that returns its argument, but negates it on alternate calls:

```python
def make_flipper():
    is_odd=True
    def flipper(x):
        is_odd = not is_odd
        if is_odd:
            return x
        else:
            return -1 * x
    return flipper

f = make_flipper()
print str(map(f,[0,1,2,3,4,5]))
```

This would output "[0,1,-2,3,-4,5]" (since -1 * 0 = 0)

Of course there are parallels to be made between closures (functions with internal values) and objects (values with internal functions ('methods')). It's been said that closures are a poor man's objects, and that objects are a poor man's closures. Still, they're another useful tool to have available, especially if they're done in a less clunky way than in Python (which seems like a useful side-effect of the scoping rules, rather than an explicit design decision).

**UPDATE**

I tend to ramble, and the above Python examples aren't the most elegant demos of closures, due to the clunky way Python defines closures (unless we use "lambda" of course!). Here are some Javascript examples, which I've used on live sites to great effect;

**Toggling**

Need to toggle something between on and off, open or closed, shown or hidden? Use a closure which captures a boolean:

```javascript
// Turns nested lists into arbitrary-depth expand/collapse menus

$('#list-container span').each(function() {
    // This is our encapsulated boolean. Note that each li gets its own.
    var open = true;  // Start open
    $(this).click(function() {
        // This is our closure
        if (open) $(this).next('ul').slideUp();
        else $(this).next('ul).slideDown();
        open = !open;
    }).click();
});
```

<ul id="list-container">
  <li>
    <span>Foos</span>
    <ul>
      <li>
        <span>Foo 1</span>
      </li>
      <li>
        <span>Foo 2</span>
      </li>
    </ul>
  </li>
  <li>
    <span>Bars</span>
    <ul>
      <li>
        <span>Bar 1</span>
      </li>
      <li>
        <span>Bar 2</span>
        <ul>
          <li>
            <span>Sub-Bar 1</span>
          </li>
        </ul>
    </ul>
  </li>
  <li>
    <span>Bazi</span>
    <ul>
      <li>
        <span>Baz 1</span>
        <ul>
          <li>
            <span>Sub-Baz 1</span>
          </li>
          <li>
            <span>Sub-Baz 2</span>
          </li>
        </ul>
      </li>
      <li>
        <span>Baz 2</span>
      </li>
    </ul>
  </li>
</ul>

<script src="/js/jquery.js"></script>
<script src="/js/underscore.js"></script>

<script type="text/javascript"> // <![CDATA[
// Turns nested lists into arbitrary-depth expand/collapse menus

$('#list-container span').each(function() {
    // This is our encapsulated boolean. Note that each ul gets its own.
    var open = true;  // Start open
    $(this).click(function() {
        // This is our closure
        if (open) $(this).next('ul').slideUp();
        else $(this).next('ul').slideDown();
        open = !open;
    }).click();
});
// ]]></script>

**Semaphores**

Need to share some variables between different bits of code? Use closures which encapsulate the same variables! Note that in this example, we make on/off buttons trivially by using exactly the same technique as the open/closed menus above. Closures usually compose nicely!

```javascript
// We have a list of filters and a list of items. Filters can be on or off. Items are associated with filters using class names.
// Each filter has many items, each item has many filters. If all of an item's filters are off, it is hidden. Otherwise it is shown.

(function() {
    // These semaphores count the number of filters that are on for each item
    var semaphores = {};
    $('#items li').each(function() {
        semaphores[this.id] = 0;
    });

    // The filter buttons all encapsulate the same semaphores
    $('#filter_list li').each(function() {
        var on = true;  // Encapsulated state of this filter
        var my_items = $('#items .'+this.id);  // The items which this filter affects
        // Bump up the semaphore count for these items now that they have a new filter
        my_items.each(function() {
            semaphores[this.id] = semaphores[this.id] + 1;
        });

        // Enable the filter action
        $(this).click(function() {
            if (on) {
                // We're on, so switch off
                $(this).css('opacity', '0.1');
                my_items.each(function() {
                    semaphores[this.id] = semaphores[this.id] - 1;
                    if (semaphores[this.id] === 0) $(this).slideUp();
                });
            }
            else {
                // We're off, so switch on
                $(this).css('opacity', '1');
                my_items.each(function() {
                    semaphores[this.id] = semaphores[this.id] + 1;
                    $(this).slideDown();
                });
            }
            on = !on;  // Toggle
        });
    });
```

<ul id="filter_list">
  <li id="boy">Boys</li>
  <li id="girl">Girls</li>
  <li id="tall">Tall</li>
  <li id="ginger">Ginger</li>
</ul>
<ul id="items">
  <li id="item_1" class="ginger girl">Sue is short and ginger</li>
  <li id="item_2" class="tall boy">Bob is tall</li>
  <li id="item_3" class="tall ginger boy">Gary is tall and ginger</li>
</ul>

<script type="text/javascript"> // <![CDATA[
// We have a list of filters and a list of items. Filters can be on or off. Items are associated with filters using class names.
// Each filter has many items, each item has many filters. If all of an item's filters are off, it is hidden. Otherwise it is shown.

(function() {
    // These semaphores count the number of filters that are on for each item
    var semaphores = {};
    $('#items li').each(function() {
        semaphores[this.id] = 0;
    });

    // The filter buttons all encapsulate the same semaphores
    $('#filter_list li').each(function() {
        var on = true;  // Encapsulated state of this filter
        var my_items = $('#items .'+this.id);  // The items which this filter affects
        // Bump up the semaphore count for these items now that they have a new filter
        my_items.each(function() {
            semaphores[this.id] = semaphores[this.id] + 1;
        });

        // Enable the filter action
        $(this).click(function() {
            if (on) {
                // We're on, so switch off
                $(this).css('opacity', '0.1');
                my_items.each(function() {
                    semaphores[this.id] = semaphores[this.id] - 1;
                    if (semaphores[this.id] === 0) $(this).slideUp();
                });
            }
            else {
                // We're off, so switch on
                $(this).css('opacity', '1');
                my_items.each(function() {
                    semaphores[this.id] = semaphores[this.id] + 1;
                    $(this).slideDown();
                });
            }
            on = !on;  // Toggle
        });
    });
})();
// ]]></script>
