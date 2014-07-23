---
title: Looping in Javascript
---
At work I've become the go-to-guy when it comes to Javascript, mainly because I keep ranting about it. Recently I was asked "what's the best way to do a loop?". My answer was, quite simply, "use a loop". That's the answer to the question that was asked, but the real question that **should** have been asked was "what's the best way to solve this problem?", and in that case, the answer is never a loop.

I'll demonstrate this by "refactoring" a made up code example until we get a nice result. "Refactoring" is the practice of making small semantics-preserving changes, which on the whole improve a project. A piece of code has the same result before and after being refactored, but hopefully it has improved in other areas (performance, elegance, robustness, generality, etc.).

Our example will be the following, which makes heavy use of loops. It is a Javascript function which takes in an array of arrays of strings (eg. `[['hello', 'world'], ['I', 'am'], ['a', 'string']]`{.javascript}) and outputs a HTML table of these strings, eg.

```html
<table>
  <tr>
    <td>hello</td>
    <td>world</td>
  </tr>
  <tr>
    <td>I</td>
    <td>am</td>
  </tr>
  <tr>
    <td>a</td>
    <td>string</td>
  </tr>
</table>
```

```javascript
var tableify = function(values) {
    var cells = [];
    for (var i=0; i < values.length; i++) {
        for (var j=0; j < values*.length; j++) {
            cells.push('<td>' + values*[j] + '<td>');
        }
    }
    var row;
    for (var i=0; i < cells.length; i++) {
        row = '';
        for (var j=0; j < cells*.length; j++) {
            row += cells*[j];
        };
        cells* = row;
    };
    var rows = [];
    for (var i=0; i < cells.length; i++) {
        rows.push('<tr>' + cells* + '</tr>');
    }
    var all_rows = '';
    for (var i=0; i < rows.length; i++) {
        all_rows += rows*;
    }
    var table = '<table>' + all_rows + '</table>';
    return table;
};
```

I've made up the example, but I did so based on common Javascript usage; I haven't purposefully tailored the example to suit my argument. I think this kind of code is common due to the prevalence of C. Programming in C requires this imperative, looping style, and many modern scripting languages (Python, PHP, Perl, Javascript, Ruby, etc.) have been (at least early-on) designed and implemented by the same person, using C. In other words, we have a C programmer (who thinks in terms of imperative, looping code) designing the higher-level language; they'll inevitably bias the language towards imperative, looping code.

This perpetuation of imperative code doesn't have to continue forever. Most/all of the languages I just mentioned allow first-class functions. These are an excellent way to make code simpler, modular and composable (I'm increasingly of the opinion that object oriented code tries to solve the problem of arranging source code (like a module system), but doesn't do anything about simplifying).

Now let's refactor the above code. As I previously mentioned, the best way to do a loop is to use a loop; however, the point is that we should get rid of our loops. Our first change will split up this huge-list-of-statements style into some smaller functions. We use `forEach`{.javascript} for now, since there are still imperative side-effects everywhere which we must preserve:

```javascript
var tableify = function(values) {
    var cells = [];
    values.forEach(function(v) {
        v.forEach(function(c) {
            cells.push('<td>' + c + '<td>');
        });
    });
    var row;
    cells.forEach(function(v, i) {
        row = '';
        v.forEach(function(c) {
            row += c;
        });
        cells* = row;
    });
    var rows = [];
    cells.forEach(function(r) {
        rows.push('<tr>' + r + '</tr>');
    });
    var all_rows = '';
    rows.forEach(function(r) {
        all_rows += r;
    });
    var table = '<table>' + all_rows + '</table>';
    return table;
};
```

Now we have no loops, only functions. The "best way to do a loop" question has gone away, since loops aren't the best way to solve the problem.

Now that we have functions, arguments and return values, the next thing to do is get rid of the imperative aspects of this code. 'Imperative' code is code that relies on being executed in a particular order. Sometimes this is superficial and obvious, for example `x = 5; y = x * x;`{.javascript}, this clearly relies on `x`{.javascript} being calculated before `y`{.javascript}, so it's not particularly confusing; the ordering is inherent in the calculation being performed.

The problem arises when we have code like `var x = 5; fibble(x); x = 4; fobble(x);`{.javascript}. How can we tell if these statements are in the correct order? It's impossible, since we've used one variable for two pieces of data; in general, data is unknown until runtime, so this can cause all sorts of confusion.

The way we can avoid imperative code is by embracing the stack gymnastics of newly-created functions: every time we need to do a new job, we jump into the corresponding function with fresh arguments, and when we're finished we return the result and let everything else get garbage collected away.

The key to this is to replace the description of what to do (`forEach`{.javascript}) by a description of **what we actually want**. This is the real answer to the "what's the best way to do a loop?" question. A loop is 'what to do', not what we want.
Usually, what we want is either a transformed set of data (known as "mapping" the data), a value based on a set of data (known as "reducing" or "folding" the data) or to generate a data set based on a value (known as "unfolding" the value).

The key property of "fold", "unfold" and "map" is that they are not imperative; **they are not loops**. Mapping a set of data is a description of the result we want, it doesn't care how that result is achieved; it may be done with a loop, or it may be farmed out in parallel to a supercomputer. The same is true of folds and unfolds; they have a linear structure, eg. folding `[5, 10, 17]`{.javascript} with `+`{.javascript} and `0`{.javascript} gives `(0 + (5 + (10 + (17))))`{.javascript}, however we are still free to evaluate this in any way we like (with a loop, in parallel, etc.).

So, let's change our loops into maps and reductions:

```javascript
var tableify = function(values) {
    var cells = values.map(function(v) {
        return v.map(function(c) {
            return '<td>' + c + '<td>';
        });
    });
    cells = cells.map(function(v) {
        return v.reduce(function(r, c) {
            return r + c;
        });
    });
    var rows = cells.map(function(r) {
        return '<tr>' + r + '</tr>';
    });
    var all_rows = rows.reduce(function(a_r, r) {
        return a_r + r;
    });
    var table = '<table>' + all_rows + '</table>';
    return table;
};
```

This is already looking much cleaner, now that we've stopped messing around with implementation details and let the language take care of that for us. In fact, the code is now clean enough that we can spot some patterns in what it's doing. To make these more obvious, let's pull out the functions for a moment:

```javascript
var tableify = function(values) {
    var make_table_cell = function(content) {
        return '<td>' + content + '</td>';
    };
    var make_table_row = function(content) {
        return '<tr>' + content + '</tr>';
    };
    var make_table = function(content) {
        return '<table>' + content + '</table>';
    };
    var plus = function(x, y) { return x + y; };
    var join = function(xs) {
        return xs.reduce(plus, '');
    };

    var cells = values.map(function(x) { return x.map(make_table_cell); });
    cells = cells.map(function(v) {
        return join(v);
    });
    var rows = cells.map(make_table_row);
    var all_rows = join(rows);
    var table = make_table(all_rows);
    return table;
};
```

Our code is now a mixture of `function`{.javascript} calls, `map`{.javascript}s and `join`{.javascript}s (reductions). This has replaced all of the variable-juggling we had before, with arrays and indices and other nastiness. Our application-specific code (the bits which modify the strings) are in their own little `function`{.javascript}s at the top.

However, in this case we're actually redefining these functions every time we're called, then throwing them away when we return. We can make a simple change to refactor this, so that they're only defined once:

```javascript
var tableify = (function() {
    var make_table_cell = function(content) {
        return '<td>' + x + '</td>';
    };
    var make_table_row = function(content) {
        return '<tr>' + content + '</tr>';
    };
    var make_table = function(content) {
        return '<table>' + content + '</table>';
    };
    var plus = function(x, y) { return x + y; };
    var join = function(xs) {
        return xs.reduce(plus, '');
    };
    return function(values) {
        var cells = values.map(function(x) { return x.map(make_table_cell); });
        cells = cells.map(function(v) {
            return join(v);
        });
        var rows = cells.map(make_table_row);
        var all_rows = join(rows);
        var table = make_table(all_rows);
        return table;
    };
})();
```

Now, we can spot an obvious redundancy here: all of the `make_`{.javascript} functions do the same thing, just with different text. Since duplicate code = more chances to make a mistake, let's refactor this:

```javascript
var tableify = (function() {
    var maker = function(tag) {
        return function(content) {
            return '<' + tag + '>' + content + '</' + tag + '>'
        };
    };
    var make_table_cell = maker('td');
    var make_table_row  = maker('tr');
    var make_table      = maker('table');
    var plus = function(x, y) { return x + y; };
    var join = function(xs) {
        return xs.reduce(plus, '');
    };
    return function(values) {
        var cells = values.map(function(x) { return x.map(make_table_cell); });
        cells = cells.map(function(v) {
            return join(v);
        });
        var rows = cells.map(make_table_row);
        var all_rows = join(rows);
        var table = make_table(all_rows);
        return table;
    };
})();
```

This is still a bit crap, since we have to wrap our pretty standard code (`reduce`{.javascript} and `join`{.javascript}) in `function`{.javascript}/`return`{.javascript} boilerplate. Let's get rid of that. We also flip the arguments to our `reducer`{.javascript} functions, since having `function(element, accumulator)`{.javascript} makes things easier than `function(accumulator, element)`{.javascript}. The `flip`{.javascript} function just turns `f(x, y)`{.javascript} into `f(y, x)`{.javascript}:

```javascript
var tableify = (function() {
    var plus = function(x, y) { return x + y; };
    var flip = function(f) {
        return function(x, y) {
            return f(y, x);
        };
    };
    var reduce = function(f, z) {
        return function(xs) {
            return xs.reduce(flip(f), z);
        };
    };
    var maker = function(tag) {
        return function(content) {
            return '<' + tag + '>' + content + '</' + tag + '>'
        };
    };
    var make_table_cell = maker('td');
    var make_table_row = maker('tr');
    var make_table = maker('table');
    var join = reduce(plus, '');

    return function(values) {
        var cells = values.map(function(x) { return x.map(make_table_cell); });
        cells = cells.map(join);
        var rows = cells.map(make_table_row);
        var all_rows = join(rows);
        var table = make_table(all_rows);
        return table;
    };
})();
```

We can also do a similar wrapper for the standard `map`{.javascript} method:

```javascript
var tableify = (function() {
    var plus = function(x, y) { return x + y; };
    var flip = function(f) {
        return function(x, y) {
            return f(y, x);
        };
    };
    var reduce = function(f, z) {
        return function(xs) {
            return xs.reduce(flip(f), z);
        };
    };
    var map = function(f) {
        return function(xs) {
            return xs.map(f);
        };
    };
    var maker = function(tag) {
        return function(content) {
            return '<' + tag + '>' + content + '</' + tag + '>'
        };
    };
    var make_table_cell = maker('td');
    var make_table_row = maker('tr');
    var make_table = maker('table');
    var join = reduce(plus, '');

    return function(values) {
        var cells = map(map(make_table_cell))(values);
        cells = map(join)(cells);
        var rows = map(make_table_row)(cells);
        var all_rows = join(rows);
        var table = make_table(all_rows);
        return table;
    };
})();
```

If we use [the currying function](http://chriswarbo.net/index.php?page=news&type=view&id=currying-in-javascript) [I've blogged about previously](http://chriswarbo.net/index.php?page=news&type=view&id=admin-s-blog%2Fbetter-currying-in) then we can collapse these functions-of-functions and they become much nicer. There's a slight complication with `map`{.javascript} and `reduce`{.javascript}, since the native versions supply more arguments than we use, and the currying function will try to exhaust its argument list. The workaround is to wrap our callback functions so that the extra arguments are ignored (and we may as well inline `flip`{.javascript}, so I've done that too):

```javascript
var tableify = (function() {
    var plus = c(function(x, y) { return x + y; });
    var flip = c(function(f, x, y) { return f(y, x); });
    var reduce = c(function(f, z, xs) {
        return xs.reduce(function(acc, x, index, arr) {
            return f(x, acc);
        }, z);
    });
    var map = c(function(f, xs) {
        return xs.map(function(x, index, arr) {
            return f(x);
        });
    });
    var maker = c(function(tag, content) {
        return '<' + tag + '>' + content + '</' + tag + '>'
    });
    var make_table_cell = maker('td');
    var make_table_row = maker('tr');
    var make_table = maker('table');
    var join = reduce(plus, '');

    return function(values) {
        var cells = map(map(make_table_cell), values);
        cells = map(join, cells);
        var rows = map(make_table_row, cells);
        var all_rows = join(rows);
        var table = make_table(all_rows);
        return table;
    };
})();
```

Of course, now that we've got nicer interfaces to standard functions like `map`{.javascript}, `reduce`{.javascript}, `plus`{.javascript}, `flip`{.javascript} and `join`{.javascript}, we can pull them out into a library/prelude, since they're generic enough to be useful elsewhere. `maker`{.javascript} looks pretty useful for handling HTML, so I'll rename that to `tag`{.javascript} and pull it out too. Without these cluttering our code, we get:

```javascript
var tableify = (function() {
    var make_table_cell = tag('td');
    var make_table_row = tag('tr');
    var make_table = tag('table');

    return function(values) {
        var cells = map(map(make_table_cell), values);
        cells = map(join, cells);
        var rows = map(make_table_row, cells);
        var all_rows = join(rows);
        var table = make_table(all_rows);
        return table;
    };
})();
```

Now our code is much cleaner, but we can still make improvements. For example, we assign a value to `cells`{.javascript} and then reassign it to something else. The original code probably did this to break up a mass of unreadable code into more manageable chunks, but now that we've simplified everything, it makes sense to put this back into a single assignment, since this makes the code less imperative. While we're at it we'll get rid of the `table`{.javascript} variable, since we just return it immediately:

```javascript
var tableify = (function() {
    var make_table_cell = tag('td');
    var make_table_row = tag('tr');
    var make_table = tag('table');

    return function(values) {
        var cells = map(join, map(map(make_table_cell), values));
        var rows = map(make_table_row, cells);
        var all_rows = join(rows);
        return make_table(all_rows);
    };
})();
```

Now we're reaching the end of the "obvious" improvements, so we'll need to do a bit of thinking. There's a well-known optimisation when using a `map`{.javascript} function: if we have `map(f, map(g, xs))`{.javascript}, this means we traverse `xs`{.javascript} and apply the function `g`{.javascript} to each element, making a new array. We then traverse this new array and apply `f`{.javascript} to every element to yield the result. We can optimise this by composing the `f`{.javascript} and `g`{.javascript} functions together, which allows us to traverse the array once and it removes the need for the intermediate array. Let's apply this, assuming that our library/prelude defines `var compose = c(function(f, g, x) { return f(g(x)); });`{.javascript}:

```javascript
var tableify = (function() {
    var make_table_cell = tag('td');
    var make_table_row = tag('tr');
    var make_table = tag('table');

    return function(values) {
        var rows = map(compose(make_table_row, compose(join, map(make_table_cell))), values);
        var all_rows = join(rows);
        return make_table(all_rows);
    };
})();
```

Now we can rearrange our code a little. The functions we're defining here are purely for our own benefit, so we're free to play with them as long as we make equivalent changes to the way we call them. Let's use this freedom to move some of the complexity out of the application of our functions and into their definitions:

```javascript
var tableify = (function() {
    var make_cell = map(tag('td'));
    var make_table_row = compose(tag('tr'), compose(join, make_cell));
    var make_table = compose(tag('table'), join);

    return function(values) {
        return make_table(map(make_table_row, values));
    };
})();
```

Our function body is now one line! Now, in a similar way to our `map`{.javascript} optimisation, using `compose`{.javascript}, we can also use function composition with our `reduce`{.javascript} calls.

`reduce(f, z, map(g, xs))`{.javascript} will traverse the array `xs`{.javascript} and apply the function `g`{.javascript} to each element, making a new array which is then reduced element-wise with the function `f`{.javascript}.

The `f`{.javascript} function takes two arguments, the next value from the array and an 'accumulator' (which is `z`{.javascript} for the first element). Since we're using Curried functions, this is the same as saying that `f`{.javascript} is a function which takes a single element as an argument and returns a function which takes the accumulator as an argument. Note that Javascript's native `reduce`{.javascript} function passes arguments in the opposite order, but we put `flip`{.javascript} in our own version of `reduce`{.javascript} to get this nice result.

Having single-argument functions like this, we can see that composing `f`{.javascript} and `g`{.javascript} together with combine the `map`{.javascript} and the `reduce`{.javascript}!

I'll break this down into small refactorings, and demonstrate it for `make_cell`{.javascript} and `make_table_row`{.javascript}, since we have to undo a few things before we can restructure our reductions:

```javascript
// Previous definitions
var make_cell = map(tag('td'));
var make_table_row = compose(tag('tr'), compose(join, make_cell));

// Substitute "make_cell" into "make_table_row"
var make_table_row = compose(tag('tr'), compose(join, map(tag('td'))));

// Bring back "join" and "plus", for reference
var plus = c(function(x, acc) {
    return acc + x;
});
var join = reduce(plus, '');
var make_table_row = compose(tag('tr'), compose(join, map(tag('td'))));

// Compose "plus" with our "tag('td')" function, to make "plus_td", then
// use this to make "join_td", which we use to implement "make_table_row"
var plus = c(function(x, acc) {
    return acc + x;
});
var plus_td = compose(plus, tag('td'));
var join_td = reduce(plus_td, '');
var make_table_row = compose(tag('tr'), join_td);

// Send "plus" back to the standard library, and collapse down the new definitions
var make_table_row = compose(tag('tr'), reduce(compose(plus, tag('td')), ''));
```

Phew, got there in the end! Now our `make_table_row`{.javascript} function will only traverse its argument once, building up its HTML as it goes. Here's the full code so far:

```javascript
var tableify = (function() {
    var make_table_row = compose(tag('tr'), reduce(compose(plus, tag('td')), ''));
    var make_table     = compose(tag('table'), join);

    return function(values) {
        return make_table(map(make_table_row, values));
    };
})();
```

Now we can do a similar transformation to our use of `make_table`{.javascript}:

```javascript
// Previous version
var make_table = compose(tag('table'), join);
...
return make_table(map(make_table_row, values));

// In-line the definition
return compose(tag('table'), join)(map(make_table_row, values));

// Tease apart the composition
return tag('table', join(map(make_table_row, values)));

// Bring back "join" and "plus" for reference
var plus = c(function(x, acc) {
    return acc + x;
});
var join = reduce(plus, '');
return tag('table', join(map(make_table_row, values)));

// Compose "make_table_row" with "plus", to make "plus_tr", then
// use this to make "join_tr" and substitute this for the "map"
var plus = c(function(x, acc) {
    return acc + x;
});
var plus_tr = compose(plus, make_table_row);
var join_tr = reduce(plus_tr, '');
return tag('table', join_tr(values));

// Send "plus" back to the standard library, and collapse these
// definitions together
return tag('table', reduce(compose(plus, make_table_row), '', values));
```

Notice that we're calling `tag`{.javascript} from our function body, so it will happen at every call, rather than storing an intermediate version as `make_table`{.javascript}. We'll allow this for now, since we're not done yet. Our complete function now looks like this:

```javascript
var tableify = (function() {
    var make_table_row = compose(tag('tr'), reduce(compose(plus, tag('td')), ''));

    return function(values) {
        return tag('table', reduce(compose(plus, make_table_row), '', values));
    };
})();
```

Now we've composed all of our maps into our reductions, but we still need 2 reductions since our incoming array is 2D (rows-of-cells). Since we've done a lot of rearranging, let's clean this up a little. We'll start by collapsing it down to remove our current biases:

```javascript
var tableify = (function() {
    return function(values) {
        return tag('table', reduce(compose(plus, compose(tag('tr'), reduce(compose(plus, tag('td')), ''))), '', values));
    };
})();
```

We have no need for our outer function anymore, so we can get rid of it:

```javascript
var tableify = function(values) {
    return tag('table', reduce(compose(plus, compose(tag('tr'), reduce(compose(plus, tag('td')), ''))), '', values));
};
```

Our inner function is needed to accept the argument `values`{.javascript} and send it to `reduce`{.javascript}, which we can't call directly since it's wrapped in `tag('table')`{.javascript}. Right? Wrong! One function wrapped in another is exactly what `compose`{.javascript} is for, so we can throw away the inner function too! This leaves us with the following:

```javascript
var tableify = compose(tag('table'), reduce(compose(plus, compose(tag('tr'), reduce(compose(plus, tag('td')), ''))), ''));
```

Now we can split this up a bit to make it more readable; for example, there's a repeated pattern like this:

```javascript
reduce(compose(plus, f), '')
```

We can pull this out into its own function by rearranging it:

```javascript
// Previous definition
var flatten = function(f) {
    return reduce(compose(plus, f), '');
};

// Rewrite the parentheses to make the currying more obvious (this has
// no effect on the meaning)
var flatten = function(f) {
    return reduce(compose(plus)(f))('');
};

// Flip the arguments of "reduce"
var flatten = function(f) {
    return flip(reduce)('')(compose(plus)(f));
};

// Now reign in the parentheses again (this also has no effect on the
// meaning)
var flatten = function(f) {
    return (flip(reduce, ''))(compose(plus)(f));
};

// This is clearly a composition of "flip(reduce, '')" and "compose(plus)"
var flatten = function(f) {
    return compose(flip(reduce, ''), compose(plus))(f);
};

// Now we can get rid of our "f" argument
var flatten = compose(flip(reduce, ''), compose(plus));
```

This makes `tableify`{.javascript} look like the following (we wrap it in another one-shot function so we don't pollute the global namespace):

```javascript
var tableify = (function() {
    var flatten = compose(flip(reduce, ''), compose(plus));
    return compose(tag('table'), flatten(compose(tag('tr'), flatten(tag('td')))));
})();
```

Now we can tease out the `make_`{.javascript} functions from before:

```javascript
var tableify = (function() {
    var flatten = compose(flip(reduce, ''), compose(plus));
    var cell  =         tag('td');
    var row   = compose(tag('tr'),    flatten(cell));
    var table = compose(tag('table'), flatten(row));
    return table;
})();
```

This is now pretty concise. How about we move the `flatten`{.javascript} function out into our standard library, since it's a useful string function and not really HTML-related:

```javascript
var tableify = (function() {
    var cell  =         tag('td');
    var row   = compose(tag('tr'),    flatten(cell));
    var table = compose(tag('table'), flatten(row));
    return table;
})();
```

Hmm. There's still a pattern here: we have `compose(tag(...), flatten(...))`{.javascript}. Can we simplify that?

```javascript
var tableify = (function() {
    var collapse_with = function(tag_name, inner) {
        return compose(tag(tag_name), flatten(inner))
    };
    var cell  =           tag('td');
    var row   = collapse_with('tr',    cell);
    var table = collapse_with('table', row);
    return table;
})();
```

Looks promising! How about we clean this up a bit?

```javascript
// Previous definition
var collapse_with = function(tag_name, inner) {
    return compose(tag(tag_name), flatten(inner))
};

// Curry it
var collapse_with = c(function(tag_name, inner) {
    return compose(tag(tag_name), flatten(inner))
});

// If we look at the "inner" argument, we've got a composition
var collapse_with = c(function(tag_name) {
    return compose(compose(tag(tag_name)), flatten);
});

// Flip the outermost composition so that "tag_name" is on the end
var collapse_with = c(function(tag_name) {
    return flip(compose)(flatten, compose(tag(tag_name)));
});

// Now we have another composition! We can get rid of "tag_name"
var collapse_with = flip(compose)(flatten, compose(tag));

// There's no point flipping "compose" when we're giving it two arguments
var collapse_with = compose(compose(tag), flatten);
```

Let's put this back in our function, and clean it up:

```javascript
var tableify = (function() {
    var collapse_with = compose(compose(tag), flatten);
    var cell =           tag('td');
    var row  = collapse_with('tr',    cell);
    return     collapse_with('table', row);
})();
```

It may be worth pulling out the `collapse_with`{.javascript} function into a HTML library, since it's not table-specific. This may be seen as cheating, so I'll leave it here, but in a publically-accessible place in case I want to use it elsewhere:

```javascript
var collapse_with = compose(compose(tag), flatten);
var tableify = collapse_with('table',
               collapse_with('tr',
                         tag('td')));
```

Let's put the whole lot together, including the generic library functions we teased out as we went:

```javascript
// Function composition
var compose = c(function(f, g, x) { return f(g(x)); });

// Flip arguments from f(x, y) into f(y, x)
var flip = c(function(f, x, y) { return f(y, x); });

// Reduce an array of values into one value (wraps Javascript's reduce)
var reduce = c(function(f, z, xs) {
    return xs.reduce(function(acc, x, index, arr) { return f(x, acc); }, z);
});

// Wrap a string in a HTML/XML tag
var tag = c(function(t, c) { return '<' + t + '>' + c + '</' + t + '>'; });

// Wrapper for Javascript's +
var plus = c(function(x, acc) { return acc + x; });

// Reduces an array into a string, using a function from elements to strings
var flatten = compose(flip(reduce, ''), compose(plus));

// Concatenates together an array of strings, wrapping each in a given tag
var collapse_with = compose(compose(tag), flatten);

// Turns a two-dimensional array of strings into a HTML table
var tableify = collapse_with('table',
               collapse_with('tr',
                         tag('td')));
```

Let's compare this version of `tableify`{.javascript} to the original:

 - 1 line, compared to 48. 10 if we include library functions.
 - Our new code uses:
    - Function definition
    - Function application
 - Our original code used:
    - Function definition
    - Function application
    - Nested for-loops
    - Addition
    - Incrementing
    - Array construction
    - (Two-dimensional-) array-indexing
 - Each line in our new code clearly follows the last: we define a function then use it. If any lines are rearranged, it will complain about values being undefined. In contrast, the original jumps into and out of different blocks, performs loops-in-loops and is altogether very hard to follow. Despite this, we can rearrange a lot of it without signalling an error; but we will get corrupt data!
 - Our new code can be parallelised trivially, whilst the original is clearly a serial algorithm and far too fragile to pull apart automatically.

Some points to notice about this code:

 - Our `tablify`{.javascript} function is evaluated immediately; we're not hiding anything inside a function definition. The currying wrapper around all of our raw functions builds up arguments, and calls its wrapped function as soon as it has enough. This means that everything will be evaluated as soon as possible; anything that can be evaluated as part of the definition will be, anything that must wait until call-time will. Javascript will sort this out for us.
 - We've gone from for-loops, to forEach functions, to map function, to reduce functions, and now we don't seem to have any loops at all. We could be rather Zen about this and say that, after all, the best way to do a loop is to not do one at all!
 - This code is in a style called "point-free", which can lead to very terse (although sometimes unreadable!) code.

Whilst it can be difficult to write code directly in a point-free style, I find it useful to write code in a less terse way, then move it closer and closer to point-free. By doing this, we remove redundant cruft and reveal more of our problem's real structure, making it much clearer what's really involved. Once we've fished out the real solution hidden amongst our original, naive code, we can clean it up now that we know what we're doing.

Do you notice how the vast majority of the code we started with had nothing to do with the problem? Most of it was trying to deal with the mess caused by other parts. Looking at the final definition, it's obvious what's going on: collapse our argument into a table, transforming each element by collapsing them into rows, transforming their elements by tagging them as cells. It's obviously right, since it *feels* so right. How could something so elegant be wrong?
