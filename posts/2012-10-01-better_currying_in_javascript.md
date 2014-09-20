---
title: Better Currying in Javascript
---
Whilst indulging in some purely functional PHP, I came across an
improvement that can be made to our [beloved Curry function](http://chriswarbo.net/index.php?page=news&type=view&id=currying-in-javascript).

Let's say we have an `id`{.javascript}entity function, defined as follows:

```javascript
var id = c(function(x) { return x; });
```

This takes an argument and spits it back out. If we pass it a
function, we can call that function:

```javascript
id(function(x) { return 'Hello ' + x; })('World');
```

However, we've hit the annoying chained-parentheses issue that our
curry function tried to avoid: `foo(bar)(baz)`{.javascript}.

There's a trick to working around this: we should only ever
give a function as many arguments as it was defined to have.

Most implementations of currying will gather as many arguments as possible, then pass them *all* to the non-curried function they wrap, even if it's more than the non-curried function requires. That leads to the above behaviour.

Instead, we should only be passing the required number of arguments; if we're given more than that, we should keep hold of the rest until the non-curried function has returned. We can then pass these extra arguments
to the return value, under the assumtion that it's a function (if that assumption's wrong, it our caller's fault for giving us too many arguments).

This allows us to use a simple, single set of parentheses as the
standard interface for what could be quite convoluted stack
gymnastics; we hide the implementation cruft :)

Here's the improved Curry function (also using `Function.apply`{.javascript} rather
than `eval`{.javascript}, since the presence of `eval`{.javascript} makes static analysis
impossible):

```javascript
var c = curry = function(f) {
    // This is our curry function. It will make f Curryable

    var apply = function(args) {
        // Helper function.
        // Takes an array of arguments args, returns
        // f(args[0], args[1], ..., args[args.length - 1])
        var extra_args = args.slice(f.length);
        var result = f.apply(null, args.slice(0, f.length));
        extra_args.forEach(function(a) {
            result = result(a);
        });
        return result;
    };

    var add_args = function(old_args) {
        // Helper function.
        // Takes an array of arguments we've been given so far,
        // If they're enough for f then we run it.
        if (old_args.length >= f.length) return apply(old_args);
        // If not, we return a function to gather more.
        return function() {
            var new_args = [];
            for (var i = 0; i < arguments.length; i++)
              new_args.push(arguments[i]);
            return add_args(old_args.concat(new_args));
        };
    };
    // We kick things off by applying no arguments
    return add_args([]);
};
```

If we use this version of Currying, we can get rid of the
chained-parentheses:

```javascript
id(function(x) { return 'Hello ' + x; }, 'World');

var triple = c(function(x, y, z) { return x + y + z; });
id(triple, 'Hell', 'o Wor', 'ld');
```

Of course, we're free to use chained parentheses if we want!

```javascript
id(triple, 'Hell', 'o Wor', 'ld');
id(triple)('Hell', 'o Wor', 'ld');
id(triple, 'Hell')('o Wor', 'ld');
id(triple, 'Hell', 'o Wor')('ld');
id(triple)('Hell')('o Wor', 'ld');
id(triple)('Hell', 'o Wor')('ld');
id(triple, 'Hell')('o Wor')('ld');
id(triple)('Hell')('o Wor')('ld');
```

What do we lose by doing this? We must make all arguments explicit.
However, I see this as good style anyway.

Happy hacking!
