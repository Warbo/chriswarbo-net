---
title: Currying in Javascript
---
In Computer Science, "Currying" (named after Haskell Curry) is a method for specialising functions.

Let's say we had the following function, which takes 2 numbers and gives their product:

```javascript
var times = function(x, y) {
    return x * y;
};
```

We can study functions like this by using Alonzo Church's Lambda Calculus system. Church's system does not allow us to write a function which takes 2 numbers, like our `times` example above, but it turns out that this doesn't limit us. The reason is that, in Church's system, each variable (like `x`, `y` and `times` in our example) is available to any nested function definitions; we say it has *nested scope*. In other words, we can write the following definition for `times` and it would still work:

```javascript
var times = function(x, y) {
    var a_nested_function() {
        // This is a different function to "times", but since it is
        // declare *inside* "times", it has access to x and y
        return x * y;
    };
    return a_nested_function();
};
```

Some readers may spot that this makes `a_nested_function` a *closure*. With this technique in hand, it should be fairly obvious how we can rewrite `times` so that it doesn't take 2 arguments at once:

```javascript
var times = function(x) {
    // Here we have access to x
    return function(y) {
        // Here we have access to x and y
        return x * y;
    };
};
```

By rewriting our functions to take 1 argument at a time like this, we get a much more symmetric language: every function is called in the same way. For example, in the Haskell language we would run the original definition of `times` like this:

```haskell
width = 5
height = 12
area = times width height
```

We would run the 1-argument-at-a-time version (which I'll call "Curryable") like this:

```haskell
width = 5
height = 12
area = times width height
```

It's exactly the same! Compare this to Javascript, where the first version would be used like this:

```javascript
var width = 5;
var height = 12;
var area = times(width, height);
```

Whereas the Curryable version would be called like this:

```javascript
var width = 5;
var height = 12;
var area = times(width)(height);
```

Note that there are 2 sets of parentheses; this is because we're running the `times` function, which gives us back another function, then we're running that function. The result is completely equivalent to the first version, but Javascript makes a distinction, for no real reason.

Now, what's the point of writing our functions in this "Curryable form", you might be asking. Isn't it more cumbersome, with more boilerplate, for what I just admitted is a completely equivalent result? This is partly true. For the moment we'll ignore the issue of cumbersome, boilerplate code, since this is Javascript's fault and not inherent to the Lambda Calculus approach in general (for example, Haskell gets this right).

The real reason why we should write our functions in this way doesn't lie with cumbersome strings of function applications, like our `times(width)(height)` example; in fact it's the complete opposite! If we **don't** do all of those applications, what happens? Let's see!

```javascript
var my_times = times;            // With no calls, we just get a new reference to "times"
var double = times(2);           // With one call, we get a function with x filled in, but not y
var ten = double(5);             // Calling this new function once is the same as putting two calls after "times"
var ten = times(2)(5);           // This is the same as above
```

As you can see, giving all of the arguments to "times" may give equivalent behaviour, but we are also given a new opportunity: calling it with one argument! If we call a Javascript function of two arguments with just one, then Javascript assumes a value of 'undefined' for the second argument, and runs the entire thing accordingly. The major difference with our Curryable version is that calling it with one argument doesn't cause any assumptions and doesn't trigger any calculations; we get the opportunity to fill in some of the arguments now, and fill in the rest later. As you may have guessed, we say that the `double` function is `times` *Curried* with 2.

As a side note, I thought I'd point out another important property of Currying, since I've seen some Currying-in-Javascript attempts which get this wrong: Curryable functions do not 'accumulate' arguments, eg. by storing them in an array. Calling a Curryable function does not change it's behaviour. Rather, we *return new functions* which can access the arguments given so far. As an example of the difference, let's look at an 'accumulating arguments' function:

```javascript
var byte_reader = (function(length) {
    var bits = [];
    var reader = function(bit) {
        bits.push(bit);
        if (bits.length == length) {
            var result = parseInt(bits.join(''), 2);
            bits = [];
            return result;
        }
        return reader;
    };
    return reader;
})(8)
```

Here we have an anonymous function (also known as a "lambda", due to Lambda Calculus) which we call with the argument 8 immediately after defining it. This is so that the `reader` function inside is a closure which contains the `bits` array and the `length` value. The resulting function, that we call `byte_reader`, takes one argument and appends it to the `bits` array, then returns itself. Once the `bits` array reaches the given length, in this case 8, then it is returned as a number in base 2 and the array is reset. We can call it like this:

```javascript
byte_reader(1)(1)(1)(1)(1)(1)(1)(1);  // Returns 255
```

This may look very similar to this Curryable function:

```javascript
var byte_currier = function(a) {
  return function(b) {
    return function(c) {
      return function(d) {
        return function(e) {
          return function(f) {
            return function(g) {
              return function(h) {
                return a*128 + b*64 + c*32 + d*16 + e*8 + f*4 + g*2 + h;
              };
            };
          };
        };
      };
    };
  };
};
```

The difference is that we can call `byte_currier` and any of its inner functions whenever we like, in any order, and we will get the expected result. We can't do this with `byte_reader`, since it operates by spreading around references to itself and mutating them all with side-effects whenever any is called:

```javascript
var br_with_one = byte_reader(1);  // This is a reference to byte_reader. byte_reader now contains a 1
var bc_with_one = byte_currier(1); // This is a new function, which contains a 1

var br_with_zero = byte_reader(0);  // This is another reference to byte_reader, which now contains a 1 and a 0
var bc_with_zero = byte_currier(0); // This is a new function, which contains a 0

var br_one_to_go = br_with_one(1)(1)(1)(1)(1);  // This is another reference to byte_reader. It has 1011111
var bc_one_to_go = bc_with_one(1)(1)(1)(1)(1)(1);  // This is a new function with 1111111

var byte1 = br_one_to_go(1);  // byte1 == 127. byte_reader has reset itself, so all br functions now expect 8 bits
var byte2 = bc_one_to_go(1);  // byte2 == 255. All bc functions are unaffected.
var byte3 = bc_one_to_go(1);  // We can do it again if we like. byte3 == 255
var byte4 = bc_one_to_go(0);  // We can reuse our curried functions however we like. byte4 == 254.
var byte5 = bc_with_one(0)(0)(0)(0)(0)(0)(0);  // Our curried functions sit waiting to be reused, unaffected by whatever else we've done. byte5 == 128.
```

The Curryable function, and all of its Curried forms, always behave in the same, predicatablw way. The 'accumulating arguments' function, on the other hand, changes its behaviour after every call, and since we keep creating references to it, these changes can come from anywhere!

Now we've seen how Currying can be useful, I'll go back to the issue of cumbersome, boilerplate code. Surely the horribly-nested definition of `byte_currier` is proof enough that Currying in Javascript isn't worth it! If you are thinking this, then ask yourself what you normally do when some code is cumbersome and boilerplate? You refactor it into a function! Here's my best attempt so far:

```javascript
var c = curry = function(f) {
    // This is our curry function. It will make f Curryable

    var apply = function(args) {
        // Helper function.
        // Takes an array of arguments args, returns
        // f(args[0], args[1], ..., args[args.length - 1])
        return eval('f('+args.map(function(val, key) {
            return 'args['+key+']';
        }).join(', ')+')');
    };

    var add_args = function(old_args) {
        // Helper function.
        // Takes an array of arguments we've been given so far,
        // If they're enough for f then we run it.
        if (old_args.length >= f.length) return apply(old_args);
        // If not, we return a function to gather more.
        return function() {
            var new_args = [];
            for (var i = 0; i < arguments.length; i++) new_args.push(arguments*);
            return add_args(old_args.concat(new_args));
        };
    };
    // We kick things off by applying no arguments
    return add_args([]);
};
```

I've been using the above code in a stream processing library I'm writing, and it's given me no problems so far. It's nice and flexible, so you can use it on pretty much any Javascript function which takes at least 1 argument. I've bound it to the name `c` to make this really easy. The only note to make is that it only pays attention to named arguments; if you access the `arguments` array directly then it will act just like before, running with undefined values for any unsupplied arguments. For example:

```javascript`
var my_function = c(function(a, b, c) {
    // a, b and c are named arguments, so they're Curryable.
    // arguments[3] and arguments[4] are anonymous, so they'll
    // be ignored as soon as we supply a value for c.
    return a+b+c+arguments[3]+arguments[4];
});
```

The Curryable version will run once it has `a`, `b` and `c`. It will not wait for the 2 extra arguments that the code expects to find in the `arguments` array, and thus they will be undefined. However, the Curryable version (and any Curried versions you generate) will still work perfectly well as a multi-argument Javascript function. Thus you can run the above function in the regular Javascript way. For instance, these are all equivalent:

```javascript
my_function(1, 2, 3, 4, 5);
my_function(1, 2)(3, 4, 5);
my_function(1)(2, 3, 4, 5);
my_function(1)(2)(3, 4, 5);
```

As you can see, with this method you may as well make everything Curryable, since you can call it just like you normally would, **and** in some new ways. Note that, if for some reason you want to use Javascript's default-to-undefined assumption, you can still get the same effect by explicitly passing undefined as the value of those arguments. IMHO this is better anyway, since the behaviour is explicit in the code.

So how can we use this Currying ability? Basically in any situation where we call a function many times with many of the same arguments. By Currying the first few arguments, we get specialised functions for free! The following function signatures can give you an idea:

```javascript
var read = c(function(filename, length) {...});
var write = c(function(filename, to_write) {...});
var query = c(function(database, query_type, parameters) {...});
var set_text = c(function(dom_object, new_text) {...});
var send_request = c(function(method, url, parameters) {...});

var queue = read(queue_file);
var log = write(logfile);
var customers = query(customer_db);
var find_customer = customers('SELECT');
var notify = set_text(document.getElementById('notification-area'));
var get = send_request('GET');
var post = send_request('POST');
var poll_status = get('http://www.mysite.com/status.txt');
var send_status = post('http://www.mysite.com/set_status.php');
```

Happy Hacking!
