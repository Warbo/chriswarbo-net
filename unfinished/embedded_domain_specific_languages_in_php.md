---
title: Embedded Domain-Specific Languages in PHP (AKA Scrap Your Dependency Injection)
---

There are few programming constructs as useful as the function; we can define all kinds of useful functions to make our code simpler, shorter, safer and easier to test (most of the functions in this tutorial are one-liners!). Unfortunately some languages, notably Java, don't have any support for functions (until Java 8, at least). This has lead to hacks and workarounds, known as "design patterns", proliferating through the world of software development, infecting all manner of codebases.

The problem with design patterns is that they're 'opt-in', ie. they usually require programmers to sprinkle repeated bits of code ('patterns', for example constructing 'null objects') throughout their project in order to get any benefit. It's easy to omit a pattern by mistake, especially when inheriting someone else's code. This not only makes the pattern less powerful (eg. your return value won't be null-checked) but it can also bring down the whole house of cards (eg. if the code only handles null objects, a real null could bring the whole thing down).

Functions, on the other hand, are designed precisely to *get rid* of repeated patterns in code (for example, we can handle nulls automatically with [maybe](/blog/2012-09-11-perhaps__perhaps__perhaps.html)). Also, since functions are black-box entities, we can use them to encapsulate anything we don't want the rest of the project to see, making it impossible to accidentally bypass the architecture (classes try to do this, but if you've ever used Python or Javascript you'll know that classes are just a limited kind of function).

Thankfully, most scripting languages have pretty good support for functions, but nevertheless seem to have caught a bad case of design pattern. To put this right, and hopefully save some codebases from ruthless obfuscation, I thought I'd write a tutorial on some useful functions within the context of the widely (ab)used language PHP. The motivating use-case for this tutorial will be to separate our business logic from potentially-dangerous procedure calls, so that our codebase is easier to test and debug.

The approach we'll take, which is common in functional programming, is to define *values* which *represent actions*. Rather than, for example, performing an SQL query, instead we'll return a *request to perform an SQL query*. How might we represent such requests? Well, the most general-purpose approach is to represent them as *programs* in a *domain-specific language* (DSL). There are two kinds of DSL: some are standalone, for example SQL, which are very limited in what they can do (which is why we have stored procedures, for example); others, known as embedded domain-specific languages (EDSLs) live inside another 'host' language, which we can use for anything the DSL can't handle.

We'll write our business logic in some EDSLs hosted by PHP. We can then interpret these languages however we like; crucially we will define *multiple interpreters* and give ourselves the choice of which one to use. One may perform the requested action (eg. querying a database), another might just check the actions as part of a test ("was our SQL injection attempt foiled?"), yet another might log the actions (for example, to profile which data is the hottest), and so on.

## Some Utilities ##

Before we begin, I want to use the following functions. They're all generic and useful, it's just unfortunate that PHP doesn't have them built-in:

```{pipe="tee tag > /dev/null"}
echo "<?"
cat
```

```{pipe="tee append > /dev/null"}
#!/bin/sh
echo "" >> f.php
tee -a f.php | ./tag
```

```{pipe="sh > /dev/null"}
chmod +x tag append
```

```{.php pipe="./append"}
// Identity function: returns its argument unchanged
function id($x) { return $x; }

// Since "array" isn't actually function :(
function array_() { return func_get_args(); }

// Function composition: apply $f to the return value of $g
function compose($f, $g) {
  return function($x) use ($f, $g) {
    return $f($g($x));
  };
}

// Partial application: supply some of $f's arguments now and some
// later. This is a quick alternative to currying.
function apply($f) {
  $args = array_slice(func_get_args(), 1);
  return function() use ($f, $args) {
    return call_user_func_array($f, array_merge($args,
                                                func_get_args()));
  };
}
```

## Mapping ##

If we're going to make some languages, the first thing to consider is the *tokens*, ie. the individual commands of the language. Tokens form a structure known as a *functor*, which we can think of as a 'box'. The defining feature of a functor is a *map* function which applies a given function to the contents of a box (if any). This is a bit abstract, but some concrete examples will show you how trivial this is:

Arrays are 'boxes', so we can apply a function to their contents:

```{.php pipe="./append"}
function map_array($f, $x) {
  $result = [];
  foreach ($x as $key => $value) {
    $result[$key] = $f($value);
  }
  return $result;
}
```

<div id="sma" class="hide odd">

#### A simpler map_array ####

  <div id="sma_box">

PHP already has this function built in, called [`array_map`](). I swapped the name around to prevent conflicts, but we can simply do this:

```{.php pipe="./tag"}
function map_array($f, $x) { return array_map($f, $x); }
```

Or even:

```{.php pipe="./tag"}
$map_array = apply('array_map');
```

  </div>
</div>

Objects are 'boxes' too, so we can apply a function to their properties:

```{.php pipe="./append"}
function map_object($f, $x) {
  $result = new stdClass;
  foreach (get_object_vars($x) as $name => $value) {
    $result->$name = $f($value);
  }
  return $result;
}
```

<div class="hide odd" id="smo">

#### A simpler map_object in terms of map_array ####

  <div id="smo_box">

Objects bloat our code considerably, since we're often forced to write statements instead of expressions. We can avoid this in `map_object`, since `object_get_vars` gives us an array, which we already know how to map a function over:

```{.php pipe="./tag"}
function map_object($f, $x) {
  return (object) map_array($f, get_object_vars($x));
}
```
  </div>
</div>

A function is like a 'box' too, but it's a bit more complicated to imagine. If we think of a function as being a huge [lookup table](http://en.wikipedia.org/wiki/Lookup_table), where the argument is looked up in the table to find the return value, then the return values are the 'contents'. This means our map function needs to apply one function to the return values of another. This is just *[function composition](http://en.wikipedia.org/wiki/Function_composition)* which I defined at the beginning:

```{.php pipe="./append"}
function map_function($f, $g) { return compose($f, $g); }
```

Now, these map functions aren't completely arbitrary; they must obey a couple of rules. They are:

```{.php pipe="./tag"}
// Mapping an identity function cannot change the contents of a box
map('id', $x) == $x

// Mapping $g then $f is the same as mapping compose($f, $g)
map($f, map($g, $x)) == map(compose($f, $g), $x)
```

These rules might look confusing, but in practice it's easy to obey them (all of the definitions we've seen so far do).

Now, how does this relate to the tokens of an EDSL? How are tokens like boxes? We can split our tokens into two groups: 'results' and 'commands'. Results are like arrays, they contain a value we can map a function over. Commands are like functions, running them will produce a value we can map a function over.

Let's make a few languages to see how this works. We will represent all of our tokens as arrays where the first element is the token's name and the rest are either command arguments or result values. We'll only use one kind of result, which we'll call `VAL`, but we'll implement a few different commands. Here are the example languages we'll implement:

#### EDSL for database programming ####

Token                          Meaning
------------------------------ -------
`ERR`                          Error
`SQL  $query         $handler` A raw SQL query
`SQLS $query $params $handler` An SQL query containing sequential parameters (`?`)
`SQLN $query $params $handler` An SQL query containing named parameters (`:foo`)


#### EDSL for common HTTP requests ####

Token                      Meaning
-------------------------- -------
`ERR`                      Error
`GET  $url $handler`       A GET request
`POST $url $data $handler` A POST request
`EOF`                      Done

#### EDSL for reading files ####

Token                     Meaning
------------------------- -------
`ERR`                     Error
`SEEK $position $handler` Move our cursor to some position
`READ $length $handler`   Read some data, beginning at the cursor
`CLOSE`                   Close the file

We don't want to expose our implementation, so we'll encapsulate these in functions:

```{.php pipe="./append"}
// Database functions
function db_sql($sql) {
  return ['SQL', $sql];
}
function db_sql_seq($sql, $params) {
  return ['SQLS', $sql, $params];
}
function db_sql_named($sql, $params) {
  return ['SQLN', $sql, $params];
}

// HTTP functions
function http_get($url) {
  return ['GET', $url];
}
function http_post($url, $data) {
  return ['POST', $url];
}

// File functions
function file_close($file) {
  return ['CLOSE', $file];
}
function file_seek($file, $position) {
  return ['SEEK', $file, $position];
}
function file_read($file, $length) {
  return ['READ', $file, $length];
}
```

<div id="red" class="hide odd">
#### A note on redundancy ####

  <div id="red_box">

You may think the above definitions look rather like a design pattern, and you'd be right. The reason I've left so much redundancy in that code is to make its purpose clearer for the tutorial. If this were a serious project I'd remove this redundancy using functions.

The most obvious redundancy is that all these functions have the same structure: accept some arguments and return them in an array, prefixed by some string. Well, it's easy to return an array of our arguments (see `array_`), but that won't handle the strings. However, it's easy to prepend an argument using `apply`. Let's see what happens:

```{.php pipe="./tag"}
$db_sql         = apply('array_', 'SQL');
$db_sql_seq     = apply('array_', 'SQLS');
$db_sql_named   = apply('array_', 'SQLN');
$http_get($url) = apply('array_', 'GET');
$http_post      = apply('array_', 'POST');
$file_close     = apply('array_', 'CLOSE');
$file_seek      = apply('array_', 'SEEK');
$file_read      = apply('array_', 'READ');
```

Well that's brought down the amount of code quite considerably, however there's still some clear redundancy in this code; namely that we keep calling `apply` just with different arguments:

```{.php pipe="./tag"}
foreach (['db_sql'       => 'SQL',
          'db_sql_seq'   => 'SQLS',
          'db_sql_named' => 'SQLN',

          'http_get'  => 'GET',
          'http_post' => 'POST',

          'file_seek'  => 'SEEK',
          'file_read'  => 'READ',
          'file_close' => 'CLOSE',
         ]) as $n => $t) {
  $$n = apply('array_', $t);
}
```

That's better: we've removed redundancy and made it easier to add new functions in the future (just add a `'name' => 'token'` pair to the array). In the process we've switched from globals to locals; I think this is a GoodThing(TM), but if you insist on your functions being global you can use [this function]()

  </div>
</div>

Now we know what all of our tokens look like, we can implement our map functions. Here I'll use functions called `do_...` to indicate a side-effecting procedure:

```{.php pipe="./tag"}
map_db($f, (Output x next) = Output x (f next)
map_db($f, (Bell     next) = Bell     (f next)
map_db($f,  Done           = Done

// We use a wrapper function to encapsulate our DB connection details
$map_db = call_user_func(
  function($username, $password, $host, $database) {
    $db = db_connect($username, $password, $host, $database);

    // Our map function itself
    return function($f, $x) use (&$db) {
      switch ($x[0]) {

        case 'VAL':
          return ['VAL', $f($x[1])];

        case 'SQL':
          return ['VAL', $f(do_db_query($db, $x[1]))];

        case 'SQLS':
          return ['VAL', $f(do_db_seq_query($db, $x[1], $x[2]))];

        case 'SQLN':
          return ['VAL', $f(do_db_named_query($db, $x[1], $x[2]))];

        case 'ERR':
          return ['ERR'];
      }
    };
  },
  'my_user',
  'my_pass',
  'my_host',
  'my_db');

// We use a wrapper function to encapsulate our cURL client
$map_http = call_user_func(
  function($client) {

    // Our map function itself
    return function($f, $x) use (&$client) {
      switch ($x[0]) {

        case 'VAL':
          return ['VAL', $f($x[1])];

        case 'GET':
          return ['VAL', $f(do_curl_get($client, $x[1]))];

        case 'POST':
          return ['VAL', $f(do_curl_post($client, $x[1], $x[2]))];

        case 'ERR':
          return ['ERR'];
      }
    };
  },
  do_make_curl_client());

// We use a wrapper function to encapsulate our file
$map_file = call_user_func(
  function($path) {
    $file = fopen($path);

    // Our map function itself
    return function($f, $x) use (&$file) {
      switch ($x[0]) {

        case 'VAL':
          return ['VAL', $f($x[1])];

        case 'READ':
          return $file? ['VAL', ] : ['ERR'];

        case 'SEEK':
          fseek($file, $x[1]);
          return $file? ['VAL', $f(fseek($x[1], $x[2])] : ['ERR'];

        case 'CLOSE':
          if ($file) {
            fclose($file);
            return ['VAL', NULL];
          }
          return ['ERR'];

        case 'ERR':
          return ['ERR'];
      }
    };
  },
  '/home/me/foo');
```

That's all we need for our tokens. The next thing we need is a way to combine these into actual programs.

## Wrap and Join ##

Now we'll introduce a couple of other functions known as `wrap` and `join`. If we have a map function, a wrap function and a join function then we have a *monad*. Monads are special functors which are very useful for representing arbitrary single-threaded computations.

<div class="odd hide" id="conv">

#### Note about conventions ####

  <div id="conv_box">

The function I'm calling `wrap` here is usually called `return`; I've used a non-standard name to prevent confusion with PHP's `return` keyword, which means something different.

Also, it might be useful to know that [many other monad tutorials](http://www.haskell.org/haskellwiki/Monad_tutorials_timeline) don't talk about `join` at all; instead they talk about a different function called `bind` (often written as an operator `>>=`). This actually makes no difference; we can define bind in terms of join and we can define join in terms of bind, so either will suffice. We'll see what bind does later on.

  </div>
</div>

The wrap function is pretty straightforward; it takes a value and returns a 'box' containing that value. For our languages that means prefixing the value with `'VAL'`:

```{.php pipe="./append"}
function wrap_val($x) { return ['VAL', $x]; }
```

<div class="hide odd" id="swv">

#### A simpler wrap_val ####

  <div id="swv_box">

Of course, we could also define `wrap_val` this way:

```{.php pipe="./tag"}
$wrap_val = apply('array_', 'VAL');
```

  </div>
</div>

Here are some wrap functions we can use for arrays, objects and functions (notice that functions are the most awkward, since PHP forces us to use statements):

```{.php pipe="./append"}
function wrap_array($x) { return array_($x); }

function wrap_object($x) {
  $o = new stdClass;
  $o->foo = $x;
  return $o;
}

function wrap_function($x) {
  return function() use ($x) { return $x; };
}
```

<div class="hide odd" id="swa">

#### A simpler wrap_array ####

  <div id="swa_box">

Of course 'wrap_array' is equivalent to the following:

```{.php pipe="./tag"}
$wrap_array = 'array_';
```

This transformation is known as *eta-reduction*.
```

  </div>
</div>

The `join` function is a bit harder to explain; it takes a box containing boxes (what we might call a '2D box') and collapses it down a level. A '2D box' becomes a '1D box', a '5D box' becomes a '4D box', and so on. Here are some examples for arrays, objects and functions:

```{.php pipe="./tag"}
function join_array($x) {
  return call_user_func_array('array_merge', $x);
}

function join_object($x) {
  $y = new stdClass;
  foreach (object_get_vars($x) as $o) {
    foreach (object_get_vars($o) as $name => $value) {
      $y->$name = $value;
    }
  }
  return $y;
}

function join_function($x) {
  return function($y) use ($x) {
    return call_user_func($x($y), $y);
  };
}
```

So, what does `join` mean in the context of our EDSLs? It turns out that join is our interpreter: we can join commands by executing them in sequence. Executing a command gives us a result, which we can then pass to another command. Here are some interpreters for our EDSLs:

```{.php pipe="./append"}
map f m ≡ m >>= (\x -> return (f x))
join n ≡ n >>= id

m >>= g ≡ join (map g m)

(SQL  q)    >>= f = f (db_send_query q)    = join (map f (SQL  q))
(SQLS q xs) >>= f = f (db_send_query q xs) = join (map f (SQLS q xs))
(SQLN q ns) >>= f = f (db_send_query q ns) = join (map f (SQLN q ns))

function join_db($x) {
  switch ($x[0]) {
    case 'VAL':
      return $x[1];
    case 'SQL':
      return
  }
}
```

To have a monad, our wrap functions need to obey the following rule:

```{.php pipe="./tag"}
// Applying $f then wrapping is the same as wrapping then mapping $f
wrap($f($x)) == map($f, wrap($x))
```

Join functions can perform arbitrary work, such as calling databases or opening files, as long as they obey the following rules:

```{.php pipe="./tag"}
// Joining twice can be done 'outside-in' or 'inside-out'
join(join($x)) == join(map('join', $x))

// Wrapping then joining does nothing
join(map('wrap', $x)) == join(wrap($x)) == $x

// Mapping $f 'inside' $x then joining is the same as joining $x then
// mapping $f
join(map(apply('map', $f), $x)) == map($f, join($x))
```

Since we want to pick an choose our implementation, we don't want our join function to actually perform any actions. Instead, we'll just have it accumulate tokens for later processing.

## Free Monads ##

A monad which just accumulates values is known as a *free monad*. Specifically, if `F` is a functor then there is a unique *free monad of `F`*. We'll use this to accumulate our tokens.

Free monads are usually described using an analogy to *free monoids*. Let's say we want to combine a bunch of numbers using a binary function, eg. `+`, `*` or `max`. However, we want the ability to pick and choose the function *after we've already combined the numbers*. How can we combine numbers if we don't know how what our combining function will be? The answer is to combine them into arrays:

```{.php pipe="./tag"}
// Our interpreter is just array_reduce
function interpret($function, $identity, $array) {
  return array_reduce($array, $function, $identity);
}

// Particular combination functions
foreach (['plus'  => [function($x, $y) { return $x + $y; }    , 0   ],
          'times' => [function($x, $y) { return $x * $y; }    , 1   ],
          'max'   => ['max'                                   , -INF],
          'array' => [function($x, $y) {
                        return array_merge($x, [$y]);
                      }                                       , []  ],
         ] as $n => $a) {
  $$n = call_user_func_array(apply('interpret') $a);
}

// Test data
$data = [50, -45, 7.9, 897];

// Combining our test data with different combination functions
$plus ($data) == 909.9
$times($data) == -15944175
$max  ($data) == 897
$array($data) == [50, -45, 7.9, 897]

// Since $array gave us back the data unchanged, we can use it to
// defer our choice of combination function
$plus ($array($data)) == 909.9
$times($array($data)) == -15944175
$max  ($array($data)) == 897
$array($array($data)) == [50, -45, 7.9, 897]
[/code]

These binary functions are known as *monoids* and `$array` is known as a *free monoid*. specifically We aren't going to implement for example the maybe monad projoins its contents after scan collapse its argument in arbitrary ways, a sequencing

#### Free Monads ####

However, if we use functors to represent know that functors let us allow us to can see that functors allowwe can use [i]map[/i] to   have two more functions letting us map functions over their contents, also let us combine those function calls in useful ways. Such function combination is very powerful: we can use it to implement programming languages. However, as I mentioned above, we don't want to use just one implementation; we want the option of several, so we can run, test, inspect, etc.

As an analogy, think about how we might combine two numbers. We might add them, subtract them, multiply them, etc. How might we combine them in a way that lets us pick an choose the operation [i]after[/i] we've combined them? The answer is that we put them in an array! We can then apply any operation we like to the elements.

The corresponding object for combining our functions is known as a 'free monad'. Now, I said above that monads are a kind of functor; in which case, the free monad of a functor is itself a functor. Rather than defining a single map function, we'll define a 'map function generator': this will take the map function from an existing functor, say "F", and return the map function for a new functor, the free monad of F. This is where things get a bit confusing, but since it works for all functors, it only needs to be written once:

```{.php pipe="./append"}
// We take a map function, so we know what functor (language) to use
function make_functor_free($map) {
  // We generate a map function
  return function($f, $x) use ($map) {
    // A free monad contains two kinds of value: PURE and ROLL
    switch ($x[0]) {
      case 'PURE':
        // Apply our function to PURE values straight away
        return ['PURE', $f($x[1])];

      case 'ROLL':
        // For ROLL, we defer the call by composing $f with $map
        return ['ROLL',
                $map(function($a) { return $map($f, $a); }, $x[1])];
    }
  };
}
```

Now that we've defined free monads as functors, we can define them as monads. To define a monad we need a wrap function and a join function:

```{.php tag="./append"}
// A helper function
function concat_free($map) {
  return function($x) use ($map) {
    switch ($x[0]) {
      case 'PURE':
        return $x[1];

      case 'ROLL':
        return ['ROLL', $map(concat_free($map), $x[1])];
    };
}

// Again we use a 'generator'
function make_monad_free($map) {
  return [
    'wrap' => function($x) {
      // Wrapping up a value just makes it PURE
      return ['PURE', $x];
    },

    'bind' => function($x, $f) use ($map) {
      return call_user_func(concat_free($map), $map($f, $x));
    }];
}

instance Functor f => Monad (Free f) where
  return = Pure -- just like []
  x >>= f = concatFree (fmap f x)
```

--this is the same thing as (++) basically


```{.php pipe="./tag"}
// Some procedures with side-effects
function connect() {
  // ...
}

function disconnect() {
  // ...
}

function send($x) {
  // ...
}

// Some business-logic with side-effects
function bl1() {
  connect();
  send('abc');
  send('xyz');
  disconnect();
}

// The same logic without side-effects

```

Notice that we haven't specified a way to *unwrap* our values, and there's a very good reason: we want functors to be one-way, just like calling a procedure which has side-effects, eg. deleting a file: once we've run it, there's no way to go back.

similarity  at least, that a funct If our we're going to treat functors as use functors as an interface for side-effecting procedures. This is a much cleaner alternative to dependency injection.

<div>
<script type="text/javascript">//<![CDATA[
  (function() {
    var buttons = document.getElementsByClassName("hide");
    for (var i = 0; i < buttons.length; i++) {
      buttons[i].onclick = (function(vis) {
        var box = document.getElementById(buttons[i].id + "_box");
        box.style["display"] = "none";
        box.style["cursor"]  = "pointer";
        return function() {
          box.style["display"] = ["block", "none"][vis+0];
          vis = !vis;
        };
      }(false));
    }
  }());
//]]></script>
</div>
