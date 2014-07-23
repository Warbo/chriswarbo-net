---
title: Dynamic scope vs. Lexical scope
---
Yesterday I came across [a blog post about dynamic scope] [1]. While its content is completely correct, dynamic scope is actually completely unnecessary, and in fact a Bad Thing(TM), compared to lexical scope.

[1]: http://sixtyk.blogspot.co.uk/2013/03/dynamic-scope-pattern.html

In the example the name `important_variable` is a free variable in the functions `do_something` and `do_something_else`. This means we've used the variable `important_variable` in the functions' definitions but haven't declared it or accepted it as an argument. For example, in a pseudo-Javascript that uses 'let' to make variable scoping explicit:

```javascript
let (do_something = function() {
  printf(important_variable);
}) in {
  do_something();
}
```

Here we bind the variable `do_something` to a function, so `do_something` is a bound variable when we call it. Inside the function we're using two variables called `printf` and `important_variable` which **are not arguments** to the `do_something` function and **are not declared** with a `let` **inside** the function, so they are free variables. What happens to them depends on whatever "scoping rule" is in effect.

"Global scope" is an incredibly bad scoping rule. It keeps a table of variable names and whenever we hit a free variable, we look it up in the table. This has been used in Mathematics since antiquity, but now we have computers our Mathematics (programming) is reaching huge scales, where global scoping really falls short.

The reason is that whenever we call any function, we lose all information about what's in the table, since any function can do anything it likes to the table. Since programming is mostly about chaining together function calls, this makes global scoping a pretty useless way of handling free variables, since we can't really know anything that's going on. Hence, never use global scope. Many languages claim that they disallow global scope for variables, yet they carry on using it for function names, class names, type names, etc. (as if there were any distinction!)

Another scoping rule is called "dynamic scope" and is what the above link is about. Dynamic scope dates back to McCarthy's original LISP in the 1950s. The rule says that when we call a function with free variables, we should bind them to whatever those names are in the current context.

With dynamic scoping we can define `do_something`, then define `printf` and `important_variable`, then call `do_something`. The free variables will be bound and everything will be good. We can call do_something over and over and we can also define `important_variable` and `printf` over and over. There's no point redefining `do_something`, although we can if we want. Each call will traverse up the `let` blocks until it finds `do_something`, `printf` and `important_variable`, and those values will be used together. For example:

```javascript
let (do_something = function() {
  printf(important_variable);
}) in {
  let (printf = function(x) {...}) in {
    let (important_variable = 1) in {
      // Prints 1
      do_something();
    }
    let (important_variable = 2) in {
      let (important_variable = 3) in {
        // Prints 3
        do_something();
      }
    }
  }
}
```

Another scoping rule is "lexical scope", which dates back to at least ALGOL 60. The rule says that when we define a function with free variables, we should bind them to whatever those names are in the current context.

Note that this is **identical** to dynamic scope, except it happens when we *define* a function rather than when we call it. With lexical scope we can define `important_variable` and `printf`, then define do_something, then call `do_something`. The free variables will be bound and everything will be good. We can call do_something over and over, we can define `important_variable` and `printf` over and over and we can redefine `do_something` over and over. Each call will traverse up the `let` blocks until it finds `do_something`. We then traverse up the `let` blocks *starting inside the definition of `do_something`* until we find `printf` and `important_variable`, and those values are used. For example:

```javascript
let (printf = function() {...}) in {
  let (important_variable = 1) in {
    let (do_something = function() {
      printf(important_variable);
    }) in {
      // Prints 1
      do_something();
      let (important_variable = 2) in {
        // Prints 1
        do_something();
      }
    }
  }
}
```

This looks just like our dynamic scope version, except the nesting of the `let` blocks has swapped around. This retains the global-like ability to access variables without having to pass them as arguments all of the time, but we have a few advantages over dynamic scope:
 - The burden of providing a correct context for free variables is placed on whoever's writing the function, rather than on everyone who ever calls it.
 - Only one context is ever needed, even if the call sites are in wildly different places.
 - We can read function definitions top to bottom safe in the knowledge that nothing in the caller's code can mess it up, unless explicitly passed as an argument.
 - We can read our application's code top to bottom safe in the knowledge that nothing in our libraries' code can mess it up, unless explicitly passed as a return value.
 - We can use whatever variable names we like without fear of shadowing a free variable by accident.

The last point about shadowing names is the a **major** advantage, especially when our variable scope gets large. For example, let's say that one day we are asked to give users of our application the ability to change their usernames. Pretty standard, let's write some generic CRUD-style code to do it. Here `form_username` is the username given in the update form and `db_username` is the username currently in the database:

```javascript
function(form_username, db_username) {
  // Don't hit the database unnecessarily
  if (form_username == db_username) {
    return true;
  }

  // Don't allow duplicate usernames
  let (existing = db_select('users', 'username', form_username)) in {
    if (count(existing) > 0) {
      return false;
    }
  }

  // OK, go ahead and update
  db_update('users',                   // Table to update
            'username', form_username, // New value
            'username', db_username);  // WHERE condition
  return true;
}
```

All looks fine (ignoring error conditions), right? Well actually it fails to connect to the database. That's weird, since we have all kinds of functions that look pretty much the same, for updating contact details and things. Why doesn't this one work? The database is up, all of the other functions are connecting, why won't this one?

Eventually, after many hours of debugging, we discover the following function deep in the bowels of the application:

```javascript
let (execute_query = function(query_string) {
  let (database = connect(db_username, db_password)) in {
    return send_query(database, query_string);
  }
}) in { ... }
```

When we connect to the database, the username to connect with is taken from the `db_username` variable. Well, where does **that** come from? Some more digging around reveals, at the very top of the whole stack:

```javascript
let (run = function(db_username, db_password) {...}) in {
  if (have_arg("test")) {
    run("tester", "test123");
  } else {
    run("real_user", "pass123");
  }
}
```

The database details are passed in at the top of the application, so that we can run it in test mode easily. Whenever any part of the application calls `execute_query`, it gets the value of `db_username` by traversing up the nested scopes until it gets all of the way up to the top and finds either the live details or the test details. That is, of course, any part of the application except our username function, since it will find **our** `db_username` and try to connect to the database server with that! This will clearly be rejected as unauthorised, and hence the error.

Of course this would be even worse if our variables were global, since the whole application would start using incorrect credentials as soon as someone tries changing their username. Hence dynamic scope is better than global scope, but this is still pretty bad. The whole point of a database API is that we don't care how it works. We shouldn't be completely ignorant, for example it might be good to know in which situations function `foo` will be faster than function `bar` and vice versa, but we shouldn't have to care about the bloody variable names! In the Programming Language Theory lingo we would say that this breaks "alpha equivalence", ie. `function(x) { blah(x); }` should behave the same as `function(y) { blah(y); }`.

In the case of lexical scope this issue doesn't arise. We can use exactly the same mechanism to pass `db_username` into `run` and access it in `execute_query`, but the binding will happen when `execute_query` is *defined* rather than when we *call* it. This means that all of the binding has taken place long before our application code ever runs; usually during the application initialisation. Our functions will still have access to `db_username`, and can shadow it if we like, but the only stuff we can affect is that which we define ourselves.

Now, in a lexically-scoped language variable access is one-way, children can access their parent's scope but not the other way around. This is, of course, encapsulation, but we can get around it with judicious use of higher-order functions. There's nothing wrong with code like this:

```javascript
// Database credentials aren't in scope yet, so we can't define the
// database API. However, we *can* define a function which returns
// an API when given the credentials. We can think of this as a
// 'factory'
let (database_factory = function(db_username, db_password) {
  // Return a database API specialised to the given credentials
  return [function() {
            // ...
         },
         function() {
           // ...
         },
         ...
         ];
}) in {
  // Some arbitrary amount of nesting ...
  // Now we have some credentials, so we can make a database API
  // with them
  let ([db_select, execute_query, ...] = database_factory(username, password)) in {
    // Any code here has access to the API
  }
}
```

In fact, this is a more flexible approach than passing in the credentials at the top, since we can now use many different versions of the API in different parts of the codebase. If we want to use the same API in different parts that's fine, we can pass the functions around as arguments/return values. This is similar to parameterised module systems, but less structured.
