---
title: Environment Variables are Dynamically Scoped Keyword Arguments
---

Like most of my blog posts, this began as
[a comment on Hacker News](https://news.ycombinator.com/item?id=26663742),
in response to a post
[discouraging the use of environment variables](https://nibblestew.blogspot.com/2021/03/never-use-environment-variables-for.html).

I disagree with that post's arguments and conclusions, mainly the way it
completely ignores the existence and usefulness of dynamic scope. Since I've
also been making very good use of dynamic variables in Scala recently, I thought
it was worth writing up.

## Scopes ##

"Scope" describes where to look for the value of a variable. There are three
types of scope that are common across most programming languages (inheritance
in object-oriented programming can be thought of as a form of scope, but that
would be a whole other tangent!)

This doesn't really come up when we do 'first-order' programming with bound
variables, like this:

```javascript
function foo(x, y) {
  let a = x;
  let z = a + y;
  return z * 2;
}

let a = 5;
print(foo(1, 2));
print(a);
```

Here the call to `foo` binds the variables `x` and `y` (to `1` and `2`); `a` and
`z` are bound in the block by the `let` statements. Hence the first `print`
gives out `6` (since it's `(1 + 2) * 2`). In most languages, the second `print`
will give out `5`, due to the `let a = 5;` line (it would give `1` when using
global scope).

We need to care about scoping rules when we have *free variables*, whose values
will be fetched from *somewhere else*; scoping tells us where to get them from.

For example, the `a` variable is "free" in the function `bar`, since it is not an
argument to `bar` or defined within `bar`:

```javascript
function foo(x) {
  let a = x;
  return function bar(y) {
    let z = a + y;
    return z * 2;
  };
}

let a = 5;
print(foo(1)(2));
print(a);
```

### Global Scope ###

*Global* variables are bound to at most one value; all occurrences of a global
variable's name, anywhere in a program, refer to the same thing.

If we treat `a` as globally scoped in the above example, it gets bound by the
line `let a = 5;`. The call `foo(1)` also binds the `a` variable, in the line
`let a = x;`. There are two ways that a language could handle this:

 - It could be treated as an error. In this case global variables would be
   *immutable constants*. Such errors could be *static* (spotted by a tool like
   a compiler, before the program is ever run) or *dynamic* (triggered during
   program execution, if it ever attempts to bind an existing variable).
 - Subsequent bindings could replace any existing ones. This is a form of
   *mutation*, and imposes a notion of ordering/time on a program's meaning.
   Hence a result in one part of a program might depend on which order the other
   parts of that program happen to be evaluated.

The first approach is pretty reasonable, especially when statically-checked (in
which case the above example would not be a valid program). Frustratingly,
languages which provide global variables tend to use the second approach
instead. Under that scheme, the variable `a` will be re-bound to the value of
`x`, which is `1`.

The `bar` function is called with the argument `2`, which gets bound to the
argument name `y`. When we encounter the line `let z = a + y;`, these variables
resolve to `let z = 1 + 2;`, hence binding `z` to `3` and resulting in the value
`6` being printed.

Finally, the value of `a` is printed, which is `1`.

Global scope is a "Worse is Better" approach: it's easy to implement (e.g. using
a single hashmap) and easy to understand on a small-scale; yet as a codebase
grows it gets harder and harder to predict its behaviour, since we need to keep
searching the whole application to see which things may affect which others. We
can also encounter surprising breakages, if we choose a variable name which
just-so-happens to already be used elsewhere.

Programming with mutable global scope can be made reasonable by following
disciplined practices like modularity (e.g. a module should never refer to
variables used by another module; variable names should include their module, to
prevent clashes; etc.). However, that's only useful in greenfield work, since we
can't assume that some arbitrary legacy system will obey any particular
practices (even if it claims to, without enforcement by the language/tooling it
may have crumbled under the weight of maintenance).

### Lexical Scope ###

This is the most common form of scope in most programming languages. Values are
looked up by starting where the variable is *written* (hence "lexical"), and
working outwards from there until a binding is found.

In the above example, the variable `a` in the line `let z = a + y;` is resolved
by starting in the place where this line is written, which is in the `bar`
function:

```javascript
         function bar(y) {
    ...
    return z * 2;
  }
```

The `bar` function doesn't bind an `a` variable, so we look in the place where
the `bar` function is written, which is in the `foo` function:

```javascript
function foo(x) {
  let a = x;
  return ...;
}
```

The `foo` function *does* bind an `a` variable, in the line `let a = x;` (and
the `x` variable is also bound, as the function's argument). Hence this is the
value that will be used in our original line `let z = a + y;`, which (for the
argument `x = 1`) gives `let z = 1 + 2;` and prints `6` like in the global
version.

However, unlike the global version, the subsequent `print(a);` line will print
`5` this time, since the binding `let a = 5;` is written in the same place as
that `print` call.

Lexically scoped variables tend to be a good default, since they are modular and
encapsulated without requiring discipline from the programmer: they can only be
referenced by code which is written in the same unit/module. Clashes are
minimised, since the same name can be used in different places to refer to
different things.

Lexically scoped variables are controlled by whoever is *writing* the function,
and they usually know what variables in their module/library/etc. should be.

Lexical scope can be tricky to *implement*, since a function call's bindings may
be needed after it has finished running. For example, when we perform the
`bar(2)` call above, the `let z = a + y;` line needs to know the binding `x = 1`
from the `foo(1)` call that defined `bar`; yet that call has already finished
and returned a value!

We need some way to 'propagate' bindings into functions which reference them
(known as *closures*). One way is to keep old stack frames alive in case they're
needed later, resulting in
["spaghetti stacks"](https://en.wikipedia.org/wiki/Parent_pointer_tree#Use_as_call_stacks).

### Dynamic Scope ###

This is similar to lexical scope, but rather than looking for bindings where the
code was *written*, it looks where the code was *called*. In the above example,
we would resolve `a` in the line `let z = a + y;` by looking where that line was
called from: since it's called from inside the `bar` function we look there for
an `a` binding. There isn't one, so we look where `bar` was called, which was
from the top-level, so we look there and find the binding `let a = 5;`. Hence we
get `let z = 5 + 2;` and the value `14` is printed. The final line `print(a);`
is called from the top-level, so it also uses the `let a = 5;` binding, printing
the value `5`.

Dynamic scope seems to have been invented by accident, when early Lisp
implementations wanted to provide higher-order functions like lambda calculus,
but hadn't implemented spaghetti stacks (or equivalent) to allow the re-entrant
behaviour required for lexical scope. Hence they looked up variables from the
*call stack* instead, and got this subtley different behaviour.

Dynamic scope turns out to be useful for variables which the *caller* of the
code knows better than the *writer* of the code. This makes it useful for
configuration parameters and dependency injection. In particular it avoids the
need to either pass extra arguments along chains of function calls, or write
all relevant functions within one big lexical scope; yet still allows overriding
as needed (e.g. within tests).

## Environment Variables are Dynamically Scoped ##

This is my main *objective* disagreement with the linked post: it claims that
environment variables act like (mutable) global variables, and uses many of the
known problems of (mutable) global variables as reasons to avoid environment
variables. Yet these arguments fall apart when we consider environment variables
in context: as a form of inter-process communication. When viewed in this way,
environment variables are *not* globals, but are instead much closer to
*dynamic* variables.

### Environment Variables are Locally Overridable ###

We can override environment variables for a particular subprocess without
affecting anything else. For example, consider the following script:

```bash
echo "BEFORE $FOO"
FOO=bar printFoo
printFoo
echo "AFTER $FOO"
```

Let's assume `printFoo` is a script which performs `echo "FOO is $FOO"`, and we
run our script with an environment containing `FOO=foo`, we will get:

```
BEFORE foo
FOO is bar
FOO is foo
AFTER foo
```

The binding `FOO=bar` changes the output between the first and second lines, but
it could not have *mutated* the variable `FOO`, since the original value `foo`
remains intact for the final two lines. This behaviour doesn't match that
of global variables described above (mutable or immutable), since globals only
have a *single* binding (which, in the mutable version, may be irrevocably
overwritten).

### How Environment Variables *Actually* Work ###

The real behaviour of environment variables is that each process gets its own
'environment' of variable bindings when started, and those environments begin as
*copies* of the environment that invoked that process; plus any given extras or
overrides, like in the `FOO=bar` example above.

This explains the behaviour of our example script: the line `FOO=bar printFoo`
runs the `printFoo` process in a new environment, copied from the script's
except for the override `FOO=bar`. This override is why the second line shows
`bar`.

Once that `printFoo` process has finished, its environment (containing the
override) is discarded. The next `printFoo` command is *also* run from the
script's environment, getting its own copy of the original `foo` value
(*without* any overrides this time). Hence the output goes back to `foo`.

The final `echo` uses the script's environment directly, which contains the
original value `foo`.

This mechanism of *copying* variable bindings is rather inefficient, compared to
the lightweight stack-based approach used by function/procedure calls *within* a
process. However, the resulting behaviour, shown above, is indistinguishable
from dynamic variable binding.

### A Word on Mutation ###

Some languages don't allow environment variables to be mutated, e.g. Scala's
`sys.env` is an immutable map (although the JVM has other ways to read and
write the environment). Many languages *do* permit mutation, and it acts
differently to mutating a dynamic variable (as far as subprocesses are
concerned).

Dynamic variables are found by looking further and further up the call stack for
a binding, so any mutations made to such a binding will be visible to everything
below that binding's stack frame; even after the code performing the mutation (a
function/procedure/etc.) has long since finished and had its own stack frame of
bindings discarded.

On the other hand, since environment variables are *copied* from parent to child
at each new scope (process), mutations are only made to that process's *own
copy*, and hence aren't visible to any sibling or parent of that process.

In fact, this isolation of mutations makes environment variables even *less*
'globalish' and 'mutableish' than ordinary dynamic variables!

As an example, if the `printFoo` command above were to finished by mutating the
`FOO` variable, e.g. using `export FOO=baz`, it wouldn't affect our script at
all. Even the call to `printFoo` which "inherits" the script's `FOO` without
overriding it, would only be mutating *its own copy* of `FOO`, which won't
affect the script's copy of the variable (and hence won't alter the final line).

Note that I *highly* recommend to avoid mutating env vars, for the same reason
I avoid mutating *any* variables, regardless of language; unless there's a
specific reason to do so. *Overriding* dynamic variables with a new scope is
fine; it's what they're for!

If we treat environment variables in a sane, immutable way, then they act
*exactly* like dynamic variables.

### Environment Variables *Within* Processes ###

I can predict an obvious objection to what I've said: aren't environment
variables mutable globals *within* processes (e.g. in a Python script, or
whatever)?

No!

Processes are free to do what they like with their given environment variables.
Many languages, like Python, *choose* to treat their given environment variables
like mutable globals by default, and we can absolutely criticise them for that
choice; but it's not the fault of environment variables *themselves*.

Other languages treat environment variables in a saner way. For example, Racket
puts the whole environment in a dynamic variable called
`current-environment-variables`, which can be overridden for sub-expressions
just like we did for sub-processes above. For example:

```scheme
(define (print-foo)
  (printf (string-append "FOO is " (getenv "FOO"))))

(print-foo)
(parameterize ((current-environment-variables
                (make-environment-variables "FOO" "bar")))
  (print-foo))
(print-foo)
```

With an initial environment of `FOO=foo`, this will print:

```
FOO is foo
FOO is bar
FOO is foo
```

This keeps the behaviour of environment variables more consistent within and
between processes (subprocesses invoked by Racket will inherit whichever
`current-environment-variables` is in scope at the time; this is also true
for nice libraries like
[shell-pipeline](https://docs.racket-lang.org/shell-pipeline)).

I hope more languages will adopt this sort of approach, and in the mean time
we can use libraries to bypass the crappy defaults of common languages.

## Use-Cases for Dynamic Scope ##

Now that we have a more appropriate mental model of environment variables, we
can think about situations where they (and dynamic scoping in general) *are*
appropriate.

Lexical scope is certainly best to use by default; Scheme was a definite
improvement over previous Lisps in that regard! However, far from being merely
a historical mistake, dynamic scope turns out to be *very* useful in situations
where (as mentioned above) a variable is better specified by the *caller* than
by an application/library itself. In that sense, dynamic variables are like
*implicit arguments* to a function, passed along (also implicitly) to
subroutines.

### Configuration ###

Dynamic variables can be a useful way of referring to the sort of data that is
often put into config files; e.g. verbosity level, UI options, paths to required
files, etc.

It's no coincidence that such [configuration data is *also* a good fit for using
environment variables](https://12factor.net/config).

When it comes to env vars versus commandline arguments, I treat the distinction
in a similar way to keyword arguments versus positional arguments for functions
(in languages which support both, like Python). Note that this is more than just
a passing resemblance: keyword arguments are often the only form which allows
*default values* (commonly used like function-level 'configuration')!

I personally try to avoid giving my software config files, since they're a form
of global mutable state, and they also make it harder to isolate and reproduce
executions. They also introduce extra headaches like read errors, and require
some mechanism to specify the config file location (which itself is a form of
config, that could be put in an env var!)

**TANGENT:** There's an under-appreciated approach to config files, which manages
to avoid some of the problems mentioned above. That is to pipe the config data
into an application via
[process substitution](https://en.wikipedia.org/wiki/Process_substitution). For
example:

```bash
myProgram -c <(jq -n '{"verbosity": 1, "port": 8000}')
```

This lets us specify all of our config up-front, without having to manage extra
files on disk (e.g. versioning, permissions, etc.). For this to work, the config
file path must be accepted as a commandline argument, the application needs to
read it directly (rather than trying to dereference symlinks, etc.) and it
should avoid seeking (`zsh` supports seeking via its `=(foo)` form of process
substitution, but other shells like `bash` do not).

Note that I *agree* with that article's criticism of application-specific
formats, like `:`-separated entries in `PATH`, but that's certainly not unique
to env vars. Config files and env vars are both strings of bytes, so what works
for one will usually work for the other. In particular, I have nothing against
JSON inside environment variables (although judgment needs to be made about when
to use a single variable containing a JSON object, and when to use multiple
variables).

### Dependency Injection ###

Dynamic variables are a great way to implement *dependency injection*, where we
allow clients/handles/connections to external or side-effecting systems to be
overriden by callers.

#### Logging ####

A simple example of dependency injection is logging, where a library might
choose *when* and *what* to log, but the caller decides *how* to handle log
messages.

There are two ways to achieve this purely using lexical scope. We could write
our entire application inside the body of a function, which accepts a logger as
argument:

```javascript
function myLibrary(log) {
  // Entire application is defined here

  // Whenever we want to log, we just do:
  log("My message");
}
```

Applications and other libraries can call this function with their desired
logger, or multiple times if they need to use several implementations (e.g.
silencing certain actions, accumulating output to various buffers during tests,
etc.). If we *do* call this function multiple times, we'll need to keep track of
the different results, calling the correct version when appropriate.

This is clearly impractical, not least because cramming everything into the same
function body prevents modularity. A common alternative is to have a *single*
instance of our library, but allow the logger to be passed in as an extra
argument wherever it will be used. For example:

```python
def bill(log, customer):
  log("Billing customer " + customer.name)
  # billing logic goes here

def outstanding(db):
  return filter(lambda customer: customer.balance < 0, db.customers)

def sendBills(log, db):
  log("Sending monthly bills")
  list(map(lambda customer: bill(log, customer), outstanding(db)))
```

This is a reasonable approach, but may become unwieldy as we add more of these
[cross-cutting concerns](https://en.wikipedia.org/wiki/Aspect-oriented_programming).
Language features like currying and default arguments can help rein in the
*external* API, but still don't help with the *internal* implementation; e.g.
we could curry `bill` and `sendBills` with a default `log`, or use a default
argument to achieve a similar thing; but internal calls (like `sendBills`
calling `bill`) would still need to pass everything along explicitly, in case
the default has been overridden.

In contrast, dynamic variables can be defined by a library and referenced without
much/any ceremony, and overridden as needed by callers. For example, in Racket:

```scheme
;; Dynamic variable for logging procedure; defaults to printing on stderr
(define logger (make-parameter eprintf))

;; Shorthand for getting logger and applying it to a string
(define (log msg)
  ((logger) msg))

(define (bill customer)
  (log (string-append "Billing customer " (hash-ref customer 'name)))
  ;; billing logic goes here
  )

(define (outstanding db)
  (filter (lambda (customer) (< (hash-ref customer 'balance) 0))
          (hash-ref db 'customers)))

(define (sendBills db)
  (log "Sending monthly bills")
  (map bill (outstanding db)))
```

This defines a dynamic variable `logger` (known as a
["parameter"](https://docs.racket-lang.org/reference/parameters.html) in
Racket), which defaults to the `eprintf` procedure for printing on stderr. We
can log using this parameter via `((logger) "my string")`, but this looks a bit
weird so I've defined a more conventional `log` procedure too.

Applications and other libraries can call our functions as-is to use the default
logger, or they can override it by wrapping their code in `parameterize`, e.g.

```racket
(parameterize ((logger my-custom-logger))
  (log "About to send bills")
  (sendBills myDb)
  (log "Finished sending bills"))
```

Note that Racket's stdio system is actually defined using parameters, so this
example is a bit redundant! Instead, we could just write our log messages to the
`current-error-port`, which is already a parameter:

```scheme
;; No need for any 'logger' parameter

;; Logging just writes strings to current-error-port
(define (log msg)
  (display msg (current-error-port)))

;; bill, outstanding and sendBills remain the same as above

(parameterize ((current-error-port my-custom-port))
  (log "About to send bills")
  (sendBills myDb)
  (log "Finished sending bills"))
```

I'm not just being a SmugLispWeenie either, since Scala also uses dynamic
scope for its stdio, via the
[`Console` object](https://www.scala-lang.org/api/current/scala/Console$.html).
For example:

```scala
def log(msg: String): Unit = Console.err.println(msg)

// billing definitions go here

Console.withErr(myStderrStream) {
  Console.err.println("About to send bills")
  sendBills(myDb)
  Console.err.println("Finished sending bills")
}
```

#### Web Services ####

Recently I've been using dynamic variables in Scala for managing connections to
Web services like AWS. This involves more complex APIs than the 'fire and
forget' of logging, as well as static types.

Here's a simple example of a key/value store referenced by a dynamic variable:

```scala
object KeyValueStore {
  import scala.util.DynamicVariable

  trait Store {
    def get(k: Key        ): Try[Option[Val]]
    def put(k: Key, v: Val): Try[Unit       ]
  }

  // Dynamic variable, referenced via `store.value` and overridden via
  // `store.withValue(myStore)(myExpression)`. We use 'Try' to represent network
  // errors, etc. which also permits an always-failing default.
  val store = new DynamicVariable[Try[Store]](
    Failure(new AssertionError("No KeyValueStore in scope"))
  )

  // Shorthand for using the current store. Scala treats `X(...)` as shorthand
  // for `X.apply(...)`, so this lets us look up keys using `KeyValueStore(...)`
  def apply(k: Key        ): Try[Option[Val]] = store.value.flatMap(_.get(k   ))
  def   put(k: Key, v: Val): Try[Unit       ] = store.value.flatMap(_.put(k, v))

  // Create a new scope where 'store' calls out to AWS SSM; evaluate 'x' within
  // that scope and returns its value ('=> T' indicates call-by-name)
  def withSSM[T](ssm: Try[AWSSimpleSystemsManagement], x: => T) =
    store.withValue(ssm.map(ssm => new Store {
      def get(k: Key        ) = /* Get using SSM client */
      def put(k: Key, v: Val) = /* Store using SSM client */
    }))(x)

  // Create a new scope where 'store' is backed by a HashMap, initialised with
  // the given defaults. Useful for testing.
  def withTempStore[T](init: Map[Key, Val], x: => T) = store.withValue(Success({
    val store = HashMap.empty ++ init
    new Store {
      def get(k: Key        ) = Success(store.get(k   )             )
      def put(k: Key, v: Val) = Success(store.put(k, v).pipe(_ => ())
    }
  }))(x)
}
```

#### Environment Variables ####

Interestingly, *the environment itself* is often a good use-case for a dynamic
variable. As shown above, Racket accesses env vars via a dynamic variable called
`current-environment-variables`, and I've implemented Scala code similar to the
`KeyValueStore` above for overriding the environment (*without* the `put`, since
I prefer to keep all env vars immutable, as mentioned above).

Whilst shells don't have an explicit variable for "the current environment",
they do *mostly* treat env vars in the same way as "normal" dynamic variables
(AKA "shell variables"); the main difference is that shell variables aren't
inherited by subprocesses. For example:

```bash
#!/usr/bin/env bash

myEnv() {
    env | grep FOO || echo "FOO is not in env"
}

printFoo() {
    echo "BEGIN $*"
    myEnv
    echo "FOO is $FOO"
}

FOO=foo

printFoo A

FOO=bar printFoo B

printFoo C
```

This script outputs:

```
BEGIN A
FOO is not in env
FOO is foo
BEGIN B
FOO=bar
FOO is bar
BEGIN C
FOO is not in env
FOO is foo
```

The function calls labelled `A` and `C` are using the shell variable `FOO`, which
has a value of `foo` and doesn't appear in the environment of subprocesses (like
the `env` command, which prints out its environment). For the call labelled `B`
we override this shell variable, turning it into an environment variable which
*is* visible to the `env` command (hence the `FOO=bar` line extracted by
`grep`). They otherwise look pretty much the same; plus, reading a variable
which isn't defined *anywhere* in our script will "fall back" to checking the environment (options like `set -u` come in handy for catching failures here!).

**SIDE NOTE:** I didn't realise until I was writing this that overriding
variables works for function calls in bash as well as "normal" subprocesses!

## A Word on Threading ##

Threading is a Worse Is Better approach to concurrency and parallelism, since it
*appears* rather straightforward at first glance, whilst subtly undermining the
semantics of the language, and hence the assumptions made by our mental models.

Threading can interfere with dynamic variables, if they use a "thread-local"
implementation. Racket takes care to ensure its dynamic variables ("parameters")
have consistent values, even if code is moved between threads; however, its
mutation form `(my-parameter my-new-value)` will have problems across threads
(yet another reason to avoid mutation and threads!).

The story in Scala is worse, since even *reading* a dynamic variable can break
in the presence of multithreading. In particular, the implementation of `DynamicVariable` only makes new scopes visible to the current thread and its
descendents. Using a *thread pool* to execute concurrent tasks will send some
tasks to old threads (to avoid the overhead of spinning up new threads), but
those might not have the correct dynamic scopes set up for the task. This can
cause values from incorrect scopes to creep in unexpectedly (and
non-deterministically!).

This is particularly annoying, since it prevents us using `Future` to wrap up
our Web services (similar to how I used `Try` in the `KeyValueStore` variable
above).

To avoid this, I've come up with the following alternative to `DynamicVariable`,
which forces new threads to be created whenever a dynamic variable gets a new
scope. This isn't the most efficient thing in the world, but it seems to work
nicely for my use-case. In particular:

 - We can use "real" implementations as our default values, wrapped in `Future`
 - Those `Future`s will begin executing immediately, which minimises the latency
   until we get working connections to required services.
 - `Future` can store errors (similar to `Try`), so connection problems won't
   crash the program.
 - These connections will only succeed when they have the correct credentials,
   which aren't available to tests, etc. (they require an `AWS_PROFILE` env var,
   since I have no "default" AWS credentials specified)
 - The only place where these dynamic variables are overridden is in tests,
   which is less performance critical than the "happy path" of the actual
   application.

```scala
import java.util.concurrent.Executors
import scala.concurrent.{
  ExecutionContext,
  ExecutionContextExecutorService,
}
import scala.util.DynamicVariable

final class StackedExecutionContext private() extends ExecutionContext {
  private def newPool = ExecutionContext.fromExecutorService(
    Executors.newCachedThreadPool()
  )
  private var active = newPool
  private var old    = List[ExecutionContextExecutorService]()

  def freshPool: Unit = this.synchronized {
    old    = (active.tap(_.shutdown)::old).filter(!_.isTerminated)
    active = newPool
  }

  def       execute(runnable: Runnable ): Unit =
    this.synchronized { active.execute      (runnable) }
  def reportFailure(cause   : Throwable): Unit =
    this.synchronized { active.reportFailure(cause   ) }
}

final object StackedExecutionContext {
  /** Use this to execute `Future` values which involve DynamicallyScoped */
  implicit val ec = new StackedExecutionContext
}

final class DynamicallyScoped[T] private(d: DynamicVariable[T]) {
  import StackedExecutionContext.ec.freshPool

  /** The value currently bound to this variable */
  def value: T = d.value

  /** Evaluate the given thunk, with the given value bound to this variable.
    * We also spawn a fresh thread pool, to ensure this value is propagated to
    * any new Futures.
    */
  def withValue[A](v: T)(x: => A): A = d
    .withValue(v)(freshPool.pipe(_ => x))
    .tap(_ => freshPool)
}

final object DynamicallyScoped {
  def apply[T](init: T) = new DynamicallyScoped(new DynamicVariable[T](init))
}
```

I was *very* wary when implementing this, so the test suite needed to be
pretty thorough to convince me that it works. I ended up implementing a toy
little programming language involving dynamic variables, futures and delays.
I then wrote two interpreters: a pure, simple, single-threaded interpreter
which I could be confident was correct; and an interpreter which uses
`DynamicallyScoped`, real `Future`s and a `StackedExecutionContext`. I used
ScalaCheck to generate arbitrary programs in this language, and tested whether
both interpreters gave the same result.

That's actually pretty cool on its own, and probably deserves a standalone
blog post!
