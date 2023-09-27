---
title: FP
---

> Also - OOP languages allow for dynamic dispatch and if I'm not totally
> mistaken (but I might), Haskell at least doesn't allow that unless you start
> adding language extensions, so you can't even make something like `Logger` a
> type class.

FP can absolutely do dynamic dispatch, since it's just a really limited form of
higher-order function. Languages like Java and C++ made a big deal about dynamic
dispatch because they didn't have first-class functions. For example, in Java:

``` java
abstract class Animal {
  private String myName;
  public abstract String greet(String name);
}

class Dog extends Animal {
  public Dog(String myName) {
    this.myName = myName;
  }

  public String greet(String name) {
    return "Woof, hello " + name + ", I am " + this.myName + "!";
  }
}

class Cat extends Animal {
  public Cat(String myName) {
    this.myName = myName;
  }

  public String greet(String name) {
    return "Meow, hello " + name + ", I am " + this.myName + "!";
  }
}

class App {
  public static main(String[] args) {
    List[Animal] pets = new ArrayList();
    pets.append(new Dog("Fido"));
    pets.append(new Cat("Mittens"));
    for (p : pets) {
      System.out.println(p.greet("John"));
    }
  }
}
```

The result will be `Woof, hello John, I am Fido! Meow, hello John, I am
Mittens!`

This is just a really complicated way of choosing between two particular
functions (barking or meowing). When we have first-class functions this sort of
thing becomes trival, e.g. here's a simple version in Haskell:

```haskell
-- Animal's public interface; just a single function in this example
type Animal = String -> String

-- Implements an Animal, using dog behaviour
dog myName name = "Woof, hello " ++ name ++ ", I am " ++ myName ++ "!"

-- Implements an Animal, using cat behaviour
cat myName name = "Meow, hello " ++ name ++ ", I am " ++ myName ++ "!"

-- Greet using the given Animal's behaviour. In this case it's redundant,
-- but for richer interfaces we can pull out and combine the bits we need
greet :: Animal -> String -> String
greet a name = a name

main = let pets :: [Animal]
           pets = [dog "Fido", cat "Mittens"]

           go p = putStrLn (greet p "John")
        in mapM go pets
```

Here we're just passing around the implementations of `greet` directly as
needed. In OOP terminology: methods are just instance variables. This isn't the
case in Java, C++, etc. since they couldn't treat functions/methods as
first-class values, so they invented things like vtables, etc. to work around
this deficiency.

If you'd rather have implementations looked up based on the type, rather than
passed around as a normal argument, then that's what type classes (ad-hoc
polymorphism) is for, e.g.

```haskell
-- The functionality we want to look up. I've made myName public, so
-- it's less trivial than just a single function.
data Animal = Animal { myName :: String, greet :: String -> String }

-- The types we'll use to look up Animal
data Dog = Dog String
data Cat = Cat String

-- The Animal functionality for each type
class IsAnimal t where
  asAnimal :: t -> Animal

instance IsAnimal Dog where
  asAnimal (Dog n) = Animal { n, bark }
    where bark name = "Woof, hello " ++ name ++ ", I am " ++ n ++ "!"

instance IsAnimal Cat where
  asAnimal (Cat n) = Animal { n, meow }
    where meow name = "Meow, hello " ++ name ++ ", I am " ++ n ++ "!"

main = let pets :: [Animal]
           pets = [asAnimal (Dog "Fido"), asAnimal (Cat "Mittens")]

           go p = putStrLn (greet p "John")
        in mapM go pets
```

Here `Animal` is analogous to the Java abstract class, `IsAnimal` is analogous
to `extends Animal` and `asAnimal` upcasts `Dog`, `Cat`, etc. to `Animal`. Note
that we can also make other `Animal`s, by providing an implementation of
`myName` and `greet` (equivalent to anonymous classes in Java).

There are language extensions which let us combine `IsAnimal` and `Animal` into
one typeclass, then use fancy types like:

```haskell
pets :: [forall t. IsAnimal t => t] = [Dog "Fido", Cat "Mittens"]
```

However, that's precisely the same as making a list of the class's methods (i.e.
`[Animal]` in the above example).

---

> Others have mentioned closures, too. I'm not sure if they mean something like:

> ...

> I mean, I guess that works somehow, but now we've made it possible to create
> user repositories with weird semantics

Yes, that's what they mean by using closures. As for 'weird semantics': there's
a question that we need to answer before choosing an approach: should there be a
single `UserRepository`, or can we create many? (the "Zero, One, Many" rule).
Your remark about 'weird semantics' makes it sound like there should only be
one; in which case, we shouldn't be allowFIXME

---

The closure approach is to write a function taking the desired arguments, and
define everything we need within that function's scope. For example:

```haskell
-- userRepository is just a function
userRepository logger baseDir = ...
  where
    -- The logger and baseDir are in scope here

    load :: ID -> IO User
    load id =
      -- Annoyingly we can't nest "where", so I've used let/in
      let loadFrom = ...
       in loadFrom baseDir id

    save :: User -> IO ()
    save = ...
```

We could put all of our logic in this function, analogous to putting it in the
`UserRepository` class, e.g.

```haskell
main = do
  let baseDir = File   "/some/Directory"
  let logger  = Logger "/some/Directory/logfile.log"
  -- userRepository contains the rest of the application logic
  userRepository baseDir logger
```

Alternatively, we could return some functions to use elsewhere:

```haskell
-- Functions are values, so we can return them in a tuple if we like.
userRepository logger baseDir = (load, save)
  where
    load = ...
    save = ...

main = do
  let baseDir = File   "/some/Directory"
  let logger  = Logger "/some/Directory/logfile.log"
  let (load, save) = userRepository baseDir logger  -- Unpacking is nicer than using 'fst' and 'snd'
  -- load and save are closures; they contain references to baseDir and logger, so
  -- we don't need to give them any extraneous arguments
  user <- load 1
  ...
```

Using tuples is a bit naff, since they depend on the order, and their accessors
have uninformative names like 'fst' and 'snd'. We can use a record datatype
instead:

```haskell
-- A data type storing two values 'load' and 'save', which happen to be functions.
data UserRepository = Repo { load :: ID -> IO User, save :: User -> IO () }

-- When we define a record, Haskell will automatically provide a function for
-- extracting each field. In this case we will get the following:
-- load :: UserRepository -> (ID   -> IO User)
-- save :: UserRepository -> (User -> IO ()  )

-- The parentheses are redundant, so these are equivalent to:
-- load :: UserRepository -> ID   -> IO User
-- save :: UserRepository -> User -> IO ()

-- Since the names 'load' and 'save' have already been used, we should use
-- different names for our local definitions here (name clashes are a common
-- complaint about Haskell records!)
userRepository logger baseDir = Repo { myLoad, mySave }
  where
    myLoad :: ID -> IO User
    myLoad id = let loadFrom = ...
                 in loadFrom baseDir id

    mySave :: User -> IO ()
    mySave = ...

main = do
  let baseDir = File   "/some/Directory"
  let logger  = Logger "/some/Directory/logfile.log"
  let repo    = userRepository baseDir logger
  -- We need to pass 'repo' to 'load' and 'save'; equivalent to using 'fst' and 'snd' on a tuple
  user <- load repo 1
  ...
```

This seems pretty similar to your hypothetical "module" example.

---

> Still - I don't know whether it's just due to unfamiliarity, but it seems to
> me that the simplicity and readability of the "OOP" equivalent (not requiring
> you to understand the semantics of "map" and "join" in the context of Reader),
> are a bit better.

Reader is really just a function, and its 'map' is just function composition, so
those are probably quite familiar and the Reader terminology is unnecessary
baggage. The real trick to Reader is in 'product' and 'join', where we combine
two functions into one by re-using an argument; this is how we send the baseDir
and logger around to where they're needed. Again, this is all just normal
functions at the end of the day. We could do it in OOP, but it would probably
look clunky.

The main reason to use Reader is so we can use 'do' notation (or for/yield in
Scala). I didn't know if that would be too confusing, but since you know it
here's what Reader would actually look like ('do' takes care of calling map,
join, etc. for us):

```haskell
type App = Reader (Logger, File)

baseDir :: App File
baseDir = do
  (_, d) <- ask  -- 'ask' is Haskell's version of what I called 'read'
  return d       -- 'return' is Haskell's version of what I called 'wrap'

logger :: App Logger
logger = do
  (l, _) <- ask
  return l

load :: ID   -> App User
load id = do
    d <- baseDir
    loadFrom d id
  where loadFrom = ...

save :: User -> App ()
save = ...

-- We can write our application in terms of App, which is nice if we don't need IO
app :: App ()
app = do
  user <- load 1
  ...

-- Use runReader to get a pure result, then use 'return' to make it IO
main = return (runReader (File "/some/Directory", Logger "/some/Directory/logfile.log"))
```

I've used App instead of IO above; presumably a real application would need to
perform IO as well. That would require a bit more plumbing. If you don't mind
getting more complicated, one common approach is to define App as a 'monad
transformer stack' containing 'Reader (Logger, File)' and 'IO', which would let
us combine Reader actions with IO actions:

```haskell
do
  d <- baseDir
  c <- listDir d
  putStrLn ("Base dir contains " ++ show (length c) ++ " entries")
```

---

I've never actually used transformer stacks. Instead, I like to use 'free
monads', which would look something like this:

```haskell
-- Define a datatype describing the different actions we want
data App m a where
  BaseDir   :: App m File
  GetLogger :: App m Logger
  Load      :: ID -> App m User
  Save      :: User -> App m ()
  ...

-- Magic TemplateHaskell macro from polysemy library. This will create
-- functions like:
--   baseDir   :: Member App r => Sem r File
--   getLogger :: Member App r => Sem r Logger
--   load      :: Member App r => ID -> Sem r User
--   save      :: Member App r => User -> Sem r ()
--
-- The type 'Sem r' is a "set of effects", and 'Member App r' constrains
-- that set to include App.
makeSem ''App

-- Define an 'interpreter' for implementing App actions. The type says
-- we're removing App from a set of effects, as long as that set also
-- contains IO. Note that 'Embed' and 'embed' just make Haskell's normal
-- IO type compatible with Sem.
runAppIO :: Member (Embed IO) r => File -> Logger -> Sem (App ': r) a -> Sem r a
runAppIO d l = interpret (embed . go)
  where
    go :: App m a -> IO a
    go BaseDir   = return d
    go GetLogger = return l
    go (Load id) = loadFrom d id
    go (Save u)  = ...

    loadFrom = ...

app :: Member App r => Sem r ()
app = do
  user <- load 1
  ...

main = do
  let baseDir = File   "/some/Directory"
  let logger  = Logger "/some/Directory/logfile.log"
  runM (runAppIO baseDir logger app)
```

The reason I like this approach is that by using a 'set of effects' we can split
up our actions into lots of distinct types, e.g. I might do this:

```haskell
type Error = String  -- For simplicity

-- Logging actions
data Log m a where
  Log   :: String -> Log m ()
  Debug :: String -> Log m ()
  Warn  :: String -> Log m ()

-- Filesystem operations

data Input m a where
  ReadFile :: File -> Input m (Either Error String)

data Output m a where
  SaveFile :: File -> String -> Output m (Option Error)

makeSem ''Log
makeSem ''Input
makeSem ''Output

-- Shorthands, for specific combinations of effects

type In    a = forall r. (Member Input r                               ) => Sem r a
type Out   a = forall r. (                Member Output r              ) => Sem r a
type InOut a = forall r. (Member Input r, Member Output r              ) => Sem r a
type App   a = forall r. (Member Input r, Member Output r, Member Log r) => Sem r a

-- Now we can be really specific about the effects that each function uses

dbg :: Show a => a -> Log ()
dbg x = debug (show x)

load :: ID -> In (Either Error User)
load id = do
  let filename = File ("users/" ++ show id)
  result <- readFile filename
  return (case result of
    Left  err     -> Left ("Failed to read file for user " ++ show id ++ ": " ++ err)
    Right content -> case parseUser content of
      Left  err -> Left ("Failed to parse " ++ show filename ++ ": " ++ err)
      Right u   -> Right u)

save :: User -> Out (Option Error)
save u = do
  let filename = File ("users/" ++ show (userId u))
  saveFile filename (serialiseUser u)

-- We can mix and match these functions; their effects accumulate in the type

updateUser :: (User -> User) -> ID -> InOut (Option Error)
updateUser f id = do
  got <- load id
  case got of
    Left  err -> return (Just err)
    Right u   -> save (f u)

-- Write our application using these types
app :: App ()
app = do
  ...

-- Specify how to interpret each effect

runLogIO :: Member (Embed IO) r => Logger -> Sem (Log ': r) a -> Sem r a
runLogIO l = interpret (embed . go)
  where
    go :: Log m a -> IO a
    go (Log   msg) = logWith l Info    msg
    go (Debug msg) = logWith l Debug   msg
    go (Warn  msg) = logWith l Warning msg

runInIO :: Member (Embed IO) r => File -> Sem (Input ': r) a -> Sem r a
runInIO d = interpret (embed . go)
  where
    go :: Input m a -> IO a
    go (ReadFile f) = readContents (joinPath d f)  -- Also do error handling here

runOutIO :: Member (Embed IO) r => File -> Sem (Output ': r) a -> Sem r a
runOutIO d = interpret (embed . go)
  where
    go :: Output m a -> IO a
    go (SaveFile f content) = writeContents (joinPath d f) content  -- Also do error handling here

-- Run our application, using these interpreters
main = do
  let baseDir = File   "/some/Directory"
  let logger  = Logger "/some/Directory/logfile.log"
  runM (runLogIO logger (runInIO baseDir (runOutIO baseDir app)))
```

This takes a bit more effort than just calling IO actions directly. However,
we've gained a bunch of nice things:

 - Each piece of logic can be constrained to only those effects it needs;
   e.g. adding debug logging to a tricky calculation doesn't let it overwrite
   files in the base dir.

 - All of the actual IO is in the interpreters; our application logic just
   describes what it wants to do.

 - The dependencies (logger and baseDir) only exist in the interpreters; the
   application logic doesn't even know they exist.

 - In particular, the application can log strings, but has no knowledge of the
   underlying Logger; hence there's less that can go wrong (e.g. it can't do
   silly things like switch the logfile, or whatever else Loggers can do).

 - I've also made the In and Out interpreters prepend the baseDir to all
   paths. This way, the application logic is restricted to that dir, without
   having to know or care. (Of course a real implementation would check for
   things like '..' too).

Having the application logic isolated from underlying details isn't just nice
for simplifying and eliminating mistakes in the application. It also lets us
write multiple interpreters, which implement the effects in different ways. For
example we could make an interpreter for Log which doesn't have a Logger at all,
and just writes to stdout instead:

```haskell
runLogStdIO :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
runLogStdIO = interpret (embed . go)
  where
    go :: Log m a -> IO a
    go l = case l of
      Log   msg -> putStrLn msg
      Debug msg -> putStrLn ("[debug] " ++ msg)
      Warn  msg -> putStrLn ("[warn] "  ++ msg)
```

Of course, we don't need to go as far as using IO. We can make a StdOut effect
instead:

```haskell
data StdOut m a where
  Stdout :: String -> StdOut m ()

-- Interpreter for writing StdOut messsages to this process's stdout handle
runStdOutIO :: Member (Embed IO) r => Sem (StdOut ': r) a -> Sem r a
runStdOutIO = interpret (embed . go)
  where
    go :: StdOut m a -> IO a
    go (Stdout msg) = putStr msg
```

Now we can write an interpreter which turns Log effects into StdOut effects,
without using IO:

```haskell
runLogStdOut :: Member StdOut r => Sem (Log ': r) a -> Sem r a
runLogStdOut = interpret go
  where
    go :: Log m a -> StdOut m a
    go l = case l of
      Log   str -> Stdout str
      Debug str -> Stdout str
      Warn  str -> Stdout str
```

What if we want to go the other way, and send stdout messages to a log
(e.g. when running a CLI app on a server)? We can do that too:

```haskell
runStdOutLog :: Member Log r => Sem (StdOut ': r) a -> Sem r a
runStdOutLog = interpret go
  where
    go :: StdOut m a -> Log m a
    go (Stdout msg) = Log msg  -- StdOut doesn't track severity, so make everything Log
```


We can also define dummy interpreters for use in tests, e.g. accumulating stdout
and log messages to a list; using a HashMap instead of reading and writing
files; etc.

This is obviously a more hardcore approach, but I've found it really useful
(e.g. in https://github.com/Warbo/y-monad )
