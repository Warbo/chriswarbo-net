---
title: Controlling Side-Effects
---
This is an explanation I gave [on Hacker News](https://news.ycombinator.com/item?id=8691289) about pure functional programming, in languages like Agda. The quotes are from [this comment](https://news.ycombinator.com/item?id=8690573) left by the user [pron](https://news.ycombinator.com/user?id=pron)

> Languages that use types for elaborate proofs always seem to me like they only prove the most uninteresting properties of programs, namely the parts that deal with data transformations.

> At the end of the day, programs are written for their side effects

The reason for such a focus on data transformation is that it's very easy to do in these purely functional languages (Agda included). It's so easy, in fact, that "other" things, like side effects, event handling, etc. are represented as data transformations. This is most obvious in Haskell, since laziness decouples definition from computation; eg. making it trivial to represent event streams as infinite lists.

> What I'm most interested in is proofs that, say, in a concurrent environment, a function that closes a socket will never be called as long as there are pending tasks to write to the socket.

The usual approach is to define a datatype of operations you might want to perform (eg. `Write String`, `Read Length`, etc.). Next you define a datatype which can combine these operations together in ways you may want (eg. something like a rose tree, if you want threads firing off threads). This forms an "embedded domain-specific language". We then write a bunch of helper functions for manipulating these datastructures (eg. a "combinator library"), then we use these to write down what we actually want as a combination of operations.

Next we write an interpreter function which opens a socket, performs all of the operations in the tree (possibly in multiple threads), waits for them to finish then closes the socket.

> Or, because I write concurrent data structures, I'm interested to prove that a certain function will eventually release all locks it acquires. Are there any such languages?

You can do the same thing as with the socket example, except you can strengthen the type of your program (operation-combining) datastructure to enforce that locks are released. As a simplified example, we can force a serial computation to release locks by only allowing AQUIRE and RELEASE to be inserted together:

```haskell
data Op = Foo | Bar | Aquire ID | Release ID

data SafeOp = SFoo | SBar

data Program : Type where
  NoOp       : Program                        -- Empty Program
  WithLock   : ID -> Program                  -- Make a Program aquire and release a lock
  PrefixOp   : SafeOp  -> Program -> Program  -- Add a non-lock Op to a Program
  Interleave : Program -> Program -> Program  -- Combine two Programs

-- Part of the interpreter, not exposed to the world
progToOps : Program -> [Op]
progToOps NoOp               = []
progToOps (WithLock id p)    = [Aquire id] ++ progToOps p ++ [Release id]
progToOps (PrefixOp SFoo p)  = [Foo] ++ progToOps p
progToOps (PrefixOp SBar p)  = [Bar] ++ progToOps p
progToOps (Interleave p1 p2) = interleave (progToOps p1) (progToOps p2)

interleave []     ys = ys
interleave (x:xs) ys = [x] ++ interleave ys xs
```

Notice that `progToOps`{.haskell} can never output a list containing `Aquire`{.haskell} without also containing a `Release`{.haskell} with the same `ID`{.haskell}. Also notice that we can define arbitrary combinations of the other `Ops`{.haskell}:

 - `[]`{.haskell} is `NoOp`{.haskell}
 - `[Foo, Bar]`{.haskell} is `PrefixOp SFoo (PrefixOp SBar NoOp)`{.haskell}
 - `[Foo, Aquire A, Bar, Aquire B, Release A, Foo, Release B]`{.haskell} is `Interleave (PrefixOp SFoo (PrefixOp SBar NoOp)) (Interleave (WithLock A NoOp) (WithLock B (PrefixOp SFoo NoOp)))`{.haskell}

Of course these datastructures would be build up by helper functions instead of by hand, would be tree-like for concurrency, would probably provide guarantees per sub-tree, would allow arbitrary functions (of some suitable type) in place of `Foo`{.haskell}, `Bar`{.haskell}, etc.

> But in your example the program doesn't represent the code itself but a program written in some new language.

That's the key idea ;)

If I write some Java like `DB.connect(credentials).select("users").where("name", "Kevin")`{.java}, am I using "some new language"? As far as Alan Kay is concerned, yes; that was one of his [main inspirations for OOP](http://www.purl.org/stefan_ram/pub/doc_kay_oop_en):

> My math background made me realize that each object could have several algebras associated with it, and there could be families of these, and that these would be very very useful.

The only difference between my Java example and my earlier Haskell/Agda example is that the functional version operates in two phases: calculating which operations to perform (building a `Program`{.haskell} containing `Ops`{.haskell}) is separate to performing those operations (applying an interpreter function to a `Program`{.haskell}).

However, keep in mind that we're not doing imperative programming, so there's is no inherent notion of time: we get the same result no matter which order we evaluate stuff in (that's the [Church-Rosser Theorem](http://en.wikipedia.org/wiki/Church%E2%80%93Rosser_theorem)). Hence these two "phases" are *logically* distinct (defined separately), but not necessarily *temporally* distinct (running separately). In practice, we tend to define our `Program`{.haskell} lazily, so it gets constructed on-demand by the interpreter.

This is a bit like having a `main`{.c} loop containing a `switch`{.c}, for example (forgive mistakes; I've never used function pointers in C):

```c
void main(op* instruction, int* length, void* callback) {
    socket* s = set_up_socket();
    int close = 0;
    char* data;
    while (!close) {
        switch(*instruction) {
            case READ:
                // Read logic
                data = read_from_socket(s, *length);
                *callback(instruction, data);  // Update instruction based on data
                break;
            case WRITE:
                // Write logic
                data = *callback(instruction);  // Update instruction and return data
                write_to_socket(*length, data);
                break;
            case CLOSE:
                // Close logic (ignores any other instructions)
                close = 1;
                break;
    }
    close_socket(s);
}
```

This `main`{.c} function is basically the same as our interpreter; it's keeping the dangerous socket-related stuff in one place, providing some safety that the socket will be closed after use, etc. The values of `data`{.c} and `instruction`{.c} are being computed by arbitrary C code living at `callback`{.c}, just like our `Program`{.haskell} can be computed by arbitrary Haskell/Agda/whatever code. In this case the interleaving of choosing and executing instructions is explicit, but a lazily-evaluated `Program`{.haskell} will behave similarly in practice.

Does this mean that those callbacks are in "some different language" to C? No; everything is regular C except that we just-so-happen to be labelling some ints as `READ`{.c}, `WRITE`{.c} and `CLOSE`{.c}. Likewise, in Haskell/Agda/etc. we have the full breadth of the whole language available to us; we just-so-happen to be returning values of type `Op`{.haskell} and `Program`{.haskell}. All of our logic, concurrency, etc. is done in the "host" language; we only define `Op`{.haskell}s for the things we want to restrict, like `Read`{.haskell} and `Write`{.haskell}; there's absolutely no point defining operations for boolean logic, arithmetic, string manipulation, arrays/lists, etc. because that's available already. Of course, once we've finished defining our socket library, that too will be available to everyone (if we release it); just because it might use some crazy `Op`{.haskell}, `Program`{.haskell} and `runSocket`{.haskell} things internally doesn't mean anyone has to care about that; it's just an implementation detail.

Notice that this is very similar to an OOP dynamic dispatch system, where `READ`{.c}, `WRITE`{.c} and `CLOSE`{.c} are method names and the callbacks just-so-happen to be kept together in a struct which we call an "object".

Just like Alan Kay said.
