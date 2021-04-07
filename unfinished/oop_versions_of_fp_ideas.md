Some of the ideas used in Functional Programming are quite alien to those who
are used to Procedural or Object Oriented Programming. Some of these differences
are quite profound, but many are just mismatches in terminology.

Consider the following Java-like interfaces:

    interface Combinable<T> {
      // Combine another value with this one.
      // Nesting shouldn't make a difference, e.g.
      //   T x = combine(foo, combine(bar, baz));
      //   T y = combine(combine(foo, bar), baz);
      //   x == y
      public static T combine(T x, T y);
    }

    interface Empty<T> extends Combinable<T> {
      // An "empty" value of this type, such that
      //   combine(foo, empty) == foo
      //   combine(empty, foo) == foo
      public static T empty;
    }

    interface Mappable<F> {
      // Use the given function to transform As into Bs
      // This shouldn't change the structure, e.g.
      //   map(foo, x => x) == foo
      //   map(map(foo, g), f) == map(foo, x => f(g(x)))
      public static F<B> map(F<A> x, Function<A, B> f);
    }

    interface Wrappable<F> extends Mappable<F> {
      // Wrap a value in a mappable structure, such that
      //   map(wrap(x), f) == wrap(f(x))
      public static F<A> wrap(A x);
    }

    interface Mergable<F> extends Wrappable<F> {
      // Merge the structure of two values together, e.g.
      //   merge(wrap(foo), wrap(bar)) == wrap(new Pair(foo, bar))
      public static F<Pair<A, B>> merge(F<A> x, F<B> y);
    }

    interface Collapsable<F> extends Mergable<F> {
      // Collapse nested Mergables together, e.g.
      //   collapse(wrap(wrap(x))) == wrap(x)
      public static F<A> collapse(F<F<A>> x);
    }

Note that I've made everything 'static' so they correspond more closely to the functional versions; in a "proper OOP" implementation only `empty` and `wrap` would be static.

Even in this 'more familiar' style, these are probably still hard to understand on their own. That's why examples are so important! In fact, all of these are familar aspects of lists that we use every day; they've just been generalised beyond recognition! Here's what each of these mean in terms of lists:

    class List<T> implements Empty<List<T>>, Collapsable<List> {
      // "combine" just appends two lists
      public static List<T> combine(List<T> x, List<T> y) {
        List<T> result = new ArrayList<T>();
        result.addAll(x);
        result.addAll(y);
        return result;
      }

      // "empty" is just an empty list
      public static List<T> empty = new ArrayList<T>();

      // "map" applies the given function to each element
      public static List<B> map(List<A> x, Function<A, B> f) {
        List<B> result = new ArrayList<B>();
        for (A elem : x) {
          result.add(f.apply(elem));
        }
        return result;
      }

      // "wrap" produces a single-element list
      public static List<T> wrap(T x) {
        List<T> result = new ArrayList<T>);
        result.add(x);
        return result;
      }

      // "merge" is the "cartesian product" (AKA "SQL join"): we pair
      // up everything in the first list with everything in the second
      public static List<Pair<A, B>> merge(List<A> x, List<B> y) {
        List<Pair<A, B>> result = empty;
        for (A elemA : x) {
          for (B elemB : y) {
            result = combine(result, wrap(new Pair(elemA, elemB)));
          }
        }
        return result;
      }

      // We can "collapse" a list-of-lists by appending them together
      public static List<T> collapse(List<List<T>> x) {
        List<T> result = empty;
        for (List<T> subList : x) {
          result = combine(result, subList);
        }
        return result;
      }
    }

Hopefully those implementations are straightforward (although our motivation for defining these things may be unclear!). Here's how these relate to functional programming terminology:

 - "Combinable" is what functional programmers call a "Semigroup" ("combine" might be written "append", "mappend", "++", "<>")

 - "Empty" is called "Monoid" (it's a sub-type of Semigroup; "empty" might be written "mempty" or "identity" or "zero")

 - "Mappable" is called "Functor"; "map" may be written "fmap" in Haskell for historical reasons.

 - "Wrappable" may be called "Pointed" (as in 'points in a space'). "wrap" may be called "pure" (or "return", which may be confused for a keyword!)

 - "Mergable" is called an "Applicative" or "applicative functor". The "merge" function may be called "product". It's common for 'applicative' to include 'wrap' (AKA 'pure'), rather than having it as a separate 'pointed' interface. It's also common to see the following function instead of 'merge'/'product' (it's equivalent, but might be more confusing):

    public static F<B> app(F<Function<A, B>> f, F<A> x);
    // app(f, foo) == map(merge(f, foo), p => p[0].apply(p[1]))
    // merge(foo, bar) == app(wrap(x => new Pair(foo, x)), bar)

 - "Collapsable" is known as "Monad". "collapse" is usually called "join". Sometimes 'wrap'/'pure' is included in 'Monad', e.g. in Haskell this is done for historical reasons (Haskell had Functor and Monad before Applicative was put in between). Sometimes Monad defines a function called "bind" or ">>=" instead of "join" (which is equivalent, but might be more confusing):

    public static F<B> bind(F<A> x, Function<A, F<B>> f);
    // bind(foo, f) == join(map(foo, f))
    // join(foo) == bind(foo, x => x)

So what's the motivation behind doing this? These ideas are useful all the time when we're dealing with lists (e.g. appending, mapping, concatenating, etc.), so it can be useful to notice when we can use them with other sorts of values.

The most obvious example is other sort of 'collections', e.g. key/value mappings, sets, strings, etc. This is where the distinctions become useful:

 - Sets are Combinable (union) and Empty (empty set), but they're not Mappable. This is because the 'f(g(x))' requirement might be broken by the set's equivalence relation (used to tell whether two elements are "the same"). For example, if a set of integers {0, -2, 3} has the equivalence relation `(x, y) => x.abs == y.abs`, if we apply the function 'x => x+1' to all the elements we get {1, 4} (since -2 becomes -1, which is equivalent to the existing 1); if we apply that function again we get {2, 5}. If we instead do both applications at once we get '{2, 0, 5}'. This violates the requirements, so sets aren't a valid Mappable.

 - Key/value mappings are Combinable (append their contents, with collisions either getting overwritten or discarded); Empty (an empty map); and Mappable (transforming their values); yet they're not Wrappable (there's no key to associate with the given value). Due to the hierarchy this also means they're not Mergable or Collapsable; this may take some pondering, but keep in mind that the key type is generic, so we don't have any way to resolve the ambiguities that crop up.

 - However, a key/value mapping where the keys implement Empty *is* wrappable, since 'wrap' can use 'empty' as the key! We can also use 'combine' to resolve collisions, which is

One obvious example is 'Maybe'/'Option', which we can think of a list containing at most one element.
