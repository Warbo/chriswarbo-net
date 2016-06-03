---
title: Premature Optimisation
---

> premature optimization is the root of all evil (or at least most of it) in programming
> -- [Donald Knuth](https://en.wikiquote.org/wiki/Donald_Knuth#Computer_Programming_as_an_Art_.281974.29)

I've been throwing together a bunch of experiments recently, for manipulating syntax trees ([s-expressions](https://en.wikipedia.org/wiki/S-expression)). Naturally, this is well-suited for a [Lisp](https://lispers.org) [dialect](http://www.schemers.org), so I'm using [Racket](https://racket-lang.org).

My Scheme is a little rusty, and I'm throwing away a lot of code as I investigate the efficacy of different approaches, and cobble together test suites to exercise them. Hence, I'm writing a whole lot of redundant, inefficient code.

## Background

One transformation I want to perform is removing redundant function definitions. For example, let's say we're given some s-expressions like these:

```scheme
(define-fun-rec
  (par (a)
    (append ((x (list a))
             (y (list a)))
            (list a)
      (match x
        (case nil         y)
        (case (cons z xs) (cons z (append xs y)))))))

(define-fun-rec
  (par (a)
    (join-together ((start (list a))
                    (end (list a)))
                   (list a)
      (match start
        (case nil                 end)
        (case (cons element rest) (cons element (join-together rest end)))))))

(define-fun
  (par (a)
    (join-backwards ((x (list a))
                     (y (list a)))
                    (list a)
      (join-together x (reverse y)))))

(define-fun
  (par (a)
    (repeat ((x (list a)))
            (list a)
      (append x x))))
```

The `append` and `join-together` functions are clearly alpha-equivalent (i.e. they're the same except for their choice of local variable names), so we can throw one of them away. However, the `join-backwards` function is using `join-together` and the `repeat` function is using `append`, so we need to make sure these references are updated. Let's say we keep `append` (because it appears first) and throw away `join-together`. The code we want to end up with is:

```scheme
(define-fun-rec
  (par (a)
    (append ((x (list a))
             (y (list a)))
            (list a)
      (match x
        (case nil         y)
        (case (cons z xs) (cons z (append xs y)))))))

(define-fun
  (par (a)
    (join-backwards ((x (list a))
                     (y (list a)))
                    (list a)
      (append x (reverse y)))))

(define-fun
  (par (a)
    (repeat ((x (list a)))
            (list a)
      (append x x))))
```

So, how do we figure this out automatically? Normally I would solve this by using [de Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index) everywhere, but transforming into that form is tricky, as it requires keeping track of the context. Instead, since each function definition appears in its entirety, we can use a very simple bottom-up renaming strategy:

 - We pick a standard format for variable names, which doesn't overlap with any existing variables. For example `"normalise-variable-<N>"` where `<N>` denotes a natural number.
 - To transform an expression which introduces a variable, for example the expression `(par (a) foo)`:
    - We transform the scoped expression, in this case `foo`, to get `foo2`.
    - We find the largest `N` in the variable names used in `foo2`, and call it `M`.
    - We replace all occurrences of `a` in `foo2` with `"normalise-variable-<M+1>"`, to get `foo3`.
    - The result will be `(par ("normalise-variable-<M+1>") foo3)`.
 - To transform an expression which doesn't introduce a variable, for example a function call `(f x y)`, we just transform each component individually.
 - Finally, replace the name of the definition to use a standard format, e.g. `"defining-function-<N>"`, along with any recursive references it makes.

In the above examples, we will get:

`append`:

```scheme
(define-fun-rec
  (par (normalise-var-5)
    (defining-function-1 ((normalise-var-4 (list normalise-var-5))
                          (normalise-var-3 (list normalise-var-5)))
                         (list normalise-var-5)
      (match normalise-var-4
        (case nil normalise-var-3)
        (case (cons normalise-var-2 normalise-var-1) (cons normalise-var-2 (defining-function-1 normalise-var-1 normalise-var-3)))))))
```

`join-together`:

```scheme
(define-fun-rec
  (par (normalise-var-5)
    (defining-function-1 ((normalise-var-4 (list normalise-var-5))
                          (normalise-var-3 (list normalise-var-5)))
                         (list normalise-var-5)
      (match normalise-var-4
        (case nil normalise-var-3)
        (case (cons normalise-var-2 normalise-var-1) (cons normalise-var-2 (defining-function-1 normalise-var-1 normalise-var-3)))))))
```

`join-backwards`:

```scheme
(define-fun
  (par (normalise-var-3)
    (defining-function-1 ((normalise-var-2 (list normalise-var-3))
                          (normalise-var-1 (list normalise-var-3)))
                         (list normalise-var-3)
      (join-together normalise-var-2 (reverse normalise-var-1)))))
```

`repeat`:

```scheme
(define-fun
  (par (normalise-var-2)
    (defining-function-1 ((normalise-var-1 (list normalise-var-2)))
                         (list normalise-var-2)
      (append normalise-var-1 normalise-var-1))))
```

As you can see, `append` and `join-together` give the same result when normalised in this way, and hence they are alpha-equivalent and we can replace one with the other. They do *not* give the same result as `join-backwards` or `repeat`, which are also distinct from each other, so there are no more alpha-equivalent definitions to discard from this set.

## The Problem

Although this works, unfortunately my naive Racket implementation was pretty slow. Measuring its runtime on one particular file gave `2m30.439s`. Since I have a bunch of tests around these scripts, which try various different edge-cases, that was tediously slow!

### Solution 1

I noticed that one of my loops had the following pattern:

```bash
while read -r LINE
do
  NORMAL_FORM=$(echo "$LINE" | ./getNormalForm.rkt)

  if EXISTING=$(echo "$PREVIOUSLY_SEEN" | grep -F "$NORMAL_FORM")
  then
    EXISTING_NAMES=$(echo "$EXISTING" | cut -f1 | ./getDefinedNames.rkt)
       THESE_NAMES=$(echo "$LINE"               | ./getDefinedNames.rkt)

    NAME_REPLACEMENTS=$(echo "$NAME_REPLACEMENTS";
                        paste <(echo "$THESE_NAMES") <(echo "$EXISTING_NAMES"))
  else
    PREVIOUSLY_SEEN=$(echo -e "$PREVIOUSLY_SEEN\n$LINE\t$NORMAL_FORM")
  fi
done
```

This loop goes through each definition (they've previously been collapsed down to one line each) and builds up two variables, each containing two tab-separated columns:

 - `PREVIOUSLY_SEEN` matches definitions to their normalised form.
 - `NAME_REPLACEMENTS` matches names we're discarding to their replacements which we're keeping (in the above example, we'd have a line `join-together\tappend`)

For each definition, if we've already seen its normalised form, we match all of its defined names to those of the previously-seen definition (a single definition might define multiple names, if they're mutually recursive) and append them to `NAME_REPLACEMENTS`. If not, we append this new definition on to `PREVIOUSLY_SEEN`.

The problem which struck me is that the first column of `PREVIOUSLY_SEEN` is the entire definition, which we must repeatedly extract the names from in the `then` branch. Why not extract the names once, and store those (space-delimited) instead of the whole definition?

That results in the following pattern, using `tr` to convert between spaces and newlines:

```bash
while read -r LINE
do
  NORMAL_FORM=$(echo "$LINE" | ./getNormalForm.rkt)
  THESE_NAMES=$(echo "$LINE" | ./getDefinedNames.rkt)

  if EXISTING=$(echo "$PREVIOUSLY_SEEN" | grep -F "$NORMAL_FORM")
  then
    EXISTING_NAMES=$(echo "$EXISTING" | cut -f1 | tr '\n' ' ')

    NAME_REPLACEMENTS=$(echo "$NAME_REPLACEMENTS";
                        paste <(echo "$THESE_NAMES") <(echo "$EXISTING_NAMES"))
  else
    SPACE_NAMES=$(echo "$THESE_NAMES" | tr '\n' ' ')
    PREVIOUSLY_SEEN=$(echo -e "$PREVIOUSLY_SEEN\n$LINE\t$NORMAL_FORM")
  fi
done
```

We've replaced two uses of `./getDefinedNames.rkt` with one, and it will only be called at most once per line, rather than being called again and again if there are many duplicates. How does this affect the timing?

```
2m46.696s
```

Uh oh, we're taking an extra 16 seconds, which is a 10% increase!

The reason is that the old version would run `./getDefinedNames.rkt` twice *per duplicate pair*. If there were multiple duplicates, it would run multiple times, but if there were *no* duplicates, it wouldn't run at all! The new version runs once per definition *regardless* of whether there are any duplicates, or how many there are.

Since, as I said at the beginning, this is very cobbled-together, I decided to stick with the new version rather than bother reverting things, since it's still the same order of magnitude. So, what else can we try?

### Solution 2

It's actually wasteful to keep calling these scripts using `./`, since they're using [nix-shell shebangs](/essays/nixos/nix_shell_shebangs.html) to ensure their dependencies (i.e. Racket) are available. If we make sure Racket is available in the outer script (again, using a nix-shell shebang) then we can bypass the shebangs of the inner scripts by invoking them with `racket`.

That makes our code look like this:

```bash
while read -r LINE
do
  NORMAL_FORM=$(echo "$LINE" | racket getNormalForm.rkt)
  THESE_NAMES=$(echo "$LINE" | racket getDefinedNames.rkt)

  if EXISTING=$(echo "$PREVIOUSLY_SEEN" | grep -F "$NORMAL_FORM")
  then
    EXISTING_NAMES=$(echo "$EXISTING" | cut -f1 | tr '\n' ' ')

    NAME_REPLACEMENTS=$(echo "$NAME_REPLACEMENTS";
                        paste <(echo "$THESE_NAMES") <(echo "$EXISTING_NAMES"))
  else
    SPACE_NAMES=$(echo "$THESE_NAMES" | tr '\n' ' ')
    PREVIOUSLY_SEEN=$(echo -e "$PREVIOUSLY_SEEN\n$LINE\t$NORMAL_FORM")
  fi
done
```

How does this impact performance?

```
2m37.432s
```

Not much of an increase, but at least it's offset some of the penalty incurred earlier!

### Solution 3

Even without the overhead of nix-shell on each invocation, it still seems wasteful to be firing up `racket` twice per loop iteration. If we rewrite the loop in Racket, we can call the code used by `getNormalForm.rkt` and `getDefinedNames.rkt` without having to set up and tear down the interpreter each time. Plus, Racket is a much saner language for programming than Bash in any case.

Here's a relevant snippet of the port:

```scheme
;; Read stdin into a list of (non-empty) strings
(define given-lines
  (filter (lambda (x) (not (equal? 0 (string-length x))))
          (port->lines (current-input-port))))

;; Equivalent to the loop body
(define (mk-output line so-far name-replacements)
  (let* ([expr      (read-from-string line)] ;; Read s-expressions from each line
         [norm-line (norm     expr)]         ;; The function used by getNormalForm.rkt
         [names     (names-in expr)]         ;; The function used by getDefinedNames.rkt
         [existing  (filter (lambda (x)
                              (equal? norm-line (second x)))
                            so-far)])
    (if (empty? existing)
        (list (cons (list names norm-line) so-far)
              name-replacements)
        (list so-far
              (append (zip names (first (car existing)))
                      name-replacements)))))

;; Equivalent to the loop
(define (remove-redundancies lines so-far name-replacements)
  (if (empty? lines)
      (list so-far name-replacements)
      (let* ([result (mk-output (car lines) so-far name-replacements)]
             [new-sf (first  result)]
             [new-nr (second result)])
        (remove-redundancies (cdr lines)
                             new-sf
                             new-nr))))

(define output
  (remove-redundancies given-lines null null))
```

There's plenty of opportunity to replace the surrounding Bash code as well, but for now I just replaced this one loop, while maintaining the string-based input and output behaviour.

So, has my porting effort paid off?

```
2m10.083s
```

A 17% improvement is quite nice, but doesn't seem large enough to justify porting any more of the bash across considering the effort involved. Since we're still above 2 minutes for processing this single file, and I have around 400 files to process (not to mention all of the repeated calls during tests!), it seemed worth digging a little more to try and find more substantial optimisations.

### Solution 4

In my Racket code I've been doing some pretty braindead things, in the name of cranking out the code (since I still didn't know if these manipulations would work for my data set). Were there any low-hanging optimisation opportunities to be found on the Racket side?

How about the following, which is clearly redundant:

```scheme
(define (max-name pre expr)
  (match expr
    [(cons a b) (if (< (max-name pre a) (max-name pre b))
                    (max-name pre b)
                    (max-name pre a))]
    [x          (name-num pre x)]))
```

This finds the largest name in `expr`, which begins with `pre`. For example, `(max-name "normalise-variable-" '(lambda (normalise-variable-4) (foo normalise-variable-4 normalise-variable-10)))` will return `10`.

The base case, `(name-num pre x)`, just returns `pre` suffixed with `0`. The recursive case for `cons` is clearly performing redundant work, since it's recursing on each part to check which has the bigger number, then recursing on that part *again* to return the number. These expressions are all pretty tiny, so it can't be slowing us down much. Still, I thought I might as well do the obvious improvement and use `let` to store the results, so I can re-use them rather than recalculating:

```scheme
(define (max-name pre expr)
  (match expr
    [(cons a b) (let ([ma (max-name pre a)]
                      [mb (max-name pre b)])
                  (if (< ma mb) mb ma))]
    [x          (name-num pre x)]))
```

How does this affect performance?

```
2m9.580s
```

Hmm, very underwhelming. Is there anything else lurking in the Racket code which we can optimise?

### Solution 5

Looking further through the Racket code, I came across the following:

```scheme
(define (max-var expr)
  (if (is-var? expr)
      expr
      (match expr
        [(cons a b) (if (var-lt? (max-var a) (max-var b))
                        (max-var b)
                        (max-var a))]
        [_          (string->symbol (string-append var-prefix "0"))])))
```

Look familiar? This performs pretty much the same job as `max-name`, but is specific to variable names. This was already working when I decided to add support for normalising datatypes, constructors and destructors, so rather than refactoring I just wrote `max-name` from scratch to be more generic. Variables are still being handled by `max-var`, so we might as well do the same optimisation as we did for `max-name`:

```scheme
(define (max-var expr)
  (if (is-var? expr)
      expr
      (match expr
        [(cons a b) (let ([ma (max-var a)]
                          [mb (max-var b)])
                      (if (var-lt? ma mb) mb ma))]
        [_          (string->symbol (string-append var-prefix "0"))])))
```

Since the `max-name` improvement was rather meagre, we shouldn't expect much from this either; still, it's trivial enough to perform so we might as well see what it does:

```
0m23.532s
```

Wow, that's shaved 82% off the runtime! Not bad for a few minutes work.

There are still plenty more optimisation opportunities in the code, for example lots of boundaries where data is formatted into strings, only to be immediately parsed back somewhere else. For now though, I think 24 seconds is pretty workable, and if these numbers are representative, I've cut down my 400 file workload from 16 hours to 2 hours.

I suppose the lesson is to make those trivial improvements when you spot them; even if they appear miniscule, their aggregate effect may be substantial. Alternatively, the lesson might be to avoid being *too* braindead, even when you expect to be throwing away the code!
