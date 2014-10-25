---
title: "Strong typing" in PHP
---
I was sent a link today to a description of ["strong" typing in PHP] [1]. The article describes "autoboxing", which is a technique for wrapping non-object data in objects. The technique is to carry on using the non-object data types, but encapsulate them inside some kind of object/class. This ends up being pretty ugly, with all kinds of static horribleness. However, there's a much simpler way to enforce types simply by using classes. Behold.

[1]: http://php.webtutor.pl/en/2011/04/13/strong-data-typing-in-php-part-ii-autoboxing-and-indestructable-objects-english-version/

**Numbers**
Numbers are pretty useful things to have. How can we strongly type our numbers in PHP? The most straightforward numbers are the Naturals (0, 1, 2, ...), which we can enforce with the following interface:

```php
interface Number {
}

interface Natural extends Number {
  public function plus(Number $n);
  public function times(Number $n);
}
```

Natural numbers are closed over addition and multiplication, ie. adding 2 Naturals will always give a Natural, and multiplying 2 Naturals will always give a Natural. Thus we define plus and times for our Natural interface. In a rich type system like Haskell's, we might say `(+) :: Natural -> Natural -> Natural`, `(*) :: Natural -> Natural -> Natural`. Alas, I haven't figured out a way to give functions a type in PHP yet :(

So, what would our numbers look like? They would look like this:

```php
class Zero implements Natural {
  public function plus(Number $n) { return $n; }
  public function times(Number $n) { return $this; }
}

class Successor implements Natural {
  private $of;
  function __construct(Number $of) { $this->of = $of; }
  public function plus(Number $n) { return new Successor($this->of->plus($n)); }
  public function times(Number $n) { return $n->plus($n->times($this->of)); }
}
```

This is just good, old-fashioned [Peano Arithmetic] [2]. We can define any Natural number as either being Zero, or being the "Successor" of (ie. one-more-than) a Natural number. With these in hand we can write code like the following:

[2]: http://en.wikipedia.org/wiki/Peano_axioms

```php
$zero  = new Zero();
$one   = new Successor($zero);
$two   = new Successor($one);
$three = new Successor(new Successor(new Successor(new Zero())));
$one_hundred_and_thirty = $two->times($two)
                              ->times($two)
                              ->times($two)
                              ->times($two)
                              ->times($two)
                              ->times($two)
                              ->plus($two);
```

We're performing arithmetic without using any of PHP's numbers, just objects and classes. We're strongly typed, there are no statics to be found, there are no public properties or other hacky, non-OO kludges used. The only weirdness is the use of a `__construct` function and the `new` keyword, but PHP pretty much forces these on us as programmers anyway.

**Division**

We should probably support division, but we'd have to leave the Naturals behind. The numbers closed under addition, multiplication and division are the Rationals (except for division by zero, which we throw as a runtime error):

```php
interface Rational extends Natural {
    public function divide(Number);
}

class Fraction implements Rational {
    private $numerator;
    private $denominator;
    function __construct(Number $n, Number $d) {
        $this->numerator = $n;
        $this->denominator = $d;
        $this->simplify();
    }
    public function plus(Number $n) {
        return new Fraction($this->numerator->plus($n->times($this->denominator)), $this->denominator);
    }
    public function times(Number $n) {
        return new Fraction($this->numerator->times($n), $this->denominator);
    }
    public function divide(Number $n) {
        if ($n instanceof Zero) throw new Exception('Division by zero');
        return new Fraction($this, $n);
    }

    private function simplify() {
        while ($this->numerator instanceof Fraction) {
            $this->simplify_numerator($this->numerator);
        }
        while ($this->denominator instanceof Fraction) {
            $this->simplify_denominator($this->denominator);
        }
    }
    private function simplify_numerator(Fraction $num) {
        $this->denominator = $this->denominator->times($num->denominator);
        $this->numerator = $num->numerator;
    }
    private function simplify_denominator(Fraction $den) {
        $this->numerator = $this->numerator->times($den->denominator);
        $this->denominator = $den->numerator;
    }
}
```

**Negatives**

Numbers are tricky things, but so far we've got most primary school arithmetic implemented. We're just missing subtraction, and for this we need signed numbers:

```php
interface Signed extends Rational {
    public function subtract(Number $n);
}

class Difference implements Signed {
    private $add;
    private $sub;
    function __construct(Number $a, Number $s) {
        $this->add = $a;
        $this->sub = $s;
        $this->simplify();
    }
    public function plus(Number $n) {
        return new Difference($this->add->plus($n), $this->sub);
    }
    public function times(Number $n) {
        return new Difference($this->add->times($n), $this->sub->times($n));
    }
    public function divide(Number $n) {
        return new Difference($this->add->divide($n), $this->sub->divide($n));
    }
    public function subtract(Number $n) {
        return new Difference($this->add, $this->sub->plus($n));
    }

    private function simplify() {
        while ($this->add instanceof Difference) {
            $this->simplify_add();
        }
        while ($this->sub instanceof Difference) {
            $this->simplify_sub();
        }
    }
    private function simplify_add(Difference $a) {
        $this->sub = $this->sub->plus($a->sub);
        $this->add = $a->add;
    }
    public function simply_sub(Difference $s) {
        $this->add = $this->add->plus($s->sub);
        $this->sub = $s->add;
    }
}
```

Now we can back-port the later classes to the earlier ones, for example implementing `A->divide(B)` as `Rational(A, B)`, `A->subtract(B)` as `Difference(A, B)`, etc. This gives us pretty much all of PHP's number support (apart from helper functions, which are trivial), but why stop there? Python has built-in support for Complex numbers, why doesn't PHP?

```php
interface Complex {
    public function conjugate();
}

class ComplexPair implements Complex {
    private $real;
    private $imaginary;
    function __construct(Number $r, Number $i) {
        $this->real = $r;
        $this->imaginary = $i;
    }
    public function conjugate() {
        return new ComplexPair($this->real, new Difference(new Zero(), $this->imaginary));
    }
    public function plus(Number $n) {
        return new ComplexPair(
            $this->real->plus(
                $n->plus($n->conjugate())->divide(
                    new Succ(new Succ(new Zero()))
                )
            ),
            $this->imaginary->plus(
                $n->subtract($n->conjugate())->divide(
                    new ComplexPair(new Zero(), new Succ(new Succ(new Zero())))
                )
            )
        );
    }
    public function subtract(Number $n) {
        return $this->plus(new Difference(new Zero(), $n));
    }
    public function times(Number $n) {
        $n_real = $n->plus($n->conjugate())->divide(new Succ(new Succ(new Zero())))
        $n_imag = $n->subtract($n->conjugate())->divide(new ComplexPair(new Zero(), new Succ(new Succ(new Zero()))));
        return new ComplexPair(
            $this->real->times($n_real)->subtract(
                $this->imaginary->times($n_imag)
            ),
            $this->imaginary->times($n_real)->plus($this->real->times($n_imag))
        );
    }
    public function divide(Number $n) {
        return $this->times(new Rational(new Succ(new Zero()), $n));
    }
}
```

Of course, we can go on to Quarternions and the rest of the Clifford Algebras, or equivalently make vectors, matrices, tensors, etc.

I'll leave it as an exercise for the reader to work out some more rigourous simplification functions, as well as analogous implementations of booleans (pretty simple), characters (which we can map from numbers), lists, strings, trees, etc.
