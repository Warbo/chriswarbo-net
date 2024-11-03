#!/usr/bin/env racket
#lang racket
(require math/number-theory)
(require racket/trace)
(module+ test (require rackunit rackcheck-lib))

;;;;;;;;;;;;;;;
(define (lcm a b)
  (/ (* a b) (gcd a b)))

; Function to extract the root order (n) from a term
(define (get-root-order term)
  (match term
    [`(√ ,n ,_) n]
    [_ 1]))  ; Non-root terms are treated as first-order (n=1)

; Function to raise a term to a power
(define (term-power term power)
  (match term
    [`(√ ,n ,base) `(√ ,(* n power) ,base)]
    [x `(expt ,x ,power)]))

; Function to generate all combinations of terms
(define (generate-combinations terms power)
  (if (= power 0)
      '(())
      (for*/list ([t terms]
                  [rest (generate-combinations terms (sub1 power))])
        (cons t rest))))

; Main function to calculate the conjugate
(define (calculate-conjugate expr)
  (match expr
    [`(+ ,@terms)
     (let* ([root-orders (map get-root-order terms)]
            [lcm-order (foldl lcm 1 root-orders)]
            [combinations (generate-combinations terms (sub1 lcm-order))])
       ; Generate the conjugate terms
       (let loop ([combs combinations]
                  [index 0]
                  [result null])
         (if (null? combs)
             `(+ ,@(reverse result))
             (let* ([comb (car combs)]
                    [sign (if (even? index) 1 -1)]
                    [term (if (= (length comb) 1)
                              (term-power (car comb) (sub1 lcm-order))
                              `(* ,@(map (lambda (t) (term-power t 1)) comb)))])
               (loop (cdr combs)
                     (add1 index)
                     (cons (if (= sign 1) term `(* -1 ,term)) result))))))]
    [x x]))  ; If it's not a sum, return as is

; Example usage
(define example-expr '(+ (√ 2 a) (√ 3 b)))
(define conjugate (calculate-conjugate example-expr))

`(× ,example-expr ,conjugate)

;;;;;;;;;;;;;;;

(define/match (factorise n)
  [(1) '()]
  [(0) '(0)]
  [((? negative?)) (cons -1 (factorise (abs n)))]
  [(_) (append-map make-list (prime-exponents n) (prime-divisors n))])

(module+ test
  (define gen:int (gen:integer-in -100 100))

  (check-property
   (property factorise-product-equals-original
             ([n gen:int])
             (check-equal? n (apply * (factorise n))))))

(define/match (<=? x y)
  [((? number?) (? number?)) (<= x y)]
  [((? number?) (? list?)) #t]
  [((? list?) (? number?)) #f]
  [((? list?) (? list?)) (string<=? (format "~s" x) (format "~s" y))])

(module+ test
  (define (gen:list-of node leaf)
    (gen:sized
     (lambda (size)
       (if (zero? size)
           (gen:choice (gen:map leaf list) (gen:const '()))
           (gen:bind
            (gen:integer-in 1 size)
            (lambda (n)
              (gen:let ([h (gen:resize node n)]
                        [t (gen:resize (gen:list-of node leaf)
                                       (max 0 (- size n 1)))])
                       (cons h t))))))))

  (define gen:root (gen:let ([nth (gen:map gen:natural add1)]
                             [n gen:natural])
                            (list '√ nth n)))

  (define gen:atom (gen:choice gen:int gen:root))

  (define gen:sum
    (gen:map (gen:list-of (gen:delay gen:exp) gen:atom) (curry cons '+)))

  (define gen:product
    (gen:map (gen:list-of (gen:delay gen:exp) gen:atom) (curry cons '×)))

  (define gen:ratio
    (gen:sized
     (lambda (size)
       (if (< size 1)
           (gen:let ([num gen:atom]
                     [den gen:atom])
                    (list '/ num den))
           (gen:bind
            (gen:integer-in 1 size)
            (lambda (n)
              (gen:let ([num (gen:resize gen:exp (max 0 (- size n 1)))]
                        [den (gen:resize gen:exp n)])
                       (list '/ num den))))))))

  (define gen:exp (gen:choice gen:atom gen:sum gen:product gen:ratio))

  (define gen:expr (gen:resize gen:exp 100))

  (check-property
   (property <=?-is-reflexive
             ([x gen:expr])
             (check-pred (lambda (y) (<=? y y)) x)))

  (check-property
   (property <=?-is-transitive
             ([x gen:expr]
              [y gen:expr]
              [z gen:expr])
             (check-pred (lambda (triple)
                           (implies (and (first triple) (second triple))
                                    (third triple)))
                         (list (<=? x y) (<=? y z) (<=? x z)))))

  (check-property
   (property <=?-is-total
             ([x gen:expr]
              [y gen:expr])
             (check-true (or (<=? x y) (<=? y x)))))

  (check-property
   (property <=?-consistent-with-equality
             ([x gen:expr]
              [y gen:expr])
             (check-equal? (and (<=? x y) (<=? y x))
                           (equal? x y))))

  (check-property
   (property <=?-negation-invariance
             ([x gen:expr]
              [y gen:expr])
             (check-equal? (<=? x y)
                           (not (<=? y x)))))
  )

(define/match (step-factor a)
  [(1) '()]
  [((cons '× as)) as]
  [((? integer?)) (factorise a)]
  [(_) (list a)])

;; Given a pair of adjacent factors, return a list of factors that's closer to
;; being in normal form. In particular, returning (list a b) indicates they're
;; in normal form; returning anything else indicates they're not, and the result
;; must be normalised (pairwise) again.
(define (step-factors a b)
  (cond
    [(equal? (list a b) '(-1 -1)) '()]
    [(<=? b a) (list b a)]
    [else (append (step-factor a) (step-factor b))]))

(define (normalise-product prod)
  (let loop ([result '()] [rest prod])
    (if (null? rest)
        ;; No more unprocessed factors remain, we're finished
        (reverse result)
        (if (null? result)
            ;; Process the first factor on its own
            (let* ([next (car rest)]
                   [normalised (step-factor next)])
              (if (equal? normalised (list next))
                  ;; Once it's in normal form, we can move on
                  (loop normalised (cdr rest))
                  ;; If not, process all of its results again
                  (loop '() (append normalised (cdr rest)))))

            ;; Otherwise, process the next factor along with the previous
            (let* ([prev (car result)]
                   [next (car rest)]
                   [normalised (step-factors prev next)])
              (if (equal? normalised (list prev next))
                  ;; Factors were in correct order, append and move on
                  (loop (cons next result) (cdr rest))

                  ;; Otherwise, append normalised to the front of rest and try
                  ;; again. We have to pop result, to avoid duplicating prev, so
                  ;; this acts like insertion sort: allowing small factors to
                  ;; propagate backwards until they're in the correct place.
                  ;; Also note that the elements of normalised might not be next
                  ;; and prev since it implements squaring, anti-commuting, etc.
                  (loop (cdr result) (append normalised (cdr rest)))))))))
(trace normalise-product)

(module+ test
  (check-property
   (property normalise-product-preserves-negative-parity
             ([n gen:natural]
              [factors (gen:list (gen:integer-in 2 1000))])
             (let ([with-negatives (append (make-list n -1) factors)]
                   [want (sort
                          (append-map
                           factorise
                           (append factors (if (even? n) '() '(-1))))
                          <)])
               (check-equal?
                (normalise-product (shuffle with-negatives))
                want))))

  (check-property
   (property normalise-product-removes-ones
             ([xs (gen:list gen:expr)])
             (check-pred (lambda (ys) (null? (filter (curry equal? 1) ys)))
                         (normalise-product xs))))

  (check-property
   (property normalise-product-maintains-integer-product
             ([xs (gen:filter (gen:list gen:expr) integer?)])
             (let* ([int-prod (lambda (ys) (apply * ys))]
                    [want (int-prod xs)])
               (check-pred (lambda (ys) (equal? want (int-prod ys)))
                           (normalise-product xs))))
   )
  )


(define (normalise-summands sum)
  (if (null? sum)
      '(+)
      (sort sum string<?)))

(module+ test
  (check-property
   (property normalise-summands-is-sorted
     ([elements (gen:list (gen:integer-in -100 100))])
     (check-equal?
      (normalise-summands (map (lambda (x) `(× ,x)) elements))
      (map (lambda (x) `(× ,x)) (sort elements <))
      "??"))))

;; Function to normalise roots
(define (normalise-root n m)
  (cond
    [(= n 1) (list m)]
    [(= m 0) '(0)]
    [(= m 1) '(1)]
    [else
     (let* ([factors (factorise m)]
            [grouped-factors (group-by identity factors)]
            [normalised-factors
             (apply append
                    (for/list ([(factor count) (in-hash grouped-factors)])
                      (let-values ([(quotient remainder) (quotient/remainder count n)])
                        (append
                         (make-list quotient factor)
                         (if (zero? remainder)
                             '()
                             (list (list '√ n factor)))))))])
       (if (null? normalised-factors)
           '(1)
           normalised-factors))]))

(module+ test
  (check-property
   (property normalise-root-simplifies-perfect-powers
     ([base (gen:integer-in 2 10)]
      [exp (gen:integer-in 2 5)])
     (equal? (normalise-root exp (expt base exp))
             (list base)))))

;; Function to normalise the entire expression
(define (normalise expr)
  (define (ensure-fraction e)
    (match e
      [(list '/ num den) (list '/ (ensure-sum num) (ensure-sum den))]
      [_ (list '/ (ensure-sum e) '(+ (×)))]))

  (define (ensure-sum e)
    (match e
      [(list '+ terms ...) (list '+ (map ensure-product terms))]
      [_ (list '+ (ensure-product e))]))

  (define (ensure-product e)
    (match e
      [(list '× factors ...) (list '× factors)]
      [(? number?) (if (= e 1) '(×) (list '× e))]
      [(? symbol?) (list '× e)]
      [(list '√ n m) (list '× (list '√ n m))]
      [_ e]))

  (define (rationalize-denominator num den)
    (let* ([gcd (apply gcd (append num den))]
           [new-num (map (lambda (x) (quotient x gcd)) num)]
           [new-den (map (lambda (x) (quotient x gcd)) den)])
      (values new-num new-den)))

  (let* ([frac (ensure-fraction expr)]
         [num (cadr frac)]
         [den (caddr frac)]
         [norm-num (normalise-summands (map normalise-product (cdr num)))]
         [norm-den (normalise-summands (map normalise-product (cdr den)))]
         [common-factors (set-intersect norm-num norm-den)]
         [reduced-num (set-subtract norm-num common-factors)]
         [reduced-den (set-subtract norm-den common-factors)]
         [flattened-num (apply append (map (lambda (x) (if (equal? (car x) '×) (cdr x) (list x))) reduced-num))]
         [flattened-den (apply append (map (lambda (x) (if (equal? (car x) '×) (cdr x) (list x))) reduced-den))]
         [num-factors (filter number? flattened-num)]
         [den-factors (filter number? flattened-den)])
    (let-values ([(rationalized-num rationalized-den) (rationalize-denominator num-factors den-factors)])
      (let ([final-num (if (null? rationalized-num) '(+) (list '+ (cons '× (append rationalized-num (filter-not number? flattened-num)))))]
            [final-den (if (null? rationalized-den) '(+ (×)) (list '+ (cons '× (append rationalized-den (filter-not number? flattened-den)))))])
        (list '/ final-num final-den)))))

(module+ test
  (define (gen:list-range g lo hi)
    (gen:let ([init (apply gen:tuple (make-list lo g))]
              [rest (gen:list g #:max-length (- hi lo))])
             (append init rest)))

  (check-property
   (property normalise-simplifies-fraction
             ([n1 (gen:integer-in -100 100)]
              [n2 (gen:integer-in -100 100)]
              [d1 (gen:integer-in 1 100)]
              [d2 (gen:integer-in 1 100)])
             (let* ([expr `(/ (+ (× ,n1 ,d2) (× ,n2 ,d1)) (+ (× ,d1 ,d2)))]
                    [normalised (normalise expr)]
                    [expected (normalise (from-racket (+ (/ n1 d1) (/ n2 d2))))])
               (equal? normalised expected))))

  (check-property
   (property normalise-handles-non-fraction-input
     ([n (gen:integer-in -100 100)])
     (equal? (normalise n)
             (normalise (list '/ (list '+ (list '× n)) '(+ (×)))))))

  (check-property
   (property normalise-handles-raw-product
             ([factors (gen:list-range (gen:integer-in -10 10) 1 5)])
     (equal? (normalise (cons '× factors))
             (normalise (list '/ (list '+ (cons '× factors)) '(+ (×)))))))

  (check-property
   (property normalise-handles-raw-sum
     ([terms (gen:list-range (gen:integer-in -10 10) 1 5)])
     (equal? (normalise (cons '+ terms))
             (normalise (list '/ (cons '+ (map (lambda (t) (list '× t)) terms)) '(+ (×)))))))

  (check-property
   (property normalise-idempotent
             ([expr (gen:choice
                     (gen:integer-in -100 100)
                     (gen:ratio
                      (gen:sum (gen:product (gen:integer-in -10 10)))
                      (gen:sum (gen:product (gen:integer-in -10 10)))))])
             (equal? (normalise expr) (normalise (normalise expr))))))

;; Function to convert from Racket numbers to our format
(define (from-racket n)
  (cond
    [(zero? n) '(/ (+) (+ (×)))]
    [(= n 1) '(/ (+ (×)) (+ (×)))]
    [(= n -1) '(/ (+ (× -1)) (+ (×)))]
    [(rational? n)
     (if (integer? n)
         (let ([factors (factorise (abs n))])
           (normalise `(× ,@(if (negative? n) (cons -1 factors) factors))))
         (normalise `(/ (× ,(numerator n)) (× ,(denominator n)))))]
    [else (error "Unsupported number type")]))

;; Function to convert from our format to Racket numbers
(define (to-racket expr)
  (define (evaluate-product factors)
    (for/product ([factor factors])
      (if (list? factor)
          (error "Cannot convert expressions with roots to Racket numbers")
          factor)))

  (match expr
    [(list '/ (list '+ terms ...) (list '+ (list '×)))
     (apply + (map (lambda (term)
                     (match term
                       [(list '× factors ...) (evaluate-product factors)]
                       [_ term]))
                   terms))]
    [(list '/ num den)
     (/ (to-racket (list '/ num '(+ (×))))
        (to-racket (list '/ den '(+ (×)))))]
    [_ (error "Invalid expression")]))

(module+ test
  (define (gen:fraction num-lo num-hi den-lo den-hi)
    (gen:let ([num (gen:integer-in num-lo num-hi)]
              [den (gen:integer-in den-lo den-hi)])
             (/ num den)))

  (check-property
   (property roundtrip-without-roots
             ([n (gen:choice (gen:integer-in -1000 1000)
                             (gen:fraction -1000 1000 1 1000))])
             (= n (to-racket (from-racket n))))))

;; Arithmetic operations
(define (add a b)
  (normalise
   (list '/
         (list '+
               (list '× (cadr a) (caddr b))
               (list '× (cadr b) (caddr a)))
         (list '× (caddr a) (caddr b)))))

(module+ test
  (check-property
   (property addition-commutative
     ([a (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))]
      [b (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))])
     (equal? (normalise (add a b)) (normalise (add b a)))))

  (check-property
   (property addition-associative
     ([a (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))]
      [b (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))]
      [c (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))])
     (equal? (normalise (add (add a b) c))
             (normalise (add a (add b c)))))))

(define (multiply a b)
  (normalise
   (list '× (cadr a) (cadr b))))

(module+ test
  (check-property
   (property multiplication-commutative
     ([a (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))]
      [b (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))])
     (equal? (normalise (multiply a b)) (normalise (multiply b a)))))

  (check-property
   (property multiplication-associative
     ([a (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))]
      [b (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))]
      [c (gen:let ([n (gen:integer-in -100 100)]
                   [d (gen:integer-in 1 100)])
          (from-racket (/ n d)))])
     (equal? (normalise (multiply (multiply a b) c))
             (normalise (multiply a (multiply b c)))))))

(define (negate a)
  (normalise
   (list '/
         (list '× -1 (cadr a))
         (caddr a))))
(module+ test
  (check-property
   (property negation-involution
             ([a (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))])
             (equal? a (normalise (negate (negate a)))))))

(define (invert a)
  (normalise
   (list '/ (caddr a) (cadr a))))

(module+ test
  (define gen:non-zero-expr
    (let ([zero (from-racket 0)])
      (lambda (gen:expr) (gen:filter gen:expr (lambda (x) (not (equal? x )))))))

  (check-property
   (property inversion-involution
             ([a (gen:non-zero-expr (gen:let ([n (gen:integer-in -100 100)]
                                       [d (gen:integer-in 1 100)])
                                      (from-racket (/ n d))))])
             (equal? a (normalise (invert (invert a)))))))

(define (subtract a b)
  (add a (negate b)))

(module+ test
  (check-property
   (property subtraction-negation
             ([a (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))]
              [b (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))])
             (equal? (normalise (subtract a b)) (normalise (add a (negate b)))))))

(define (divide a b)
  (multiply a (invert b)))

(module+ test
  (check-property
   (property division-multiplication
             ([a (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))]
              [b (gen:non-zero-expr (gen:let ([n (gen:integer-in -100 100)]
                                              [d (gen:integer-in 1 100)])
                                             (from-racket (/ n d))))])
             (equal? (normalise (divide a b)) (normalise (multiply a (invert b))))))

  ;; Additional properties to test arithmetic operations against Racket
  (check-property
   (property addition-matches-racket
             ([a (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))]
              [b (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))])
             (= (to-racket (add a b)) (+ (to-racket a) (to-racket b)))))

  (check-property
   (property multiplication-matches-racket
             ([a (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))]
              [b (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))])
             (= (to-racket (multiply a b)) (* (to-racket a) (to-racket b)))))

  (check-property
   (property subtraction-matches-racket
             ([a (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))]
              [b (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))])
             (= (to-racket (subtract a b)) (- (to-racket a) (to-racket b)))))

  (check-property
   (property division-matches-racket
             ([a (gen:let ([n (gen:integer-in -100 100)]
                           [d (gen:integer-in 1 100)])
                          (from-racket (/ n d)))]
              [b (gen:non-zero-expr (gen:let ([n (gen:integer-in -100 100)]
                                              [d (gen:integer-in 1 100)])
                                             (from-racket (/ n d))))])
             (= (to-racket (divide a b)) (/ (to-racket a) (to-racket b))))))
