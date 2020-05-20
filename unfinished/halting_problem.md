The halting problem sounds like some obscure technicality that will never come
up in practice, but it actually stops us calculating any non-trivial property of
a program (this is known as Rice's Theorem). For example, we might ask "does
this program add two numbers together?". This a non-trivial question, since some
programs do and some programs don't. It seems like this would be easy to figure
out, but it's impossible in general, since there are infinitely many programs of
the form "run P; read two numbers; output their sum". If P halts, then the
program will add together the two numbers; if P doesn't halt, the program will
not add together two numbers (it will never reach that part of the code). Since
the halting problem is undecidable, we can't always tell whether P will halt or
not, so the "adds two numbers together" problem must also be undecidable. We can
substitute "adds two numbers together" for any non-trivial property (e.g. "does
not leak passwords", etc.). There are only two types of property we can figure
out, in general: one type is trivial properties, which either apply to all
programs or no programs (these are more like properties of the *language*,
rather than the programs; e.g. all Python code is memory-safe, so any given
Python program will be memory-safe). The other type is about the implementation
rather than the input/output behaviour (AKA the function being computed);
e.g. "does this program take more than 10 steps?" is decidable, since we can
just run the program and stop if it takes more than 10 steps.

--

The liar's paradox ("this statement is false") is caused by the self-reference
of the word "this", and was easily dismissed by banning self-reference
(e.g. Russell did this formally using type theory). Yet that's far too
simplistic: it was only recently (less than a century ago) when Goedel showed
that any system powerful enough to express (Peano) arithmetic is necessarily
powerful enough to "implement itself" (he used Goedel numbering; these days we'd
probably use bytestrings of ASCII or UTF-8). Such "meta-circular interpreters"
give rise to the liar's paradox in a way which is unavoidable (without
sacrificing the ability to express arithmetic!), i.e. we can always construct a
value (number, bytestring, whatever) X which encodes the statement "eval(Y)
cannot be proved", where Y is some expression which evaluates to X.

Goedel's work still had a "loophole": we can always devise a more powerful
logical system by including such statements as extra axioms. Turing's work
(which was contemporary with Goedel) showed that logical systems exceeding a
certain expressive power become undecidable (we now call that level "Turing
completeness"), and hence Goedel's loophole stops working.

Note that Schoenfinkel stumbled into Turing completeness in the 1920s (in the
form of combinatory logic), but didn't have the context of Goedel's
incompleteness theorems to make the connection to the decision problem. Church
also stumbled into it, but Goedel considered it to be a deficiency in his system
of logic (lambda calculus) and tried to make a more powerful system (general
recursive functions, which turned out to be equivalent). It was Turing's
philosophical arguments (in "On Computable Numbers") which finally convinced
Goedel that computability was a fundamental property of logic.

--

Turing didn't just invent a formal system, then show it has undecidable
problems. He came up with his machines as a philosphical argument that all
"effective methods" of computation that a human could possibly calculate can be
reduced to a single formal model (a universal turing machine).

Turing's argument is that we can't have an infinite number of "mental states",
since our thinking takes place in a finite space (e.g. a skull, a house, the
Virgo Supercluster, whatever), which would require infinite states to be so
infinitesimally-close to each other that they'd be physically
indistinguishable. He used the same argument to claim that the information we
input and output must also be finite. Since these sets are finite, we can (in
principle) number them and write down a huge transition table mapping what every
pair of state/symbol indices evolves into. Hence any form of thinking (including
any "effective method" of calculation) can be encoded by such a (huge)
table. These transition tables are explicit enough to be followed mechanically
by a machine, i.e. a turing machine.

Turing then showed that there are universal machines which can simulate the
behaviour of any turing machine (by encoding that machine's transition table and
initial tape on to the universal machine's initial tape). Since a universal
machine can simulate any other, it must be able to simulate our hypothetical
transition tables of all possible human thought; hence a universal machine must
be capable of performing all "effective methods" of computation. Turing gave an
example of a universal machine, showing that they can be remarkably small and
simple.

It was this philosophical argument about the nature of "effective methods",
human thought, physical processes, etc. that made Turing's work
important. Turing didn't just show that the halting problem is undecidable by
turing machines; since he argued that turing machines are the most powerful
method of computation that could ever exist, he was also arguing that the
halting problem is undecidable by any method of computation that could ever
exist.

Keep in mind that Goedel didn't like lambda calculus and thought a more powerful
logic must be possible, and he invented his system of general recursive
functions in an attempt to demonstrate it. When Church proved that general
recursive functions are equivalent in power to lambda calculus, Goedel stuck to
his guns and said that general recursive functions must therefore also be
defective! It was Turing's philosophical argument (and proof that lambda
calculus is equivalent to turing machines) that finally convinced Goedel to
accept that this is indeed the limit of computability/decidability.

--

Yep: classical logic assumes every statement is true or false; Goedel showed
that there is a third category: undecidable (by that system of logic). (BTW that
lead to constructive logic and intuitionistic logic)

Turing argued (in a way that isn't discussed in this video, but is in his "On
Computable Numbers" paper) that anything that a human mathematician/computer
could possibly do, could in principle be simulated by a (hugely complicated)
turing machine. He then showed that there is a rather simple "universal turing
machine", which can simulate any turing machine (including our hypothetical
human-simulator machine). We now call such universal machines "computers", and
the halting problem is undecidable by that system of logic (turing
machines). But, since "that system of logic" (turing machines) can simulate
anything that a mathematician can do (according to Turing), the halting problem
must be undecidable by any system of logic.

--

Nope, quantum computers can solve exactly the same problems as classical
computers (e.g. turing machines). Some experts think that quantum computers may
arrive at the answer faster for certain problems (e.g. Shor's algorithm for
finding prime factors), but AFAIK that's still speculation (we've proved that
quantum computers are fast, but we haven't ruled out the existence of classical
algorithms which are just as fast)

--

Quantum computers can solve exactly the same problems as classical computers
(like turing machines). They might be able to solve some sorts of problems more
quickly, that's all.

--

If a problem P is undecidable in some system of logic L, we can invent two new
systems of logic: L + P as an axiom, and L + not(P) as an axiom. Turing went a
bit further than Goedel, and argued that all 'effective methods of computation'
can be carried out by a universal turing machine. Since the halting problem (for
turing machines) can't be solved by a turing machine, it cannot be solved by any
effective method of computation. In other words, if we extend turing machines
with a halting-problem-decider (known as an "oracle"), there is no way to
perform the resulting calculations.

...

You're right, my explanation was confused. The halting problem for turing
machines is undecidable by turing machines; we can invent a new type of machine
(oracle machines) which can decide the halting problem for turing machines, but
(a) they are not computable by any effective method and (b) they cannot solve
their own halting problem, so we need to add another type of machine, and so on.

As for whether halting really matters, it absolutely does! Consider the program
'Run P, then format my hard drive'. Will this program format my hard drive? It
depends whether P halts or not! We could try your solution of running it for a
while to see what happens, but it might end badly! We can use examples of the
form 'Run P, then do X' to show that all non-trivial questions about program
behaviour are undecidable. This is Rice's Theorem. "Trivial" questions hold for
all programs or no programs, i.e. they're properties of the language:
e.g. Javascript provides no mechanism for formatting my hard drive, so it's
trivially "no" for any Javascript program.

The presence of non-halting functions also makes type systems unsound, which is
why languages like Agda and Coq forbid them (and are hence not
Turing-complete). For example take the function 'f(int x) { return f(x); }'; we
know it's a function whose argument is an int, but what is its return type? We
can give it any return type we like, e.g. if we claim that 'f' returns a string,
when we check the 'return' statement we'll find that it does indeed return a
string! We can use this to violate any guarantees that a programmer tries to
make at compile-time. For example, if an array-lookup function requires evidence
that the requested index is in bounds, we can use 'f(0)' as evidence that any
index is in bounds for any array, and hence gain read access to arbitrary
memory. Whilst 'f(0)' is an infinite loop, in this example it's also not needed
by the lookup (it only exists as a compile-time safety check, which we've
tricked), so an optimising compiler won't include it in the resulting
executable. Also note that forbidding such recursive definitions doesn't work,
since we can implement recursion ourselves e.g. with the Y combinator!

--

It's "reductio ad absurdum": proving that something doesn't exist, by showing
that its existence would cause a paradox/contradiction. In this case we assume
that the halting problem can be solved, but that lets us construct a
contradiction: the result of "OPPOSITE(OPPOSITE)" is both yes and no. Hence our
assumptions must be flawed, which means that the halting problem must not be
solvable.

--

The impossible part is the HALTS program. We assumed it exists, but that lead to
a contradiction (a program which both halts and doesn't halt). Hence our
assumption was wrong: HALTS doesn't exist. Hence the halting problem is
undecidable; hence there are questions which have definite answers, but which
computers are unable to answer.

--

Turing machines were invented as a model for what humans can do, so the answer
is no (unless we have completely misunderstood physics) See
https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis

--

The Church-Turing thesis says no. There are various ways to phrase it, but the
"strong" form essentially says that the laws of nature are computable by a
turing machine, hence everything in the universe (including any method of
calculation that we come up with) must also be computable by a turing
machine. This is a law of physics: it's mathematically consistent, makes sense
and agrees with every observation and experiment we've ever devised; yet (like
all laws of physics) it can't be mathematically proven to be true (since "the
truth" isn't a formally-defined mathematical object we can write proofs about)

--

To add another wrinkle: no finite number of experiments can determine that a
physical process solves the halting problem. We can apply that process to as
many programs as we like where we already know the answer, and see if it matches
our answer. If it gets one wrong, we know it doesn't solve the halting
problem. If it always gets them right, we don't know if it's a general solution,
or if it only works for a sub-set of programs. The latter might seem contrived,
but keep in mind that all of the programs we try must already come from a
solvable sub-set (i.e. the sub-set of programs that we've been able to solve, in
order to compare!)

--

Goedel's incompleteness theorem requires a particular choice of logical system,
i.e. for any system L we can find a true statement S which cannot be proven in
L. Yet we can always pick a different system where S can be proved, e.g. the
system "L, plus S as an axiom".

Turing showed that there is a limit to how far we can take this: if something
isn't computable on a Turing Machine (e.g. the halting problem) then there is no
way to compute the answer. We can define a logically consistent system which
includes that answer (known as "oracle machines"), but there's no way to compute
proofs in such a system.

--

Quantum computers might solve some problems faster than classical computers
(AFAIK that's still an open question; e.g. we haven't ruled out finding a
classical equivalent to Shor's algorithm, etc.); in any case, they cannot solve
more problems than a classical computer. Turing machines can perfectly (if
slowly) simulate quantum computers, after all.

--

Quantum computers aren't analogue, and they don't have infinite states. A
quantum computer with N qubits is in a superposition of 2^N discrete states. The
amplitudes of each component of the superposition are also discrete: this is
because they result from a discrete number of operators applied to the initial
zero/one states (e.g. a discrete number of trips around a quantum circuit
containing a discrete number of gates). This is analogous to voltage in
classical computers: we can think of it as a continuous variable, but only a
discrete set of values can be engineered to be of any use (anything else is
noise which reduces our ability to compute!). A true analogue computer would be
more powerful than a turing machine, but is fraught with impossibilities
(e.g. requiring infinitesimal noise, measurements, etc.)

--

You're putting the cart before the horse. The existence of non-halting programs
(technically, those which neither terminate nor co-terminate) make a type system
unsound (i.e. we can use infinite loops to "fake" values of any type). If we're
going to bother having a super-duper, ultra-strong type system, we probably
don't want it to be tricked in such a simple way. Hence languages which provide
such strong type systems (e.g. Agda and Coq) *also* restrict programs to avoid
such loops. The mechanism they use to forbid infinite, unproductive loops is
actually separate to type checking.

It's actually quite easy to make a language/system whose halting problem is
solvable, but such a language/system will be unable to solve some problems that
Turing machines can solve. As a simple example, (universal) Turing machines can
simulate any Turing machine, but our halting-solvable system can't (if it could,
we could use that system's halting-problem solution to solve the turing machine
halting problem, but we've just proven that's impossible).

--

The existence of partial solutions to the halting problem is trivial,
e.g. here's one: If the given code is literally just 'HALT' then output 'yes';
otherwise output 'don't know'

Figuring out useful partial solutions is known as termination analysis
https://en.wikipedia.org/wiki/Termination_analysis

--

For any system of logic (with recursively-enumerable axioms, which is all of
them), we can write a program which enumerates all proofs: start by listing the
axioms, then systematically apply every applicable rule of the logic to every
proof we've generated so far.

Given a proof, it's easy to find out which statement it proves (this will be the
last step of the proof).

Hence given any statement in any system of logic, we can write a program which
goes through every possible proof of that system, checks whether it's a proof of
the given statement, and halt if it finds one.

If we could solve the halting problem, we could prove or disprove all
mathematical statements. Given a statement S, we write three programs: A
searches for a proof of S; B searches for a proof of not(S); C searches for
either a proof of S or of not(S). We check if C halts: if not then S is
independent of our chosen logic (i.e. we can choose whether to make it true or
false). If C halts then we check whether A or B halts to find out if it's
provable or non-provable.

--

Whether a computer program loops or not is part of "everything"; since we cannot
know that we therefore cannot know everything. It's also much more profound, in
a couple of ways:
 - We can write programs like "Run P then do X". Since we can't (in general)
   decide whether or not P will halt, we also can't decide whether or not such
   programs will "do X" (if P doesn't halt, the "do X" instruction will never
   run). Since we can replace "do X" with anything we like, e.g. "delete all
   documents", it is undecidable (in general) whether a program will do anything
   (like deleting all of our documents). This is known as Rice's Theorem, and is
   the reason why malware like viruses are unavoidable.
 - We can construct computers using electronics, bouncing balls, quantum
   systems, etc. so the inability to solve the halting problem (or indeed any
   non-trivial question about a program's behaviour) also makes questions about
   these systems undecidable. For example "will this wire switch on?" is
   undecidable, since we can construct a circuit which implements the program
   "Run P then turn on this wire"; if we could tell whether any wire would turn
   on, we could solve the halting problem. Likewise for questions like "will a
   planet reach this location?" (since we can build a computer out of orbiting
   planets), etc.

--

See Rice's Theorem https://en.wikipedia.org/wiki/Rice%27s_theorem Any
(non-trivial) question about the (partial functions implemented by) computer
programs is undecidable. For example questions like "does it leak passwords?" or
"will it delete my files?" are undecidable in general. Checking whether two
programs are equivalent is also undecidable. Note that "program" is also a very
general concept: we can build computers using electronic circuits, so some
questions about circuits must be undecidable (e.g. "will this wire ever get
switched on?"; if there were a general way to answer that question, we could use
it to solve the halting problem). We can also build computers out of billiard
balls ( https://en.wikipedia.org/wiki/Billiard-ball_computer ), which means that
certain questions about Newtonian mechanics must be undecidable (e.g. "will an
object ever reach this position?"; if we were able to answer this question in
general, we could answer it for the output location of a billard ball computer
and hence solve the halting problem). Quantum mechanics lets us build quantum
computers, so it must also have undecidable questions; and so on. It's
remarkably easy to stumble across turing completeness,
e.g. https://beza1e1.tuxen.de/articles/accidentally_turing_complete.html
https://en.wikipedia.org/wiki/Computational_irreducibility

--

We only know that this is paradoxical because we constructed it to be so. If we
were given some arbitrary program, we could not tell if it was paradoxical. As
an analogy: if I construct a statement in a way that makes it true, e.g. "(A AND
B) implies A", then I know that statement is true. This does not mean that I can
recognise whether any statement is true (e.g. "P = NP" or the Riemann
Hypothesis, or the Collatz Conjecture, etc.)


Since every program has infinitely many equivalents, there are infinitely many
ways to construct such a paradoxical situation. All we have done is prove that
they exist, by demonstrating one of them. We cannot avoid the paradox by simply
skipping over this particular example.

--

There are all sorts of places where a halt-checking program would have to be
perfect. There's the fantasy-land ideas like proving any mathematical statement
by checking whether a proof-search halts; but there are also everyday tasks
where it would be really useful. The most obvious one is security: Web sites
don't allow users to upload arbitrary code on to their servers, since the
halting problem (via Rice's Theorem) means we can't decide whether it's
dangerous. If we used an imperfect checker, hackers could fool it and exploit
our systems. The same is true for app/game plugins, software updates, and any
software we ever install on our PCs/laptops/phones. This is why antivirus
programs are so bad; they often only check executables against a database of
known viruses, and don't bother trying to actually analyse the code. Since no
perfect checker can exist, we just have to trust certain people/organisations,
and forbid everything else.

--

It depends on how we define "self referencing".


'Intensional' self-reference is decidable. This is where the exact same code
appears (either verbatim, like in the Y combinator, or via a pointer, etc.). We
can decide this by just looking for that pattern in the code, without ever
having to run it.


'Extensional' self-reference is undecidable. This is where something behaves in
an equivalent way, but might not look anything like the original. Goedel
statements work in that way: a Goedel statement is a number N which encodes the
statement "the statement encoded by the number N cannot be proved". A more
modern alternative would be a computer program which generates and runs its own
source code (a program which generates its own code is called a Quine).

--

Yes, the halting problem is a Goedel statement for turing machines. The
Church-Turing thesis states that all effective methods of calculation can be
implemented by a turing machine, so we can't avoid the halting problem by
switching to a more powerful system of logic. There are such systems, known as
"oracle machines", but their behaviour cannot be calculated, so they're only
useful for thought experiments.

--

There are infinitely many ways to write the same program (as a simple example,
we could do a bunch of useless work like adding zero to a number, or multiplying
something by 1, etc.). Hence we can't just check whether our code appears
verbatim; we would need to check whether the given code behaves the same as our
program. Spoiler alert: that's equivalent to solving the halting problem!


To prove this, consider programs of the form "Run P then reset the tape then run
our program" for arbitrary programs P. Such programs will behave like our
program for those P which halt, and won't behave like our program for those P
which don't halt (since in that case they'll never get past the "Run P"
step). If we had some way of knowing whether any program behaves like ours or
not, we could use it on this form of program to decide whether any program P
halts of not, hence solving the halting problem!

--

There are always ways to increase the decidable sub-set. It's known as a "full
employment theorem", since there's always work left to do
https://en.wikipedia.org/wiki/Full_employment_theorem

--

The existence of HALTS gives rise to a contradiction, therefore HALTS cannot
exist. In contrast, the existence of a program which can output "error" doesn't
tell us anything about the nature of the universe.
