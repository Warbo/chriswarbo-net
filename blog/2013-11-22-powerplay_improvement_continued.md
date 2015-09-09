---
title: PowerPlay Improvement continued
---
## Defining Improvement ##

Over the past few weeks I've been hacking on the PowerPlay implementation I mentioned in a [previous post] [1]. PowerPlay is a meta-algorithm, which may allow existing problem-solving algorithms to improve over time.

[1] http://chriswarbo.net/index.php?page=news&amp;type=view&amp;id=admin-s-blog%2Fpowerplay-improvement

The definition of 'improve' used by PowerPlay is to keep a list of regression tests: problems which we know can already be solved. We consider a solver S2 to be an improvement over a solver S1 if S2 can solve all of our regression tests and we can find a problem it can solve which S1 cannot solve. If we manage to do this, we put this new problem in our list of tests and start using S2 as our problem solver.

Maybe a flow chart will explain better:

```{pipe="cat > chart1.dit"}
+-----------------------------+
| Set solver to Constant fail |
| Set tests  to Empty         |
+-----------------------------+
              |            /------\
              |            |      |
              V            V      |
 +---------------------------+    |
 | Search for a new solver S |    |
 | and a new problem P       |    |
 +---------------------------+    |
              |                   ^
              |                   |
              V                   |
      +----------------+          |
      | Can S solve P? |          |
      +----------------+          |
       Yes |      | No            |
           |      \-------->------+
           V                      |
   +----------------------+       |
   |Can S solve all tests?|       ^
   +----------------------+       |
      Yes |        | No           |
          |        \------->------+
          V                       |
     +-------------------+        |
     | Set solver to S   |        ^
     | Append P to tests |        |
     +-------------------+        |
              |                   |
              \------------>------/
```

```{.unwrap pipe="sh | pandoc -t json"}
nix-shell -p ditaa --run "ditaa chart1.dit" > /dev/null
./root/static/file2img.sh "Chart 1" < chart1.png
```

As I talked about in my previous post, this is a rather unsatisfactory notion of improvement. Firstly, it only tracks performance against the small, finite number of problems in our list of regression tests. Secondly, we must explicitly keep this list in memory, adding a memory cost linear in the number of improvements.

Instead, we can abandon the list and make one all-encompassing regression test: S2 is only an improvement over S1 if every problem solvable by S1 is also solvable by S2 (plus there's a problem solvable by S2 but not S1, as before). Here's the corresponding flow chart:

```{pipe="cat > chart2.dit"}
+-----------------------------+
| Set solver to Constant fail |
+-----------------------------+
              |            /----------\
              |            |          |
              V            V          |
 +---------------------------+        |
 | Search for a new solver S |        |
 | and a new problem P       |        |
 +---------------------------+        |
              |                       ^
              |                       |
              V                       |
      +----------------+              |
      | Can S solve P? |              |
      +----------------+              |
       Yes |      | No                |
           |      \------------->-----+
           V                          |
+-------------------------------+     |
| Does being solvable by solver |     ^
| imply being solvable by S?    |     |
+-------------------------------+     |
      Yes |           | No            |
          |           \--------->-----+
          V                           |
     +-------------------+            |
     | Set solver to S   |            ^
     +-------------------+            |
               |                      |
               \---------------->-----/
```

```{.unwrap pipe="sh | pandoc -t json"}
nix-shell -p ditaa --run "ditaa chart2.dit" > /dev/null
./root/static/file2img.sh "Chart 2" < chart2.png
```

I spent some time trying to support both of these notions of improvement, as pluggable modules, but the code got pretty hairy pretty quickly. I've now abandoned the list-based criterion entirely in favour of the universally-quantified one. This has resulted in a much simpler specification, available in the Simple.v file of the [git repository] [2].

[2]: https://gitorious.org/powerplay

The specification is currently quite conservative: it guarantees that implementations never lose the ability to solve a problem, but it allows trivial implementations which never improve. It is simple to prove perpetual improvement for particular problem domains (eg. those mentioned below), but I haven't yet worked out a nice set of sufficient and necessary conditions which are general enough to bake into the spec.
