---
title: Goedel Machines
packages: [ 'coqNoIde' ]
---

<!-- Quick way to build up our Coq script -->
```{pipe="cat > append"}
echo "" >> gm.v
tee -a gm.v
```

```{pipe="sh"}
chmod +x append
```

## Specification ##

```{pipe="./append" .ocaml}
Definition memory := Vector bool.
Theorem oneplusone (x : nat) : 1 + 1 = 2.
```

## Implementation ##

<!-- Check the Coq script -->

```{pipe="sh" .unwrap}
if $(coqc gm.v > result)
then
  echo "Success" | pandoc -t json
else
  echo '<h2 style="background: red; color: black;">Error Checking Script</h2>' | pandoc -t json
  echo "Show." | ./append > /dev/null
  coqc gm.v > result
  true
fi
```

```{pipe="sh"}
cat result
```
