---
title: Zero, One, Many
packages: ['racketWithRackCheck']
---

```{pipe="sh"}
bash $setup attic.rkt
for DEPENDENCY in "$numbers_in_scheme"
do
  ./extract "$DEPENDENCY"
done
```

TODO
