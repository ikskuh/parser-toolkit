# PtkGen Semantics

## Context References

tl;dr: `$n` can access the elements of the top-level productions of a rule.

```rb
rule r = "hello" "world" => $0; # access "hello"
rule r = "hello" "world" => $1; # access "world"
```

### Index Resolution

1. Flatten hierarchy
2. Use index in flattened list

```rb
rule r = a b   c d e    f g h; # [ a b  c d e      f g h ] => flat sequence
rule r = a b ( c d e )? f g h; # [ a b  c? d? e?   f g h ] => `c`, `d`, `e` get promoted to optional)
rule r = a b ( c d e )* f g h; # [ a b  [[c d e]]         f g h ] => `c d e` get promoted to list of lists ([[c d e], [c d e], ...])
rule r = a b ( c d e )+ f g h; # [ a b  [[c d e]]         f g h ] => `c d e` get promoted to list of lists ([[c d e], [c d e], ...])
rule r = a b ( c d e )  f g h; # [ a b  c d e  f g h ] => `c d e` gets flattened into the master list
```
