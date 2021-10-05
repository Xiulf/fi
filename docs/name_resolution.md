# Name Resolution

## Rules

1. All named exports must be defined in the current module.
2. Constructors and class methods cannot be explicitly exported.
3. If no exports are specified only locally defined items and submodules will be exported.
4. When a type or class is exported all it's constructors or methods are also exported.
5. Using the `module` keyword all exported items from that module can be re-exported.
6. A name is considered private if any of it's path's segments are also private.


## Algorithm

```
module A =

import B

fun a = ...
```

```
module B (module B, module C) =

fun b = ...
```

```
module C =

fun c = ...
```



```
unresolved_imports: [
]

reexports: {
    C: [B],
}

glob_imports: {
    B: [A],
    C: [B],
}

module A {
    scope: {
        values: [
            public a,
            private b,
        ],
        types: [],
        modules: [
            public B,
        ],
    },
    reexports: [],
}

module B {
    scope: {
        values: [
            public b,
            private c,
        ],
        types: [],
        modules: [
            public C,
        ],
    },
    reexports: [c]
}

module C {
    scope: {
        values: [
            public c,
        ],
        types: [],
        modules: [],
    },
    reexports: [],
}
```
