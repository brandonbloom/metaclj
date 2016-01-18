# Meta-Clojure


## Overview

Meta-Clojure provides staged compilation for Clojure. It includes a form of
syntax quoting that is aware of both local environments and special-forms.
Among other things, this makes many macros easier to write. Perhaps more
importantly, it simplifies control over when code gets evaluated or compiled.

### Usage

```clojure
(require '[metaclj.core :refer [defmeta syntax] :as meta])
```

### Meta-Macros

The `defmeta` form is analogous to `defmacro`, but is expected to return
Syntax objects (forms plus their environments) instead of plain forms.

```clojure
(defmeta my-if [test then else]
  (syntax (if test then else)))
```

Note that you don't need to unquote any of the parameters to `if`, since the
`syntax` form is aware of the meta-macro's environment.

### Call-By-Name

Since it's common for macros to have a body that always templates code with
a syntax-quoter, the convenience macro `defbn` provides a way to create
"call-by-name" macros:

```clojure
(defbn my-if [test then else]
  (if test then else))
```

Both versions of `my-if` have correct "lazy" behavior: they will only evaluate
one arm of the conditional.

### Staged Compilation

The `meta/do` macro will perform meta-quoting to zero or more forms, then
evaluate each of them:

```clojure
(meta/do 1 2 3)
;;=> 3
```

Combined with unquoting, this enables you to perform arbitrary computation at
compile time:

```clojure
(let [x 2 y 4]
  (meta/do ~(+ x y)))
;;=> 6
```

Unquoting is syntax aware and provides automatic splicing:

```
(let [args (syntax 2 4)]
  (meta/do ~(+ args)))
;;=> 6
```

You can use function expressions to defer computation. Note that the unquoted
expression will still be evaluated at compile time:

```clojure
(let [x 2 y 4]
  (meta/do (fn [] ~(+ x y))))
;=> #<Fn@32b2ad39 user/eval15784$fn__15788>
```

You can provide this to yourself by using `meta/translate`, which is a cousin
of `macroexpand-all`:

```clojure
(let [x 2 y 4]
  (meta/translate (fn [] ~(+ x y))))
=> ((fn* ([] 6)))
```

Note that the returned value is wrapped in a seq, since Meta-Clojure uniformly
supports multiple expressions with implicit splicing:

```clojure
(let [x 2]
  (meta/translate 1 x 3))
;=> (1 2 3)
```


## Status

- The comments at the bottom of [core.clj](./src/metaclj/core.clj) and
  the code in [core_test.clj](./test/metaclj/core_test.clj) form my testbed.
- Many known bugs and incomplete behavior.
- Some special forms not yet supported: `case`, `deftype`, and `reify`.
- Many of the cooler things you can do with this are neither possible nor
  obvious from this prototype.
- Lots of the code left to be cannibalized from [EClj][4].
- No progress yet on [Exotypes][4]
- Use of `clojure.core/eval` is unavoidable at the top level, but it could
  be compiled away for more interior forms.


## References

- [Multi-stage Programming][1]
- [Terralang][2]
- [MetaOCaml][3]
- [EClj][4]
- [Exotypes][5]


## License

Copyright © 2016 Brandon Bloom

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.


[1]: https://www.cs.rice.edu/~taha/MSP/
[2]: http://terralang.org/
[3]: http://okmij.org/ftp/ML/MetaOCaml.html
[4]: https://github.com/brandonbloom/eclj
[5]: http://terralang.org/pldi083-devito.pdf
