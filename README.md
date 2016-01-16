# Meta-Clojure


## Overview

Meta-Clojure is a proof of concept implementation of staged compilation for
Clojure. It includes a form of syntax quoting that is aware of both
environments and special-forms. Among other things, this makes many macros
easier to write.

The `defmeta` form is analogous to `defmacro`, but is expected to return
Syntax objects (forms plus their environments) instead of plain forms.

```clojure
(defmeta my-if [test then else]
  (syntax (if test then else)))
```

Note that you don't need to unquote any of the parameters to `if`, since the
`syntax` form is aware of the meta-macro's environment.

Since it's common for macros to have a body that always templates code with
a syntax-quoter, the convenience macro `defbn` provides a way to create
"call-by-name" macros:

```clojure
(defbn my-if [test then else]
  (if test then else))
```


## Status

- The comments at the bottom of [core.clj](./src/metaclj/core.clj) are my
  testbed.
- Many known bugs and incomplete behavior.
- Far from all Clojure forms are supported so far.
- While unquoting is less common, it's still useful, but not yet implemented.
- Many of the cooler things you can do with this are neither possible nor
  obvious from this prototype.
- Lots of the code left to be cannibalized from [EClj][4].
- No progress yet on [Exotypes][4]


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
