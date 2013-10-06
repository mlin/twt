# "The Whitespace Thing" for OCaml
http://people.csail.mit.edu/mikelin/ocaml+twt/

http://github.com/mlin/twt

**Maintainer: [Mike Lin](https://blog.mlin.net/)**

"The Whitespace Thing" for OCaml is a preprocessor (invoked as ocaml+twt) that
uses indentation to auto-parenthesize multiline expressions, as in Python and
Haskell, thus eliminating a lot of syntax clutter.

Version 1 is implemented as a line-oriented preprocessor; this is something of
a hack. At some unspecified time in the future, Version 2 should be written as
a camlp4 syntax (although this promises to be difficult).

### Installation

ocaml+twt is available in [OPAM](http://opam.ocamlpro.com):
`opam install twt`. Once installed, the ocaml+twt executable is available in
your path when you `eval $(opam config env)`.

Without OPAM, extract the source tarball and `make install`.  This installs
the executable in the same directory as ocamlc. To override this, use
`make INSTALLDIR=/some/path install`

### Usage

To use the preprocessor, either manually invoke it using `ocaml+twt mycode.ml`
and pipe the results to a file, or use the preprocessor flag to ocamlc:

```ocamlc -pp ocaml+twt mycode.ml```

There are a few options available for the preprocessor. They're pretty
self-explanatory by looking at the usage printed by invoking ocaml+twt.

With ocamlbuild, you can just add something like this to the _tags file in
your project directory:

```<**/*.ml> or <**/*.mli>: pp(ocaml+twt)```

If you use OCamlMakefile, you can make the first line of your file
`(*pp ocaml+twt *)`.

### Language documentation

See the [quick reference](https://github.com/mlin/twt/raw/master/doc/quick_reference.pdf),
and the [examples/](https://github.com/mlin/twt/tree/master/examples).

### ppcompose utility

Because the -pp flag to ocamlc is somewhat limited, I included a 'ppcompose'
utility that makes it simple to compose several preprocessors. For example,
one can compose a list comprehension camlp4 syntax with ocaml+twt as follows:

```ocamlc -pp "ppcompose 'camlp4o pa_compr.cmo' 'ocaml+twt -spaceonly'" source.ml```

The last preprocessor specified on the command line is applied first to the
source code. This means you usually want to put ocaml+twt last.

### Version history

10/05/13 version 0.93.2
- New OCaml 4.01 keyword: open!
- New command-line flag -lwt to accept pa_lwt code (recognize
  lwt, for_lwt, while_lwt, try_lwt, match_lwt, etc.)

02/01/12 version 0.93
- Handle OCaml 3.12's new 'include module type of...' in module
  signatures
- Allow pass-through of named operands in applications: f ~x
- Allow one-liner consequents for predicates:
    if pred1 then consq1
    else if pred2 then consq2
    else consq3
- Allow one-liner try and with bodies (similar):
    try expr1
    with Not_found -> expr2

08/02/10 version 0.92
- Accepts piped input
- Supports output to file (new option -o)
- ppcompose properly cleans up after itself, and makes less stderr noise
  by default (new option -v)
- New ocaml 3.12 keywords: val! method! inherit!
- bugfix: doesn't screw up with character literals '(' and ')'
- bugfix: allows passed-along optional arguments on their own lines

03/11/08 version 0.91
- Bug fixes to handling of escaped characters within string and
character literals

01/16/07 version 0.90
- Major backwards-incompatible change: elimination of "in" from let,
and elimination of requirement to indent let body.

12/10/06 version 0.86
- moved all parentheses inserted by the preprocessor from the
beginning of a line to the end of the previous line, making column
numbers in error messages usually match up, and improving the
readability of the postprocessed code. Thanks to Ingo Bormuth for the
idea and patch.

07/24/06 version 0.85
- ocamlc now shows the preprocessed filename in errors
- bugfix: your labelled or optional arguments on their own lines can
now have underscores in their names
- bugfix: you can now declare exceptions in module signatures
- bugfix: you can now have type constraints in object definitions
- bugfix: you can now have recursive object types (not thoroughly tested)
- bugfix: you can now have #load and other directives

02/19/06 version 0.81
- added the 'ppcompose' utility
- bugfix: you can now have a character literal on its own line
- bugfix: you can now have a labelled or optional argument on its own line
- bugfix: you can now have a polymorphic variant constructor on its own line

11/21/05 version 0.8
- initial release. works quite well, but the preprocessor's handling
of various pathological cases of syntax involving objects, module
signatures, and functors has not been rigorously tested.
