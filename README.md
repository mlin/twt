# "The Whitespace Thing" for OCaml

http://github.com/mlin/twt

**Maintainer: [Mike Lin](http://www.mlin.net/)**

"The Whitespace Thing" for OCaml is a preprocessor (invoked as `ocaml+twt`) that
uses indentation to auto-parenthesize multi-line expressions, like in Python and
Haskell. Using natural indentation patterns, it eliminates:

* the `;` operator for statement sequences and the `;;` top-level statement operator
* multi-line parenthesization nested function applications
* ambiguity involving nested let, if-else, and try-with expressions, and resulting parentheses
* the parenthetical keywords `in`, `done`, `begin`, and `end`

The language syntax is otherwise the same as OCaml's, with a few restrictions.

Version 1 is implemented as a line-oriented preprocessor; this is something of
a hack. At some unspecified time in the future, it should be rewritten with a
proper syntax tree parser, although the current approach honestly gets quite
far!

### Code examples

<table style="vertical-align: top">
<tr>
<td>
<strong>ocaml</strong>
</td>
<td>
<strong>ocaml+twt</strong>
</td>
</tr>
<tr>
<td>
<sub>
<pre>
let rec main magic_number =
 Printf.printf "Your guess? ";
 let guess = int_of_string (read_line ()) in
  if guess > magic_number then
   (Printf.printf "Too high!\n";
    main magic_number)
  else if guess < magic_number then
   (Printf.printf "Too low!\n";
    main magic_number)
  else
   (Printf.printf "You win!\n";
    exit 0);;

Random.self_init ();;

main (Random.int 100);;
</pre>
</sub>
</td>
<td>
<sub>
<pre>
let rec main magic_number =
 Printf.printf "Your guess? "
 let guess = int_of_string (read_line ())
 if guess > magic_number then
  Printf.printf "Too high!\n"
  main magic_number
 else if guess < magic_number then
  Printf.printf "Too low!\n"
  main magic_number
 else
  Printf.printf "You win!\n"
  exit 0

Random.self_init ()

main (Random.int 100)
</pre>
</sub>
</td>
</tr>
<tr>
<td>
<sub>
<pre>
let list_out lst =
 (List.map
  (function Some x -> x)
  (List.filter
   (function Some x -> true | None -> false)
   lst))
</pre>
</sub>
</td>
<td>
<sub>
<pre>
let list_out lst =
 List.map
  function Some x -> x
  List.filter
   function Some x -> true | None -> false
   lst
</pre>
</sub>
</td>
</tr>
<tr>
<td>
<sub>
<pre>
for i = 1 to 10 do
 print_int i;
 print_newline ()
done;
print_string "done"
</pre>
</sub>
</td>
<td valign="top">
<sub>
<pre>
for i = 1 to 10 do
 print_int i
 print_newline ()
print_string "done"
</pre>
</sub>
</td>
</tr>
<tr>
<td>
<sub>
<pre>
let contrived = function
   s when (String.length s) > 0 ->
    begin
     try
      Some (float_of_string s)
     with
      Failure _ -> Some nan
    end
 | _ -> None
</pre>
</sub>
</td>
<td valign="top">
<sub>
<pre>
let contrived = function
 | s when (String.length s) > 0 ->
    try
     Some (float_of_string s)
    with
     | Failure _ -> Some nan
 | _ -> None
</pre>
</sub>
</td>
</tr>
</table>

### Language documentation

See the [quick reference](https://github.com/mlin/twt/raw/master/doc/quick_reference.pdf),
and the longer [examples/](https://github.com/mlin/twt/tree/master/examples).

### Installation

ocaml+twt is available in [OPAM](http://opam.ocamlpro.com):
`opam install ocaml+twt`. Once installed, the ocaml+twt executable is
available in your path when you `eval $(opam config env)`.

Without OPAM, extract the source tarball and `make install`.  This installs
the executable to `PREFIX/bin` where `PREFIX=/usr/local`. You can override
this with `make install PREFIX=/home/alice`

### Usage

To use the preprocessor, either manually invoke it using `ocaml+twt mycode.ml`
and pipe the results to a file, or use the preprocessor flag to ocamlc:

```ocamlc -pp ocaml+twt mycode.ml```

There are a few optional behaviors available for the preprocessor. They're
pretty self-explanatory by looking at the usage printed by invoking ocaml+twt.

With ocamlbuild, you can just add something like this to the `_tags` file in
your project directory:

```<**/*.ml> or <**/*.mli>: pp(ocaml+twt)```

If you use OCamlMakefile, you can make the first line of your file
`(*pp ocaml+twt *)`.

### Tips and FAQs

* **Parentheses:** The preprocessor completely ignores anything inside parentheses, including newlines. Thus, any sub-expressions also need to be parenthesized, regardless of how they're indented. Occasionally, parenthesization is useful to work around the preprocessor if it doesn't understand some obscure OCaml syntax.
* **Performance:** The syntax transform does not add any expressions or statements, only parentheses. Thus, there should be no performance impact in the final product.
* **Comments:** should not occur (or terminate) at the *beginning* of a line that also has code on it.
* **ocamldoc:** You should be able to comment things as usual and run ocamldoc on the *postprocessed* code.
* **Toplevel:** no support and none likely, sorry.
* **Pattern matching:** If the consequence of a pattern is a sequence of statements, make sure to place them either all on one line (separated by ;) or entirely in their own block. That is:
<table>
<tr>
<td>
instead of...
</td>
<td>
do...
</td>
</tr>
<tr>
<td valign="top">
<pre>
match n with
  | 1 -> print_string "one"
         print_endline ()
</pre>
</td>
<td>
<pre>
match n with
  | 1 ->
     print_string "one"
     print_endline ()
</pre>
</td>
</tr> 
</table>
Of course, if the consequent is just a single expression, you can place it on the same line. This restriction is actually true almost everywhere, such as let bodies and if-then consequents; see the quick reference. The rule of thumb: **if an expression spans multiple lines, it must begin on its own line.**
* **Applications:** In multi-line function applications, if the function being applied is some complicated expression (rather than just an identifier), you must parenthesize it:
<table>
<tr>
<td>
instead of...
</td>
<td>
do...
</td>
</tr>
<tr>
<td valign="top">
<pre>
if b then (+) else (-)
  x
  y
</pre>
</td>
<td>
<pre>
(if b then (+) else (-))
  x
  y
</pre>
</td>
</tr> 
<tr>
<td valign="top">
<pre>
function
  | x when x >= 0 -> (+)
  | _ -> (-)
  x
  y
</pre>
</td>
<td valign="top">
<pre>
(function
  | x when x >= 0 -> (+)
  | _ -> (-))
  x
  y
</pre>
</td>
</tr>
</table>


### ppcompose utility

Because the -pp flag to ocamlc is somewhat limited, I included a 'ppcompose'
utility that makes it simple to compose several preprocessors. For example,
one can compose a list comprehension camlp4 syntax with ocaml+twt as follows:

```ocamlc -pp "ppcompose 'camlp4o pa_compr.cmo' 'ocaml+twt -spaceonly'" source.ml```

The last preprocessor specified on the command line is applied first to the
source code. This means you usually want to put ocaml+twt last.

### Useful links

* [Understanding GNU Emacs and Tabs](http://www.pement.org/emacs_tabs.htm)
* [Python: Myths about Indentation](http://www.secnetix.de/~olli/Python/block_indentation.hawk), largely applicable to ocaml+twt as well
* [F# lightweight syntax](http://blogs.msdn.com/dsyme/archive/2006/08/24/715626.aspx), a similar idea for the OCaml-derived language for .NET. The lightweight syntax seems to be much more popular than the "normal" syntax among F# users. (For the record, ocaml+twt [preceded this](http://groups.google.com/group/fa.caml/browse_thread/thread/50009fed6088f114/4e5d40b372f87878?lnk=gst&q=ocaml%2Btwt#4e5d40b372f87878) by about nine months; I don't know if there was any causal relationship.)

### Version history

7/5/15 version 0.93.3
- New OCaml 4.02 features: quoted strings and ppx infix extension
  nodes (both are simply passed through)

10/05/13 version 0.93.2
- New OCaml 4.01 keyword: open!
- New command-line flag -lwt to accept pa_lwt code (recognize
  lwt, for_lwt, while_lwt, try_lwt, match_lwt, etc.)

10/24/12 version 0.931
- Minor changes to installation procedure to support packaging

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
