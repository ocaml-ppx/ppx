# Distribution

A ppx rewriter is distributed as a plain OCaml library. This OCaml
library is expected to be linked with -linkall as it will register an
AST transformation via a toplevel side effect.

# User specification

To specify what ppx rewriters to apply to a file, the user will
provide a list of OCaml library names. For instance: `ppx_sexp_conv`,
`metapp.ppx`

# Preparing a driver

In order to turn a list of ppx rewriters names into a usable ppx
rewriter executable, the build tool will link an executable with all
the ppx library names specified by the user with the `ppx.runner`
library at the end. `ppx.runner` calls the entry point of the driver.

For cases when linking a static executable is not suitable, for
instance for ocamlfind, one case use a tool `ppx` provided by the
`ppx` package. To use this tool, one must compute the transitive
dependencies of the ppx libraries, turn it into a list of cma archive
files and call `ppx` as follow:

    ppx <cma-files> -- [args]

The tool with dynamically load all the cma files (or cmxs files if
using nativedynlink), and will then call the driver entry with the
arguments on the right of `--`.

Note: the ppx package doesn't exist yet. Eventually, it will be the
ocaml-ppx/ppx project we are working on. But if we need something
sooner, we'll just create a proxy for ocaml-migrate-parsetree.

# Calling the driver

This applies to both statically built ppx drivers as well as when
calling `ppx ...`. By default the rewriter will take a source file as
an argument and will pretty print the preprocessed file on standard
output. The following command line options are standardised:

- `--embed-errors`, embed errors into [%ocaml.error] extension points
  and exit with code 0 rather than report them directly
- `--dump-ast`, output a marshaled AST rather than pretty printing
- `--null`, output nothing, used for linting
- `--as-pp`, synonymous for --embed-errors --dump-ast, makes the
  executable suitable to be passed to the -pp option of the compiler
- `--as-ppx`, this one must appear as the first command line
  argument. It makes the executable suitable to be passed to the -ppx
  option of the compiler.
- `--cookie name=<expr>`, set a ppx cookie via the command line

# Runtime libraries

Often, ppx rewriters produce code that refers to libraries that must
implicitly be added to the list of normal OCaml library
dependencies. To find these libraries, the build tool must:

1. compute the transitive closure of the ppx library names specified
   by the user
2. scan this list and for every library X, if a library X.runtime
   exist, add this library to the list of normal OCaml library
   dependencies specified by the user
