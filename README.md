# Ppx - Meta-programming for OCaml

**This project is under construction**

For more info, see [this blog post](https://discuss.ocaml.org/t/the-future-of-ppx/).

[![Travis status][travis-img]][travis] [![AppVeyor status][appveyor-img]][appveyor]

[travis]:         https://travis-ci.org/ocaml-ppx/ppxlib
[travis-img]:     https://travis-ci.org/ocaml-ppx/ppxlib.svg?branch=master
[appveyor]:       https://ci.appveyor.com/project/diml/ppxlib/branch/master
[appveyor-img]:   https://ci.appveyor.com/api/projects/status/bogbsm33uvh083jx?svg=true

# Overview

The ppxlib project provides the basis for the ppx system, which is
currently the officially supported method for meta-programming in
OCaml. It offers a principled way to generate code at compile time in
OCaml projects.

Ppxlib comes with a [user manual](http://ppxlib.readthedocs.io/) aimed
at both users and authors of ppx rewriters.

# History

This repository was created by merging several older projects. See
[the history](HISTORY.md) for more details.

# Ppx_view

A ppx rewriter that provides pattern matching on abstract types by
transforming patterns into views/expressions.


Syntax
------

`ppx_view` transforms the patterns of `match`/`function` constructs
wrapped inside `[%view]` extensions by replacing matches against
constructors with function calls.

For instance, in the following code:

```ocaml
match%view expr with
| Constr var -> var
```

the constructor `Constr` is turned into a call to the function
`constr`, which is expected to be a view pattern:

```ocaml
val constr : (string, 'a, 'b) View.t -> (expression, 'a, 'b) View.t
```

Technically, the above expression is rewritten into:

```ocaml
Viewlib.View.match_ __POS__
    [Viewlib.View.case (constr Viewlib.View.__)
       (fun (Viewlib.View.Var_snoc (Viewlib.View.Var_nil, var)) -> var)]
    expr
```

where `__` is used to capture the variable.


Mapping
-------
`ppx_view` applies the following mapping:

- a literal constant `c` of type `typ` is mapped to `View.typ c`;
- an interval pattern `c1..c2` is mapped to `View.interval e1 e2`
  where `ci` is mapped to `ei`;
- a variable pattern is mapped to `View.__`;
- a catch-all pattern is mapped to `View.drop`;
- a record pattern `{ lbl1 = p1; lbl2 = p2; ... }` is mapped to
  `lbl1'match e1 (lbl2'match e2 ...)` where `pi` is mapped to `ei`;
- a constructor pattern `C (p1, ..., pn)` is mapped to `c e1 ... en`
  where `pi` is mapped to `ei`, except for constructors from the core
  library:

  - `Some` is mapped to `View.some`;
  - `None` is mapped to `View.none`;
  - `::` is mapped to `View.cons`;
  - `[]` is mapped to `View.nil`;
  - `()` is mapped to `View.unit`;
  - `true` is mapped to `View.true_`;
  - `false` is mapped to `View.false_`.

Note: the following patterns are currently not supported:

- polymorphic variants;
- lazy;
- module unpacking;
- exceptions.


Library
-------

The `Parseview` module of the library contains the functions
corresponding to the constructors and records from the `Parsetree`
module. Such functions use "shortcuts" to `xyz_desc` fields, allowing
to directly match constructors:

```ocaml
open Viewast

let is_zero : Parsetree.expression -> true = function%view
  | Pexp_constant (Pconst_integer ("0", _)) -> true
  | Pexp_ident { txt = Lident "zero"; _ } -> true
  | _ -> false
```

The access to other fields is done through a `[@view ...]` annotation:

```ocaml
open Viewast

let is_zero : Parsetree.expression -> true = function%view
  | (Pexp_constant (Pconst_integer ("0", _)))[@view { pexp_loc; }] -> true, pexp_loc
  | Pexp_ident { txt = Lident "zero"; loc; } -> true, loc
  | _ -> false, Location.none
```

The library also provides an `Ast_viewer` module that acts as the counterpart
of the `Ast_helper` module. It allows to write patterns very similar to the
corresponding expressions:

```ocaml
open Viewast

let twice_mapper =
  let module H = Ast_helper in
  let module M = Ast_mapper in
  let module V = Ast_viewer in
  let super = M.default_mapper in
  let expr self e =
    match%view super.expr self e with
    | V.Exp.Constant (V.Const.String (str, _)) ->
      H.Exp.constant (H.Const.string (str ^ str))
    | other ->
      other
  and pat self p =
    match%view super.pat self p with
    | V.Pat.Constant (V.Const.String (str, _)) ->
      H.Pat.constant (H.Const.string (str ^ str))
    | other ->
      other
  in
  { super with expr; pat; }
```


Not patterns
------------
Following the proposal in [MPR#7628](https://caml.inria.fr/mantis/view.php?id=7628),
*not* pattern are implemented through the `Not` constructor, that is
rewritten to `View.not`. *Not* patterns allow to write patterns that
are matched if a subpattern is not matched as in:

```ocaml
match%view expr with
| Pexp_constant (Not (Pconst_integer _ | Pconst_float _)) ->
  (* expr is not a number *)
| Pexp_constant (Pconst_integer _) ->
  (* expr is an integer *)
| Pexp_constant (Pconst_float _) ->
  (* expr is a float *)
```

*Not* patterns cannot contain variables.


Limitations
-----------
The major limitations of view patterns are the lack of checks for
non-redundancy and exhaustivity.
