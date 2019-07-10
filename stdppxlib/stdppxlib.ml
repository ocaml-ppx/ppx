module Appendable_list = Appendable_list
module Ansi_color = Ansi_color
module Array      = Array
module Bytes      = Bytes
module Comparable = Comparable
module Either     = Either
module Exn        = Exn
module Exn_with_backtrace = Exn_with_backtrace
module Filename   = Filename
module Hashtbl    = Hashtbl
module Table      = Table
module Int        = Int
module Io         = Io
module List       = List
module Map        = Map
module Option     = Option
module Ordering   = Ordering
module Pp         = Pp
module Result     = Result
module Set        = Set
module Staged     = Staged
module String     = String
module Char       = Char
module Bool       = Bool
module Sexp_intf  = Sexp_intf
module Sexp       = Sexp
module Fmt        = Fmt
module Unit       = Unit
module Fn         = Fn
module Dyn        = Dyn
module Float      = Float
module Poly       = Poly
module Code_error = Code_error

external reraise : exn -> _ = "%reraise"

let compare a b = Ordering.of_int (compare a b)

(* The following types are re-exported here so that they are always
   available in scope *)

type ('a, 'error) result = ('a, 'error) Result.t =
  | Ok    of 'a
  | Error of 'error

type ('a, 'b) either = ('a, 'b) Either.t =
  | Left  of 'a
  | Right of 'b

type ordering = Ordering.t =
  | Lt
  | Eq
  | Gt

let sprintf = Printf.sprintf
