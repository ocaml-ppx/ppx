(** This library is internal to dune and guarantees no API stability. *)

module Bytes    = Bytes
module Filename = Filename
module String   = String
module Char     = Char
module Hashtbl  = MoreLabels.Hashtbl
module Lexing   = Lexing
module Digest   = Digest
module StringLabels = StringLabels
module ListLabels   = ListLabels
module List   = List
module ArrayLabels   = ArrayLabels

type nonrec ('a, 'error) result = ('a, 'error) result =
  | Ok of 'a
  | Error of 'error
