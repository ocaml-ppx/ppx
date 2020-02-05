type location = Location.t

type longident = Longident.t =
  | Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident

type longident_loc = longident Location.loc

module type Asttypes =
  module type of struct include Asttypes end
  with type constant := Asttypes.constant

module type Parsetree =
  module type of struct include Parsetree end

module rec Asttypes : Asttypes = Asttypes
module rec Parsetree : Parsetree = Parsetree

include Asttypes
include Parsetree
