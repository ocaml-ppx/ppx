type location = Location.t

type longident = Longident.t =
  | Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident

type longident_loc = longident Location.loc

include module type of struct include Asttypes end with type constant := Asttypes.constant
include module type of struct include Parsetree end
