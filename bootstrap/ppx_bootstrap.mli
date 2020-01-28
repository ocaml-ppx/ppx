open! Stdppx

module Expected : sig
  exception Expected of Location.t * string
end

module Extension : sig
  type 'a entry =
    { name : string
    ; callback : (loc:Location.t -> Parsetree.payload -> 'a)
    }

  type t =
    | Patt of Parsetree.pattern    entry
    | Expr of Parsetree.expression entry
end
