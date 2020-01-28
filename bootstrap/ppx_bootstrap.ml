open! Stdppx

module Expected = struct
  exception Expected of Location.t * string
end

module Extension = struct
  type 'a entry =
    { name : string
    ; callback : (loc:Location.t -> Parsetree.payload -> 'a)
    }

  type t =
    | Patt of Parsetree.pattern    entry
    | Expr of Parsetree.expression entry
end
