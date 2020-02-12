open! Stdppx

module Expected = struct
  exception Expected of Astlib.Location.t * string
end

module Extension = struct
  type 'a entry =
    { name : string
    ; callback : (loc:Astlib.Location.t -> Ppx_ast.payload -> 'a)
    }

  type t =
    | Patt of Ppx_ast.pattern    entry
    | Expr of Ppx_ast.expression entry
end
