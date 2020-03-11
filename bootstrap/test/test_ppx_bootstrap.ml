let loc = Astlib.Location.none

(* AST expressions *)
let expr  = [%expr 1, 2, 3]
let pat   = [%pat? _, 2, 3]
let type_ = [%type: int * int * _]
let stri  = [%stri let x = 1]
let str   = [%str let x = 1 let y = 2]
let sigi  = [%sigi: module M : S]
let sig_  = [%sig: module M : S type t = unit]

(* Ppx_view *)
let f_view x =
  let open Ppx_ast.V4_07 in
  match%view x with
  | Econstraint (expr, core_type) -> Some (expr, core_type)
  | _ -> None
