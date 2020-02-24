let loc = Location.none

(* AST expressions *)
let expr  = [%expr 1, 2, 3]
let pat   = [%pat? _, 2, 3]
let type_ = [%type: int * int * _]
let stri  = [%stri let x = 1]
let str   = [%str let x = 1 let y = 2]
let sigi  = [%sigi: module M : S]
let sig_  = [%sig: module M : S type t = unit]

(* AST patterns *)
let f_expr = function [%expr 1, 2, 3]                    -> true | _ -> false
let f_pat  = function [%pat? _, 2, 3]                    -> true | _ -> false
let f_type = function [%type: int * int * _]             -> true | _ -> false
let f_stri = function [%stri let x = 1]                  -> true | _ -> false
let f_str  = function [%str let x = 1 let y = 2]         -> true | _ -> false
let f_sigi = function [%sigi: module M : S]              -> true | _ -> false
let f_sig  = function [%sig: module M : S type t = unit] -> true | _ -> false

(* Ppx_view *)
let f_view x =
  let open Ppx_ast.Viewer.V4_07 in
  match%view x with
  | Pexp_constraint (expr, core_type) -> Some (expr, core_type)
  | _ -> None
