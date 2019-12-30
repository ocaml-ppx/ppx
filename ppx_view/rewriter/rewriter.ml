open Ppx_view_common.Ast_utils
open Ppx_view_common.Ast_utils.Fixed_ast

let raise_error loc str =
  raise Location.(Error (error ~loc str))

let ppx_error loc msg =
  raise_error loc ("ppx: " ^ msg)

let mapper =
  let super = Ast_mapper.default_mapper in
  let expr self e =
    let e = super.expr self e in
    match e.Parsetree.pexp_desc with
    | Pexp_extension ({ txt = "view"; loc; }, payload) ->
      begin match payload with
      | PStr [{ pstr_desc = Pstr_eval (expr, _); _; }] ->
        Ppx_view_lib.Expand.parsetree_payload ~loc expr
      | _ ->
        ppx_error loc "invalid 'view' payload"
      end
    | _ ->
      e
  in
  { super with expr; }


let () =
  Migrate_parsetree.Driver.register
    ~name:"view_pattern"
    (module Fixed_ocaml)
    (fun _ _ -> mapper)
