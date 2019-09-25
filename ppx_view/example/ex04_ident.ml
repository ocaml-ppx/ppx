open Migrate_parsetree.Ast_407
open Viewlib
open Viewast.Parseview

module Id : sig

  val simple : (string,'i,'o) View.t -> (Longident.t Location.loc,'i,'o) View.t

end = struct

  open View

  let simple view value =
    match%view value with
    | { txt = Lident x; loc = _; } -> view x
    | _ -> error

end


let rec desc : Parsetree.expression -> string = fun expr ->
  match%view expr with
  | Pexp_construct (Id.Simple "false", None) ->
    "false : bool"
  | Pexp_construct (Id.Simple "true", None) ->
    "true : bool"
  | Pexp_construct ({ txt; _ }, None) ->
    (Longident.last txt) ^ " : <<constructor>>"
  | Pexp_construct ({ txt; _ }, Some (Pexp_tuple _)) ->
    (Longident.last txt) ^ " (,,,) : <<constructor>>"
  | Pexp_construct ({ txt; _ }, Some x) ->
    (Longident.last txt) ^ " (" ^ (desc x) ^ ") : <<constructor>>"
  | _ ->
    "???"


let run () =
  let false_ =
    Ast_helper.Exp.construct
      { txt = Lident "false"; loc = Location.none; }
      None
  in
  let true_ =
    Ast_helper.Exp.construct
      { txt = Lident "true"; loc = Location.none; }
      None
  in
  let none =
    Ast_helper.Exp.construct
      { txt = Lident "None"; loc = Location.none; }
      None
  in
  let one =
    Ast_helper.Exp.constant
      (Ast_helper.Const.int 1)
  in
  let some_one =
    Ast_helper.Exp.construct
      { txt = Lident "Some"; loc = Location.none; }
      (Some one)
  in
  Printf.printf "desc false  ~> %S\n%!" (desc false_);
  Printf.printf "desc false  ~> %S\n%!" (desc true_);
  Printf.printf "desc None   ~> %S\n%!" (desc none);
  Printf.printf "desc Some 1 ~> %S\n%!" (desc some_one);
  ()
