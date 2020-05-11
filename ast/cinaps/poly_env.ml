open Stdppx

type env = (string * Astlib.Grammar.targ) list

let empty_env = []

let args env = List.map env ~f:snd

let create ~vars ~args = List.zip_exn vars args

let uninstantiated vars =
  create ~vars ~args:(List.map vars ~f:(fun v -> Astlib.Grammar.Tvar v))

let env_is_empty = function
  | [] -> true
  | _ :: _ -> false

let targ_to_type targ : Astlib.Grammar.ty =
  match (targ : Astlib.Grammar.targ) with
  | Tname name -> Name name
  | Tvar var -> Var var

let subst_targ targ ~env =
  match (targ : Astlib.Grammar.targ) with
  | Tname _ -> targ
  | Tvar var -> Option.value_exn (List.assoc env var)

let subst_targs targs ~env =
  List.map targs ~f:(subst_targ ~env)

let rec subst_ty ty ~env : Astlib.Grammar.ty =
  match (ty : Astlib.Grammar.ty) with
  | Var string -> targ_to_type (Option.value_exn (List.assoc env string))
  | Name _ | Bool | Char | Int | String | Location -> ty
  | Loc ty -> Loc (subst_ty ty ~env)
  | List ty -> List (subst_ty ty ~env)
  | Option ty -> Option (subst_ty ty ~env)
  | Tuple tuple -> Tuple (subst_tuple tuple ~env)
  | Instance (poly, args) -> Instance (poly, subst_targs args ~env)

and subst_tuple tuple ~env = List.map tuple ~f:(subst_ty ~env)

let subst_fields fields ~env =
  List.map fields ~f:(fun (name, ty) -> (name, subst_ty ~env ty))

let subst_clause ~env clause =
  let open Astlib.Grammar in
  match clause with
  | Empty -> Empty
  | Tuple tyl -> Tuple (subst_tuple ~env tyl)
  | Record fields -> Record (subst_fields ~env fields)

let subst_variants variants ~env =
  List.map variants ~f:(fun (cname, clause) -> (cname, subst_clause ~env clause))

let subst_decl decl ~env =
  let open Astlib.Grammar in
  match decl with
  | Ty ty -> Ty (subst_ty ~env ty)
  | Record fields -> Record (subst_fields ~env fields)
  | Variant variants -> Variant (subst_variants ~env variants)
