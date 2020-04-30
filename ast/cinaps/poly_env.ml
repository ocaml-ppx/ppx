open Stdppx

type env = (string * Astlib.Grammar.targ) list
type env_table = (string, env list) Hashtbl.t

let empty_env = []

let find env_table name =
  match Hashtbl.find env_table name with
  | Some list -> list
  | None -> failwith (Printf.sprintf "no monomorphic instances found for %s" name)

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

let rec ty_instances ty =
  match (ty : Astlib.Grammar.ty) with
  | Var _ | Name _ | Bool | Char | Int | String | Location -> []
  | Loc ty | List ty | Option ty -> ty_instances ty
  | Tuple tuple -> tuple_instances tuple
  | Instance (poly, args) -> [(poly, args)]

and tuple_instances tuple =
  List.concat (List.map tuple ~f:ty_instances)

let record_instances record =
  List.concat (List.map record ~f:(fun (_, ty) -> ty_instances ty))

let clause_instances clause =
  match (clause : Astlib.Grammar.clause) with
  | Empty -> []
  | Tuple tuple -> tuple_instances tuple
  | Record record -> record_instances record

let variant_instances variant =
  List.concat (List.map variant ~f:(fun (_, clause) -> clause_instances clause))

let decl_instances decl =
  match (decl : Astlib.Grammar.decl) with
  | Ty ty -> ty_instances ty
  | Record record -> record_instances record
  | Variant variant -> variant_instances variant

let rec transitive_instances decl ~grammar_table =
  let instances = decl_instances decl in
  let transitive =
    List.map instances ~f:(fun (poly, args) ->
      match (Hashtbl.find_exn grammar_table poly : Astlib.Grammar.kind) with
      | Mono _ -> assert false
      | Poly (vars, decl) ->
        let instances = transitive_instances decl ~grammar_table in
        let env = List.combine vars args in
        List.map instances ~f:(fun (poly, args) ->
          (poly, subst_targs args ~env)))
  in
  instances @ List.concat transitive

let grammar_instances grammar ~grammar_table =
  List.concat
    (List.map grammar ~f:(fun (_, kind) ->
       match (kind : Astlib.Grammar.kind) with
       | Poly _ -> []
       | Mono decl -> transitive_instances decl ~grammar_table))

let grammar_envs grammar ~grammar_table =
  List.map (grammar_instances grammar ~grammar_table) ~f:(fun (poly, args) ->
    let vars =
      match (Hashtbl.find_exn grammar_table poly : Astlib.Grammar.kind) with
      | Mono _ -> []
      | Poly (vars, _) -> vars
    in
    poly, List.combine vars args)

let env_table grammar =
  let grammar_table = Hashtbl.of_list_exn grammar in
  Hashtbl.map ~f:(List.sort_uniq ~compare)
    (Hashtbl.of_list_multi
       (grammar_envs grammar ~grammar_table))
