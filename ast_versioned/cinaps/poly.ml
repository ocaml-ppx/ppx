open StdLabels

module Helpers = struct
  module Hashtbl = struct
    let mapi table ~f =
      let mapped = Hashtbl.create (Hashtbl.length table) in
      Hashtbl.iter
        (fun key data -> Hashtbl.add mapped key (f key data))
        table;
      mapped

    let map table ~f = mapi table ~f:(fun _ x -> f x)

    let of_alist_multi alist =
      let table = Hashtbl.create (List.length alist) in
      List.iter (List.rev alist) ~f:(fun (key, data) ->
        let list =
          match Hashtbl.find_opt table key with
          | None -> [data]
          | Some rest -> data :: rest
        in
        Hashtbl.replace table key list);
      table

    let of_alist_exn alist =
      mapi (of_alist_multi alist) ~f:(fun key list ->
        match list with
        | [] -> assert false
        | [data] -> data
        | _ -> failwith (Printf.sprintf "multiple values for key %s" key))
  end
end

type env = (string * Astlib.Grammar.ty) list
type env_table = (string, env list) Hashtbl.t

let empty_env = []

let find env_table name =
  match Hashtbl.find_opt env_table name with
  | Some list -> list
  | None -> failwith (Printf.sprintf "no monomorphic instances found for %s" name)

let args env = List.map env ~f:snd

let env_is_empty = function
  | [] -> true
  | _ :: _ -> false

let rec ty_suffix ty =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> Ml.id var
  | Name name -> Ml.id name
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | String -> "string"
  | Location -> "location"
  | Loc ty -> ty_suffix ty ^ "_loc"
  | List ty -> ty_suffix ty ^ "_list"
  | Option ty -> ty_suffix ty ^ "_option"
  | Tuple tuple -> tuple_suffix tuple
  | Instance (poly, args) ->
    String.concat ~sep:"_" (List.map args ~f:ty_suffix) ^ "_" ^ Ml.id poly

and tuple_suffix tuple =
  String.concat ~sep:"_and_" (List.map tuple ~f:ty_suffix)

let suffix_of_args args =
  match args with
  | [] -> ""
  | _ -> "_" ^ String.concat ~sep:"_" (List.map args ~f:ty_suffix)

let suffix env = suffix_of_args (args env)

let rec subst_ty ty ~env : Astlib.Grammar.ty =
  match (ty : Astlib.Grammar.ty) with
  | Var string -> List.assoc string env
  | Name _ | Bool | Char | Int | String | Location -> ty
  | Loc ty -> Loc (subst_ty ty ~env)
  | List ty -> List (subst_ty ty ~env)
  | Option ty -> Option (subst_ty ty ~env)
  | Tuple tuple -> Tuple (subst_tuple tuple ~env)
  | Instance (poly, args) -> Instance (poly, subst_tuple args ~env)

and subst_tuple tuple ~env = List.map tuple ~f:(subst_ty ~env)

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
  | Alias ty -> ty_instances ty
  | Record record -> record_instances record
  | Variant variant -> variant_instances variant

let rec transitive_instances decl ~grammar_table =
  let instances = decl_instances decl in
  let transitive =
    List.map instances ~f:(fun (poly, args) ->
      match (Hashtbl.find grammar_table poly : Astlib.Grammar.kind) with
      | Mono _ -> assert false
      | Poly (vars, decl) ->
        let instances = transitive_instances decl ~grammar_table in
        let env = List.combine vars args in
        List.map instances ~f:(fun (poly, args) ->
          (poly, subst_tuple args ~env)))
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
      match (Hashtbl.find grammar_table poly : Astlib.Grammar.kind) with
      | Mono _ -> []
      | Poly (vars, _) -> vars
    in
    poly, List.combine vars args)

let env_table grammar =
  let grammar_table = Helpers.Hashtbl.of_alist_exn grammar in
  Helpers.Hashtbl.map ~f:(List.sort_uniq ~cmp:compare)
    (Helpers.Hashtbl.of_alist_multi
       (grammar_envs grammar ~grammar_table))