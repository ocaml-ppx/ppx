open StdLabels

let version = Version.of_string "unstable_for_testing"

module Stable = Version_4_07

let rec update_ty ty : Grammar.ty =
  match (ty : Grammar.ty) with
  | Var _ -> ty
  | Name _ -> ty
  | Bool | Char | Int | String -> ty
  | Location -> ty
  | Loc ty -> Loc (update_ty ty)
  | List ty -> List (update_ty ty)
  | Option ty -> Option (update_ty ty)
  | Tuple tuple -> Tuple (update_tuple tuple)
  | Instance _ -> ty

and update_tuple tuple =
  List.rev_map tuple ~f:update_ty

let update_record record =
  List.rev_map record ~f:(fun (name, ty) ->
    name, update_ty ty)

let update_clause clause : Grammar.clause =
  match (clause : Grammar.clause) with
  | Empty -> Empty
  | Tuple tuple -> Tuple (update_tuple tuple)
  | Record record -> Record (update_record record)

let update_variant variant =
  List.rev_map variant ~f:(fun (name, clause) ->
    name, update_clause clause)

let update_decl decl : Grammar.decl =
  match (decl : Grammar.decl) with
  | Ty ty -> Ty (update_ty ty)
  | Record record -> Record (update_record record)
  | Variant variant -> Variant (update_variant variant)

let update_kind kind : Grammar.kind =
  match (kind : Grammar.kind) with
  | Mono decl -> Mono (update_decl decl)
  | Poly (args, decl) -> Poly (List.rev args, update_decl decl)

let update_grammar grammar =
  List.rev_map grammar ~f:(fun (name, kind) ->
    name, update_kind kind)

let grammar = update_grammar Stable.grammar

let rec update_data data : _ Ast.data =
  match (data : _ Ast.data) with
  | Node _ -> data
  | Bool _ -> data
  | Char _ -> data
  | Int _ -> data
  | String _ -> data
  | Location _ -> data
  | Loc loc -> Loc (Loc.map loc ~f:update_data)
  | List list -> List (List.map list ~f:update_data)
  | Option None -> Option None
  | Option (Some data) -> Option (Some (update_data data))
  | Tuple array -> Tuple (update_array array)
  | Record array -> Record (update_array array)
  | Variant { tag; args } -> Variant { tag; args = update_array args }

and update_array array =
  let len = Array.length array in
  Array.init len ~f:(fun i ->
    update_data array.(len - i - 1))

let update_ast (ast : _ Ast.t) =
  { ast with data = update_data ast.data }

let to_stable : History.conversion =
  { src_version = version
  ; dst_version = Stable.version
  ; f = fun ast ~unwrap:_ ~wrap:_ -> Some (update_ast ast)
  }

let of_stable : History.conversion =
  { src_version = Stable.version
  ; dst_version = version
  ; f = fun ast ~unwrap:_ ~wrap:_ -> Some (update_ast ast)
  }

let conversions = [ to_stable; of_stable ]
