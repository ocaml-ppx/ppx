open! Base

type ty = Astlib.Grammar.ty =
  | Var of string
  | Name of string
  | Bool
  | Char
  | Int
  | String
  | Location
  | Loc of ty
  | List of ty
  | Option of ty
  | Tuple of tuple
  | Instance of string * targ list

and tuple = ty list

and targ = Astlib.Grammar.targ =
  | Tname of string
  | Tvar of string
[@@deriving compare, equal, hash, sexp_of]

type record = (string * ty) list
[@@deriving compare, equal, hash, sexp_of]

type clause = Astlib.Grammar.clause =
  | Empty
  | Tuple of tuple
  | Record of record
[@@deriving compare, equal, hash, sexp_of]

type variant = (string * clause) list
[@@deriving compare, equal, hash, sexp_of]

type decl = Astlib.Grammar.decl =
  | Ty of ty
  | Record of record
  | Variant of variant
[@@deriving compare, equal, hash, sexp_of]

type kind = Astlib.Grammar.kind =
  | Mono of decl
  | Poly of string list * decl
[@@deriving compare, equal, hash, sexp_of]

type t = (string * kind) list
[@@deriving compare, equal, hash, sexp_of]

let rec count_recursions_in_ty = function
  | Name _ | Instance _ -> 1
  | Var _ | Bool | Char | Int | String | Location -> 0
  | Loc ty | List ty | Option ty -> count_recursions_in_ty ty
  | Tuple tuple -> count_recursions_in_tuple tuple

and count_recursions_in_tuple tuple =
  List.sum (module Int) tuple ~f:count_recursions_in_ty

let count_recursions_in_record record =
  List.sum (module Int) record ~f:(fun (_, ty) ->
    count_recursions_in_ty ty)

let count_recursions_in_clause = function
  | Empty -> 0
  | Tuple tuple -> count_recursions_in_tuple tuple
  | Record record -> count_recursions_in_record record

let ty_of_targ targ =
  match targ with
  | Tname name -> Name name
  | Tvar name -> Var name

let rec instantiate_ty ty ~subst =
  match ty with
  | Var var -> instantiate_var var ~subst
  | Name _ | Bool | Char | Int | String | Location -> ty
  | Loc ty -> Loc (instantiate_ty ty ~subst)
  | List ty -> List (instantiate_ty ty ~subst)
  | Option ty -> Option (instantiate_ty ty ~subst)
  | Tuple tuple -> Tuple (instantiate_tuple tuple ~subst)
  | Instance (name, targs) -> Instance (name, instantiate_targs targs ~subst)

and instantiate_var var ~subst = ty_of_targ (instantiate_tvar var ~subst)

and instantiate_tuple tuple ~subst = List.map tuple ~f:(instantiate_ty ~subst)

and instantiate_targs targs ~subst = List.map targs ~f:(instantiate_targ ~subst)

and instantiate_targ targ ~subst =
  match targ with
  | Tvar tvar -> instantiate_tvar tvar ~subst
  | Tname _ -> targ

and instantiate_tvar tvar ~subst =
  match List.Assoc.find subst tvar ~equal:String.equal with
  | Some targ -> targ
  | None -> (* we should not have partial instantiations *) assert false

let instantiate_record record ~subst =
  List.Assoc.map record ~f:(instantiate_ty ~subst)

let instantiate_clause clause ~subst =
  match clause with
  | Empty -> Empty
  | Tuple tuple -> Tuple (instantiate_tuple tuple ~subst)
  | Record record -> Record (instantiate_record record ~subst)

let instantiate_variant variant ~subst =
  List.Assoc.map variant ~f:(instantiate_clause ~subst)

let instantiate_decl decl ~subst =
  match decl with
  | Ty ty -> Ty (instantiate_ty ty ~subst)
  | Record record -> Record (instantiate_record record ~subst)
  | Variant variant -> Variant (instantiate_variant variant ~subst)

let lookup (t : t) ~name = List.Assoc.find t name ~equal:String.equal

let lookup_mono t ~name =
  match lookup t ~name with
  | Some (Mono decl) -> Some decl
  | Some (Poly _) | None -> None

let lookup_instance t ~name ~args =
  match lookup t ~name with
  | Some (Poly (vars, decl)) ->
    (match List.zip vars args with
     | Ok subst -> Some (instantiate_decl decl ~subst)
     | Unequal_lengths -> None)
  | Some (Mono _) | None -> None
