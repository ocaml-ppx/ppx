open! Base

type 'node data = 'node Astlib.Ast.data =
  | Node of 'node
  | Bool of bool
  | Char of char
  | Int of int
  | String of string
  | Location of Location.t
  | Loc of 'node data Loc.t
  | List of 'node data list
  | Option of 'node data option
  | Tuple of 'node data array
  | Record of 'node data array
  | Variant of { tag : string; args : 'node data array }
[@@deriving compare, equal, sexp_of]

type 'node t = 'node Astlib.Ast.t = { name : string; data : 'node data }
[@@deriving compare, equal, sexp_of]

let rec matches node ~grammar ~to_ast =
  let ast : _ t = to_ast node in
  match Grammar.lookup_mono grammar ~name:ast.name with
  | Some decl -> data_matches_decl ast.data ~decl ~grammar ~to_ast
  | None -> false

and matches_instance node ~args ~grammar ~to_ast =
  let ast : _ t = to_ast node in
  match Grammar.lookup_instance grammar ~name:ast.name ~args with
  | Some decl -> data_matches_decl ast.data ~decl ~grammar ~to_ast
  | None -> false

and data_matches_decl data ~decl ~grammar ~to_ast =
  match decl with
  | Ty ty -> data_matches_ty data ~ty ~grammar ~to_ast
  | Record record -> data_matches_record data ~record ~grammar ~to_ast
  | Variant variant -> data_matches_variant data ~variant ~grammar ~to_ast

and data_matches_variant data ~variant ~grammar ~to_ast =
  match data with
  | Variant { tag; args } ->
    (match List.Assoc.find variant tag ~equal:String.equal with
     | None -> false
     | Some clause -> array_matches_clause args ~clause ~grammar ~to_ast)
  | _ -> false

and data_matches_record data ~record ~grammar ~to_ast =
  match data with
  | Record fields -> array_matches_record fields ~record ~grammar ~to_ast
  | _ -> false

and array_matches_clause array ~clause ~grammar ~to_ast =
  match clause with
  | Empty -> Array.is_empty array
  | Tuple tuple -> array_matches_tuple array ~tuple ~grammar ~to_ast
  | Record record -> array_matches_record array ~record ~grammar ~to_ast

and array_matches_record array ~record ~grammar ~to_ast =
  Array.length array = List.length record
  && List.for_alli record ~f:(fun i (_, ty) ->
    data_matches_ty array.(i) ~ty ~grammar ~to_ast)

and array_matches_tuple array ~tuple ~grammar ~to_ast =
  Array.length array = List.length tuple
  && List.for_alli tuple ~f:(fun i ty ->
    data_matches_ty array.(i) ~ty ~grammar ~to_ast)

and data_matches_ty data ~ty ~grammar ~to_ast =
  match ty with
  | Var _ -> (* type should be monomorphic *) assert false
  | Name name -> data_matches_name data ~name ~grammar ~to_ast
  | Bool -> data_matches_bool data
  | Char -> data_matches_char data
  | Int -> data_matches_int data
  | String -> data_matches_string data
  | Location -> data_matches_location data
  | Loc ty -> data_matches_loc data ~ty ~grammar ~to_ast
  | List ty -> data_matches_list data ~ty ~grammar ~to_ast
  | Option ty -> data_matches_option data ~ty ~grammar ~to_ast
  | Tuple tuple -> data_matches_tuple data ~tuple ~grammar ~to_ast
  | Instance (name, args) -> data_matches_instance data ~name ~args ~grammar ~to_ast

and data_matches_name data ~name ~grammar ~to_ast =
  match data with
  | Node node ->
    let ast : _ t = to_ast node in
    String.equal ast.name name && matches node ~grammar ~to_ast
  | _ -> false

and data_matches_bool     = function Bool     _ -> true | _ -> false
and data_matches_char     = function Char     _ -> true | _ -> false
and data_matches_int      = function Int      _ -> true | _ -> false
and data_matches_string   = function String   _ -> true | _ -> false
and data_matches_location = function Location _ -> true | _ -> false

and data_matches_loc data ~ty ~grammar ~to_ast =
  match data with
  | Loc loc -> data_matches_ty loc.txt ~ty ~grammar ~to_ast
  | _ -> false

and data_matches_list data ~ty ~grammar ~to_ast =
  match data with
  | List list -> List.for_all list ~f:(data_matches_ty ~ty ~grammar ~to_ast)
  | _ -> false

and data_matches_option data ~ty ~grammar ~to_ast =
  match data with
  | Option option -> Option.for_all option ~f:(data_matches_ty ~ty ~grammar ~to_ast)
  | _ -> false

and data_matches_tuple data ~tuple ~grammar ~to_ast =
  match data with
  | Tuple values -> array_matches_tuple values ~tuple ~grammar ~to_ast
  | _ -> false

and data_matches_instance data ~name ~args ~grammar ~to_ast =
  match data with
  | Node node ->
    String.equal (to_ast node).name name && matches_instance node ~args ~grammar ~to_ast
  | _ -> false

let delay_generator f =
  Base_quickcheck.Generator.create (fun ~size ~random ->
    Base_quickcheck.Generator.generate (f ()) ~size ~random)

let rec generator_of_name name ~grammar ~of_ast =
  match Grammar.lookup_mono grammar ~name with
  | Some decl -> generator_of_decl decl ~name ~grammar ~of_ast
  | None -> assert false

and generator_of_decl decl ~name ~grammar ~of_ast =
  Base_quickcheck.Generator.map
    ~f:(fun data -> of_ast { data; name })
    (data_generator_of_decl decl ~grammar ~of_ast)

and data_generator_of_decl decl ~grammar ~of_ast =
  match (decl : Grammar.decl) with
  | Ty ty -> data_generator_of_ty ty ~grammar ~of_ast
  | Record record -> data_generator_of_record record ~grammar ~of_ast
  | Variant variant -> data_generator_of_variant variant ~grammar ~of_ast

and data_generator_of_variant variant ~grammar ~of_ast =
  Base_quickcheck.Generator.weighted_union
    (List.map variant ~f:(fun (tag, clause) ->
       let weight = 1. /. Float.of_int (1 + Grammar.count_recursions_in_clause clause) in
       let gen =
         delay_generator (fun () ->
           data_generator_of_clause clause ~tag ~grammar ~of_ast)
       in
       weight, gen))

and data_generator_of_clause clause ~tag ~grammar ~of_ast =
  Base_quickcheck.Generator.map
    ~f:(fun args -> (Variant { tag; args }))
    (array_generator_of_clause clause ~grammar ~of_ast)

and data_generator_of_record record ~grammar ~of_ast =
  Base_quickcheck.Generator.map
    ~f:(fun fields -> (Record fields))
    (array_generator_of_record record ~grammar ~of_ast)

and array_generator_of_clause clause ~grammar ~of_ast =
  match clause with
  | Empty -> Base_quickcheck.Generator.return [||]
  | Tuple tuple -> array_generator_of_tuple tuple ~grammar ~of_ast
  | Record record -> array_generator_of_record record ~grammar ~of_ast

and array_generator_of_record record ~grammar ~of_ast =
  Base_quickcheck.Generator.map ~f:Array.of_list
    (Base_quickcheck.Generator.all
       (List.map record ~f:(fun (_, ty) ->
          data_generator_of_ty ty ~grammar ~of_ast)))

and array_generator_of_tuple tuple ~grammar ~of_ast =
  Base_quickcheck.Generator.map ~f:Array.of_list
    (Base_quickcheck.Generator.all
       (List.map tuple ~f:(fun ty ->
          data_generator_of_ty ty ~grammar ~of_ast)))

and data_generator_of_ty ty ~grammar ~of_ast =
  match (ty : Grammar.ty) with
  | Var _ -> (* should be monomorphic *) assert false
  | Name name -> data_generator_of_name name ~grammar ~of_ast
  | Bool -> data_generator_for_bool ()
  | Char -> data_generator_for_char ()
  | Int -> data_generator_for_int ()
  | String -> data_generator_for_string ()
  | Location -> data_generator_for_location ()
  | Loc ty -> data_generator_for_loc ty ~grammar ~of_ast
  | List ty -> data_generator_for_list ty ~grammar ~of_ast
  | Option ty -> data_generator_for_option ty ~grammar ~of_ast
  | Tuple tuple -> data_generator_of_tuple tuple ~grammar ~of_ast
  | Instance (name, args) -> data_generator_of_instance name ~args ~grammar ~of_ast

and data_generator_of_name name ~grammar ~of_ast =
  Base_quickcheck.Generator.map
    ~f:(fun node -> Node node)
    (generator_of_name name ~grammar ~of_ast)

and data_generator_for_bool () =
  Base_quickcheck.Generator.map
    Base_quickcheck.Generator.bool
    ~f:(fun bool -> (Bool bool))

and data_generator_for_char () =
  Base_quickcheck.Generator.map
    Base_quickcheck.Generator.char
    ~f:(fun char -> (Char char))

and data_generator_for_int () =
  Base_quickcheck.Generator.map
    Base_quickcheck.Generator.int
    ~f:(fun int -> (Int int))

and data_generator_for_string () =
  Base_quickcheck.Generator.map
    Base_quickcheck.Generator.string
    ~f:(fun string -> (String string))

and data_generator_for_location () =
  Base_quickcheck.Generator.return (Location Astlib.Location.none)

and data_generator_for_loc ty ~grammar ~of_ast =
  Base_quickcheck.Generator.map
    ~f:(fun txt -> Loc { txt; loc = Astlib.Location.none })
    (data_generator_of_ty ty ~grammar ~of_ast)

and data_generator_for_list ty ~grammar ~of_ast =
  Base_quickcheck.Generator.map
    ~f:(fun list -> List list)
    (Base_quickcheck.Generator.list (data_generator_of_ty ty ~grammar ~of_ast))

and data_generator_for_option ty ~grammar ~of_ast =
  Base_quickcheck.Generator.map
    ~f:(fun option -> Option option)
    (Base_quickcheck.Generator.option (data_generator_of_ty ty ~grammar ~of_ast))

and data_generator_of_tuple tuple ~grammar ~of_ast =
  Base_quickcheck.Generator.map
    ~f:(fun tuple -> Tuple tuple)
    (array_generator_of_tuple tuple ~grammar ~of_ast)

and data_generator_of_instance name ~args ~grammar ~of_ast =
  match Grammar.lookup_instance grammar ~name ~args with
  | Some decl -> data_generator_of_decl decl ~grammar ~of_ast
  | None -> assert false

let generator grammar ~of_ast =
  Base_quickcheck.Generator.union
    (List.filter_map grammar ~f:(fun (name, kind) ->
       match (kind : Grammar.kind) with
       | Poly _ -> None
       | Mono decl ->
         Some (delay_generator (fun () ->
           generator_of_decl decl ~name ~grammar ~of_ast))))
