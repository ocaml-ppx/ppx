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

let rec map { name; data } ~f = { name; data = map_data data ~f }

and map_data data ~f =
  match data with
  | Node node -> Node (f node)
  | Bool _ | Char _ | Int _ | String _ | Location _ as data -> data
  | Loc loc -> Loc (Astlib.Loc.map loc ~f:(map_data ~f))
  | List list -> List (List.map list ~f:(map_data ~f))
  | Option option -> Option (Option.map option ~f:(map_data ~f))
  | Tuple array -> Tuple (Array.map array ~f:(map_data ~f))
  | Record array -> Record (Array.map array ~f:(map_data ~f))
  | Variant { tag; args } -> Variant { tag; args = Array.map args ~f:(map_data ~f) }

let rec matches ast ~grammar ~unwrap =
  match Grammar.lookup_mono grammar ~name:ast.name with
  | Some decl -> data_matches_decl ast.data ~decl ~grammar ~unwrap
  | None -> false

and matches_instance node ~args ~grammar ~unwrap =
  let ast : _ t = unwrap node in
  match Grammar.lookup_instance grammar ~name:ast.name ~args with
  | Some decl -> data_matches_decl ast.data ~decl ~grammar ~unwrap
  | None -> false

and data_matches_decl data ~decl ~grammar ~unwrap =
  match decl with
  | Ty ty -> data_matches_ty data ~ty ~grammar ~unwrap
  | Record record -> data_matches_record data ~record ~grammar ~unwrap
  | Variant variant -> data_matches_variant data ~variant ~grammar ~unwrap

and data_matches_variant data ~variant ~grammar ~unwrap =
  match data with
  | Variant { tag; args } ->
    (match List.Assoc.find variant tag ~equal:String.equal with
     | None -> false
     | Some clause -> array_matches_clause args ~clause ~grammar ~unwrap)
  | _ -> false

and data_matches_record data ~record ~grammar ~unwrap =
  match data with
  | Record fields -> array_matches_record fields ~record ~grammar ~unwrap
  | _ -> false

and array_matches_clause array ~clause ~grammar ~unwrap =
  match clause with
  | Empty -> Array.is_empty array
  | Tuple tuple -> array_matches_tuple array ~tuple ~grammar ~unwrap
  | Record record -> array_matches_record array ~record ~grammar ~unwrap

and array_matches_record array ~record ~grammar ~unwrap =
  Array.length array = List.length record
  && List.for_alli record ~f:(fun i (_, ty) ->
    data_matches_ty array.(i) ~ty ~grammar ~unwrap)

and array_matches_tuple array ~tuple ~grammar ~unwrap =
  Array.length array = List.length tuple
  && List.for_alli tuple ~f:(fun i ty ->
    data_matches_ty array.(i) ~ty ~grammar ~unwrap)

and data_matches_ty data ~ty ~grammar ~unwrap =
  match ty with
  | Var _ -> (* type should be monomorphic *) assert false
  | Name name -> data_matches_name data ~name ~grammar ~unwrap
  | Bool -> data_matches_bool data
  | Char -> data_matches_char data
  | Int -> data_matches_int data
  | String -> data_matches_string data
  | Location -> data_matches_location data
  | Loc ty -> data_matches_loc data ~ty ~grammar ~unwrap
  | List ty -> data_matches_list data ~ty ~grammar ~unwrap
  | Option ty -> data_matches_option data ~ty ~grammar ~unwrap
  | Tuple tuple -> data_matches_tuple data ~tuple ~grammar ~unwrap
  | Instance (name, args) -> data_matches_instance data ~name ~args ~grammar ~unwrap

and data_matches_name data ~name ~grammar ~unwrap =
  match data with
  | Node node ->
    let ast : _ t = unwrap node in
    String.equal ast.name name && matches ast ~grammar ~unwrap
  | _ -> false

and data_matches_bool     = function Bool     _ -> true | _ -> false
and data_matches_char     = function Char     _ -> true | _ -> false
and data_matches_int      = function Int      _ -> true | _ -> false
and data_matches_string   = function String   _ -> true | _ -> false
and data_matches_location = function Location _ -> true | _ -> false

and data_matches_loc data ~ty ~grammar ~unwrap =
  match data with
  | Loc loc -> data_matches_ty loc.txt ~ty ~grammar ~unwrap
  | _ -> false

and data_matches_list data ~ty ~grammar ~unwrap =
  match data with
  | List list -> List.for_all list ~f:(data_matches_ty ~ty ~grammar ~unwrap)
  | _ -> false

and data_matches_option data ~ty ~grammar ~unwrap =
  match data with
  | Option option -> Option.for_all option ~f:(data_matches_ty ~ty ~grammar ~unwrap)
  | _ -> false

and data_matches_tuple data ~tuple ~grammar ~unwrap =
  match data with
  | Tuple values -> array_matches_tuple values ~tuple ~grammar ~unwrap
  | _ -> false

and data_matches_instance data ~name ~args ~grammar ~unwrap =
  match data with
  | Node node ->
    String.equal (unwrap node).name name && matches_instance node ~args ~grammar ~unwrap
  | _ -> false

let delay_generator f =
  Base_quickcheck.Generator.create (fun ~size ~random ->
    Base_quickcheck.Generator.generate (f ()) ~size ~random)

let rec generator_of_name name ~grammar ~wrap =
  match Grammar.lookup_mono grammar ~name with
  | Some decl -> generator_of_decl decl ~name ~grammar ~wrap
  | None -> assert false

and generator_of_decl decl ~name ~grammar ~wrap =
  Base_quickcheck.Generator.map
    ~f:(fun data -> { data; name })
    (data_generator_of_decl decl ~grammar ~wrap)

and data_generator_of_decl decl ~grammar ~wrap =
  match (decl : Grammar.decl) with
  | Ty ty -> data_generator_of_ty ty ~grammar ~wrap
  | Record record -> data_generator_of_record record ~grammar ~wrap
  | Variant variant -> data_generator_of_variant variant ~grammar ~wrap

and data_generator_of_variant variant ~grammar ~wrap =
  Base_quickcheck.Generator.weighted_union
    (List.map variant ~f:(fun (tag, clause) ->
       let weight = 1. /. Float.of_int (1 + Grammar.count_recursions_in_clause clause) in
       let gen =
         delay_generator (fun () ->
           data_generator_of_clause clause ~tag ~grammar ~wrap)
       in
       weight, gen))

and data_generator_of_clause clause ~tag ~grammar ~wrap =
  Base_quickcheck.Generator.map
    ~f:(fun args -> (Variant { tag; args }))
    (array_generator_of_clause clause ~grammar ~wrap)

and data_generator_of_record record ~grammar ~wrap =
  Base_quickcheck.Generator.map
    ~f:(fun fields -> (Record fields))
    (array_generator_of_record record ~grammar ~wrap)

and array_generator_of_clause clause ~grammar ~wrap =
  match clause with
  | Empty -> Base_quickcheck.Generator.return [||]
  | Tuple tuple -> array_generator_of_tuple tuple ~grammar ~wrap
  | Record record -> array_generator_of_record record ~grammar ~wrap

and array_generator_of_record record ~grammar ~wrap =
  Base_quickcheck.Generator.map ~f:Array.of_list
    (Base_quickcheck.Generator.all
       (List.map record ~f:(fun (_, ty) ->
          data_generator_of_ty ty ~grammar ~wrap)))

and array_generator_of_tuple tuple ~grammar ~wrap =
  Base_quickcheck.Generator.map ~f:Array.of_list
    (Base_quickcheck.Generator.all
       (List.map tuple ~f:(fun ty ->
          data_generator_of_ty ty ~grammar ~wrap)))

and data_generator_of_ty ty ~grammar ~wrap =
  match (ty : Grammar.ty) with
  | Var _ -> (* should be monomorphic *) assert false
  | Name name -> data_generator_of_name name ~grammar ~wrap
  | Bool -> data_generator_for_bool ()
  | Char -> data_generator_for_char ()
  | Int -> data_generator_for_int ()
  | String -> data_generator_for_string ()
  | Location -> data_generator_for_location ()
  | Loc ty -> data_generator_for_loc ty ~grammar ~wrap
  | List ty -> data_generator_for_list ty ~grammar ~wrap
  | Option ty -> data_generator_for_option ty ~grammar ~wrap
  | Tuple tuple -> data_generator_of_tuple tuple ~grammar ~wrap
  | Instance (name, args) -> data_generator_of_instance name ~args ~grammar ~wrap

and data_generator_of_name name ~grammar ~wrap =
  Base_quickcheck.Generator.map
    ~f:(fun ast -> Node (wrap ast))
    (generator_of_name name ~grammar ~wrap)

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

and data_generator_for_loc ty ~grammar ~wrap =
  Base_quickcheck.Generator.map
    ~f:(fun txt -> Loc { txt; loc = Astlib.Location.none })
    (data_generator_of_ty ty ~grammar ~wrap)

and data_generator_for_list ty ~grammar ~wrap =
  Base_quickcheck.Generator.map
    ~f:(fun list -> List list)
    (Base_quickcheck.Generator.list (data_generator_of_ty ty ~grammar ~wrap))

and data_generator_for_option ty ~grammar ~wrap =
  Base_quickcheck.Generator.map
    ~f:(fun option -> Option option)
    (Base_quickcheck.Generator.option (data_generator_of_ty ty ~grammar ~wrap))

and data_generator_of_tuple tuple ~grammar ~wrap =
  Base_quickcheck.Generator.map
    ~f:(fun tuple -> Tuple tuple)
    (array_generator_of_tuple tuple ~grammar ~wrap)

and data_generator_of_instance name ~args ~grammar ~wrap =
  match Grammar.lookup_instance grammar ~name ~args with
  | Some decl -> data_generator_of_decl decl ~grammar ~wrap
  | None -> assert false

let generator grammar ~wrap =
  Base_quickcheck.Generator.union
    (List.filter_map grammar ~f:(fun (name, kind) ->
       match (kind : Grammar.kind) with
       | Poly _ -> None
       | Mono decl ->
         Some (delay_generator (fun () ->
           generator_of_decl decl ~name ~grammar ~wrap))))
