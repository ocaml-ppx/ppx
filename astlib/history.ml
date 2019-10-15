open StdLabels

type 'a conversion_function
  =  'a Ast.node
  -> to_node:('a -> version:string -> 'a Ast.node)
  -> of_node:('a Ast.node -> version:string -> 'a)
  -> 'a Ast.node

type conversion =
  { src_version : string
  ; dst_version : string
  ; f : 'a . 'a conversion_function
  }

type t =
  { index_table : (string, int) Hashtbl.t
  ; versions : string array
  ; grammars : Grammar.t array
  ; of_nexts : conversion array
  ; to_nexts : conversion array
  }

let lookup_version_index index_table ~version =
  match Hashtbl.find_opt index_table version with
  | Some index -> index
  | None -> failwith (Printf.sprintf "unknown AST version: %s" version)

let version_index t ~version =
  lookup_version_index t.index_table ~version

let find_grammar t ~version =
  t.grammars.(version_index t ~version)

let create ~versioned_grammars ~conversions =
  let len = List.length versioned_grammars in
  let index_table = Hashtbl.create len in
  List.iteri versioned_grammars ~f:(fun index (version, _) ->
    Hashtbl.add index_table version index);
  let versions = Array.of_list (List.map versioned_grammars ~f:fst) in
  let grammars = Array.of_list (List.map versioned_grammars ~f:snd) in
  let of_nexts = Array.make (len - 1) None in
  let to_nexts = Array.make (len - 1) None in
  List.iter conversions ~f:(fun (conversion : conversion) ->
    let src_index = lookup_version_index index_table ~version:conversion.src_version in
    let dst_index = lookup_version_index index_table ~version:conversion.dst_version in
    if src_index + 1 = dst_index
    then
      (match to_nexts.(src_index) with
       | None -> to_nexts.(src_index) <- Some conversion
       | Some _ ->
         failwith
           (Printf.sprintf "multiple conversions from %s to %s"
              conversion.src_version
              conversion.dst_version))
    else if dst_index + 1 = src_index
    then
      (match of_nexts.(dst_index) with
       | None -> of_nexts.(dst_index) <- Some conversion
       | Some _ ->
         failwith
           (Printf.sprintf "multiple conversions from %s to %s"
              conversion.src_version
              conversion.dst_version))
    else
      failwith
        (Printf.sprintf "conversion between non-adjacent versions %s and %s"
           conversion.src_version
           conversion.dst_version));
  let of_nexts =
    Array.mapi of_nexts ~f:(fun dst_index option ->
      match option with
      | Some conversion -> conversion
      | None ->
        failwith
          (Printf.sprintf "no conversion from %s to %s"
             versions.(dst_index + 1)
             versions.(dst_index)))
  in
  let to_nexts =
    Array.mapi to_nexts ~f:(fun src_index option ->
      match option with
      | Some conversion -> conversion
      | None ->
        failwith
          (Printf.sprintf "no conversion from %s to %s"
             versions.(src_index)
             versions.(src_index + 1)))
  in
  { index_table
  ; versions
  ; grammars
  ; of_nexts
  ; to_nexts
  }

let versioned_grammars t =
  Array.to_list (Array.map2 t.versions t.grammars ~f:(fun v g -> v, g))

let convert t node ~src_version ~dst_version ~to_node ~of_node =
  let src_index = version_index t ~version:src_version in
  let dst_index = version_index t ~version:dst_version in
  if src_index < dst_index
  then (
    let node = ref node in
    for index = src_index to dst_index - 1 do
      node := t.to_nexts.(index).f !node ~to_node ~of_node
    done;
    !node)
  else if src_index > dst_index
  then (
    let node = ref node in
    for index = src_index - 1 downto dst_index do
      node := t.of_nexts.(index).f !node ~to_node ~of_node
    done;
    !node)
  else
    node
