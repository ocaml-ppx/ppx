open StdLabels

type 'node conversion_function
  = 'node Ast.t
  -> unwrap:('node -> 'node Ast.t option)
  -> wrap:('node Ast.t -> 'node)
  -> 'node Ast.t option

type conversion =
  { src_version : Version.t
  ; dst_version : Version.t
  ; f : 'a . 'a conversion_function
  }

type t =
  { index_table : (Version.t, int) Hashtbl.t
  ; versions : Version.t array
  ; grammars : Grammar.t array
  ; of_nexts : conversion array
  ; to_nexts : conversion array
  }

let lookup_version_index index_table ~version =
  match Hashtbl.find_opt index_table version with
  | Some index -> index
  | None ->
    failwith (Printf.sprintf "unknown AST version: %s" (Version.to_string version))

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
              (Version.to_string conversion.src_version)
              (Version.to_string conversion.dst_version)))
    else if dst_index + 1 = src_index
    then
      (match of_nexts.(dst_index) with
       | None -> of_nexts.(dst_index) <- Some conversion
       | Some _ ->
         failwith
           (Printf.sprintf "multiple conversions from %s to %s"
              (Version.to_string conversion.src_version)
              (Version.to_string conversion.dst_version)))
    else
      failwith
        (Printf.sprintf "conversion between non-adjacent versions %s and %s"
           (Version.to_string conversion.src_version)
           (Version.to_string conversion.dst_version)));
  let of_nexts =
    Array.mapi of_nexts ~f:(fun dst_index option ->
      match option with
      | Some conversion -> conversion
      | None ->
        failwith
          (Printf.sprintf "no conversion from %s to %s"
             (Version.to_string versions.(dst_index + 1))
             (Version.to_string versions.(dst_index))))
  in
  let to_nexts =
    Array.mapi to_nexts ~f:(fun src_index option ->
      match option with
      | Some conversion -> conversion
      | None ->
        failwith
          (Printf.sprintf "no conversion from %s to %s"
             (Version.to_string versions.(src_index))
             (Version.to_string versions.(src_index + 1))))
  in
  { index_table
  ; versions
  ; grammars
  ; of_nexts
  ; to_nexts
  }

let versioned_grammars t =
  Array.to_list (Array.map2 t.versions t.grammars ~f:(fun v g -> v, g))

let apply_conversion conversion ast ~unwrap ~wrap =
  conversion.f ast
    ~unwrap:(unwrap ~version:conversion.src_version)
    ~wrap:(wrap ~version:conversion.dst_version)

let rec convert_loop ast ~unwrap ~wrap ~array ~index ~final ~delta =
  let conversion = array.(index) in
  match apply_conversion conversion ast ~unwrap ~wrap with
  | None -> conversion.src_version, ast
  | Some ast ->
    if index = final
    then conversion.dst_version, ast
    else convert_loop ast ~unwrap ~wrap ~array ~index:(index + delta) ~final ~delta

let convert t ast ~src_version ~dst_version ~unwrap ~wrap =
  let src_index = version_index t ~version:src_version in
  let dst_index = version_index t ~version:dst_version in
  if src_index < dst_index
  then
    convert_loop ast ~unwrap ~wrap
      ~array:t.to_nexts
      ~index:src_index
      ~final:(dst_index - 1)
      ~delta:1
  else if src_index > dst_index
  then
    convert_loop ast ~unwrap ~wrap
      ~array:t.of_nexts
      ~index:(src_index - 1)
      ~final:dst_index
      ~delta:(-1)
  else
    src_version, ast
