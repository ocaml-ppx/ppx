(** [
    let conversion_failed node_name =
      let msg = Printf.sprintf "Ppx_ast: Could not convert %s to <version>" node_name in
      failwith msg
    ] *)
val define_conversion_failed : version: string -> unit

(** [
    let concrete =
      match X.to_concrete expr with
      | None -> conversion_failed <node_name>
      | Some n -> n
    in
    ] *)
val print_to_concrete_exn :
  targs: Astlib.Grammar.ty list ->
  node_name: string ->
  string ->
  unit
