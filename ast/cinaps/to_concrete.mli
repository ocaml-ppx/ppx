(** [
    let conversion_failed node_name =
      let msg = Printf.sprintf "Ppx_ast: Could not convert %s to <version>" node_name in
      failwith msg
    ] *)
val define_conversion_failed : version: string -> unit

(** [print_to_concrete_exn ~var_name ~targs ~node_name expr] prints the code
    that converts [expr] from the abstract to the concrete type for [node_name]
    and binds it to [var_name] or raises if that conversion is impossible.
    I.e. it prints something like: [let <var_name> = <conversion_code> in].

    [var_name] defaults to ["concrete"]

    It relies on a [conversion_failed : string -> 'a] in the scope.
*)
val print_to_concrete_exn :
  ?var_name: string ->
  node_name: string ->
  string ->
  unit
