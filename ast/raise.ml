let conversion_failed ~version node_name =
  let msg =
    Printf.sprintf "Ppx_ast: Could not convert %s to %s" node_name version
  in
  failwith msg
