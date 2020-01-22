let conversion_failed ~version node_name =
  let msg =
    Printf.sprintf "Ppx_ast: Could not convert %s to %s" version node_name
  in
  failwith msg
