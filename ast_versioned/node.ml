type t = { version : string; node : t Astlib.Ast.node }

let version t = t.version

let of_node node ~version = { version; node }

let rec to_node { node; version = src_version } ~version:dst_version =
  Astlib.History.convert Astlib.history node ~src_version ~dst_version ~to_node ~of_node
