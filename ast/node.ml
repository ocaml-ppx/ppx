type t = { version : Astlib.Version.t; node : t Astlib.Ast.node }

let version t = t.version

let of_node ~version node = { version; node }

let rec to_node ~version:dst_version { node; version = src_version } =
  Astlib.History.convert Astlib.history node ~src_version ~dst_version ~to_node ~of_node
