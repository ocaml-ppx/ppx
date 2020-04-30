type t = { version : Astlib.Version.t; ast : t Astlib.Ast.t }

let version t = t.version

let of_ast ast ~version = { version; ast }

let rec to_ast { ast; version = src_version } ~version:dst_version =
  Astlib.History.convert Astlib.history ast ~src_version ~dst_version ~to_ast ~of_ast
