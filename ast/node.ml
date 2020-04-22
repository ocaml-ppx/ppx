type t = { version : Astlib.Version.t; ast : t Astlib.Ast.t }

let version t = t.version

let of_ast ~version ast = { version; ast }

let rec to_ast ~version:dst_version { ast; version = src_version } =
  Astlib.History.convert Astlib.history ast ~src_version ~dst_version ~to_ast ~of_ast
