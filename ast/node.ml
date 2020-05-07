type t = { version : Astlib.Version.t; ast : t Astlib.Ast.t }

let version t = t.version

let wrap ~version ast = { version; ast }

let rec unwrap ~version:dst_version { ast; version = src_version } =
  Astlib.History.convert Astlib.history ast ~src_version ~dst_version ~unwrap ~wrap
