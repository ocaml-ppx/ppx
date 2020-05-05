type t = { version : Astlib.Version.t; ast : t Astlib.Ast.t }

let version t = t.version

let wrap ~version ast = { version; ast }

let rec convert ~version:dst_version { ast; version = src_version } =
  let version, ast =
    Astlib.History.convert Astlib.history ast ~src_version ~dst_version ~unwrap ~wrap
  in
  { version; ast }

and unwrap ~version t =
  let t = convert t ~version in
  if Astlib.Version.equal t.version version
  then Some t.ast
  else None
