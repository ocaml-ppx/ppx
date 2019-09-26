(* distribution information *)

type file = {
  path           : string;
  ignored_types  : string list;
  desc_shortcuts : bool;
}

val files : file list
