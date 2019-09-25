type file = {
  path           : string;
  ignored_types  : string list;
  desc_shortcuts : bool;
}

let files = [
  { path = "stdlib/lexing.mli";
    ignored_types = ["lexbuf"; "lex_tables"];
    desc_shortcuts = false; };
  { path = "parsing/asttypes.mli";
    ignored_types = ["constant"; "label"; "loc"];
    desc_shortcuts = false; };
  { path = "parsing/longident.mli";
    ignored_types = [];
    desc_shortcuts = false; };
  { path = "parsing/location.mli";
    ignored_types = ["error"];
    desc_shortcuts = false; };
  { path = "parsing/parsetree.mli";
    ignored_types = [];
    desc_shortcuts = true; };
]
