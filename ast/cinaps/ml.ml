open Stdppx

let keywords =
  [ "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"
  ; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"
  ; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"
  ; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"
  ; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or"
  ; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"
  ; "val"; "virtual"; "when"; "while"; "with" ]

let map_keyword s =
  if List.mem_sorted ~compare:String.compare s keywords then
    s ^ "_"
  else
    s

let is_id_char = function
  | 'A' .. 'Z' -> true
  | 'a' .. 'z' -> true
  | '0' .. '9' -> true
  | '_' | '\'' -> true
  | _ -> false

let to_id_char char =
  if is_id_char char
  then char
  else '_'

let raw_id string = String.map string ~f:to_id_char

let id string = map_keyword (String.lowercase_ascii (raw_id string))
let tvar string = "'" ^ id string
let module_name string = String.capitalize_ascii (raw_id string)
let tag string = String.capitalize_ascii (raw_id string)

let list_lit elms =
  Printf.sprintf "[%s]" (String.concat ~sep:"; " elms)

let tuple elms =
  Printf.sprintf "(%s)" (String.concat ~sep:", " elms)

let arrow_type types = String.concat ~sep:" -> " types

let tuple_type types = String.concat ~sep:" * " types

let dotted_list ~id list =
  let len = List.length list in
  List.mapi list ~f:(fun i string ->
    if i = len - 1
    then id string
    else module_name string)

let dotted ~id string =
  String.concat ~sep:"."
    (dotted_list ~id
       (String.split_on_char ~sep:'.' string))

let print_modules ?(recursive = false) alist ~signature ~f =
  List.iteri alist ~f:(fun i (name, x) ->
    if i > 0 then Print.newline ();
    Print.println "%s %s %s"
      (if recursive
       then if i = 0
         then "module rec"
         else "and"
       else "module")
      (module_name name)
      (if signature then ": sig" else "= struct");
    Print.indented (fun () -> f name x);
    Print.println "end")

let declare_modules = print_modules ~signature:true
let define_modules = print_modules ~signature:false

let declare_module name print_body =
  declare_modules [(name, print_body)] ~f:(fun _ f -> f ())

let define_module name print_body =
  define_modules [(name, print_body)] ~f:(fun _ f -> f ())

let poly_inst name ~args =
  let prefix =
    match args with
    | [] -> ""
    | [arg] -> Printf.sprintf "%s " arg
    | _ -> Printf.sprintf "(%s) " (String.concat ~sep:", " args)
  in
  let suffix = dotted ~id name in
  prefix ^ suffix

let poly_type name ~tvars =
  poly_inst name ~args:(List.map tvars ~f:tvar)

type element =
  | Empty
  | Line of string
  | Block of (unit -> unit)

let print_with_element ~between ~element fmt =
  let f string =
    match (element : element) with
    | Empty -> Print.println "%s" string
    | Line elt -> Print.println "%s %s %s" string between elt
    | Block f ->
      Print.println "%s %s" string between;
      Print.indented f
  in
  Printf.ksprintf f fmt

let declare_type ?(tvars = []) name element =
  print_with_element "type %s" (poly_type name ~tvars) ~between:"=" ~element

let declare_val name element =
  print_with_element "val %s" name ~between:":" ~element

let print_record_type alist ~f =
  List.iteri alist ~f:(fun i (name, x) ->
    Print.println "%c %s : %s"
      (if i = 0 then '{' else ';')
      (id name)
      (f x));
  Print.println "}"

let print_array list ~f =
  match list with
  | [] -> Print.println "[||]"
  | _ :: _ ->
    List.iteri list ~f:(fun i x ->
      Print.println "%s %s" (if i = 0 then "[|" else " ;") (f i x));
    Print.println "|]"

let print_variant_type alist ~f =
  List.iter alist ~f:(fun (name, x) ->
    print_with_element "| %s" (tag name) ~between:"of" ~element:(f x))

let print_arrow list ~f return =
  List.iteri list ~f:(fun i elt ->
    Print.println "%s%s" (if i = 0 then "" else "-> ") (f elt));
  Print.println "-> %s" return

let print_labelled_arrow alist ~f return =
  print_arrow alist ~f:(fun (label, x) -> id label ^ ":" ^ f x) return

let class_params = function
  | [] -> ""
  | params ->
    let str_params = String.concat ~sep:", " params in
    Printf.sprintf "[%s] " str_params

let virtual_qualifier virtual_ =
  if virtual_ then "virtual " else ""

let define_class ?(virtual_=false) ?(params=[]) name f =
  Print.println "class %s%s%s =" (virtual_qualifier virtual_) (class_params params) name;
  Print.indented f

let declare_class ?(virtual_=false) ?(params=[]) name f =
  Print.println "class %s%s%s :" (virtual_qualifier virtual_) (class_params params) name;
  Print.indented f

let define_object ?(bind_self=false) f =
  let self = if bind_self then " (self)" else "" in
  Print.println "object%s" self;
  Print.indented f;
  Print.println "end"

let declare_object f =
  define_object ~bind_self:false f

let define_method ?signature name f =
  let signature =
    match signature with
    | None -> ""
    | Some s -> Printf.sprintf " : %s " s
  in
  Print.println "method %s%s =" name signature;
  Print.indented f

let declare_method ?(virtual_=false) ~name ~signature () =
  let qualifier = if virtual_ then "virtual " else "" in
  Print.println "method %s%s : %s" qualifier name signature

let define_anon_fun ~args f =
  let args_str = String.concat ~sep:" " args in
  Print.println "fun %s ->" args_str;
  Print.indented f
