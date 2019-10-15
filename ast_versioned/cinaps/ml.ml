open StdLabels

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

let id string = String.lowercase_ascii (raw_id string)
let tvar string = "'" ^ id string
let module_name string = String.capitalize_ascii (raw_id string)
let tag string = String.capitalize_ascii (raw_id string)

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
