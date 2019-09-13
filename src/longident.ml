open! Import
open Astlib.V4_07.Longident

let to_astlib = Astlib.Conversions.longident_to_ast

let to_concrete_exn t =
  match to_concrete t with
  | Some t -> t
  | None -> failwith "Malformed Longident"

module Builder = struct
  let lident a = of_concrete (Lident {a})
  let ldot a b = of_concrete (Ldot {a; b})
end

module T = struct
  type t = longident =
      Lident of string
    | Ldot of t * string
    | Lapply of t * t

  let compare = compare

  let is_normal_ident_char = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false

  let is_normal_ident = function
    | "asr" | "land" | "lor" | "lsl" | "lsr" | "lxor" | "mod" | "or" ->
      false
    | string ->
      String.for_all string ~f:is_normal_ident_char

  let short_name string =
    if is_normal_ident string
    then string
    else "( " ^ string ^ " )"

  let rec name lident =
    match to_concrete_exn lident with
    | Lident {a} -> short_name a
    | Ldot {a; b} -> name a ^ "." ^ short_name b
    | Lapply {a; b} -> Printf.sprintf "%s(%s)" (name a) (name b)

  let name lident =
    let lident = to_astlib lident in
    name lident
end
include T

let rec flat accu lident =
  match to_concrete_exn lident with
  | Lident {a} -> a :: accu
  | Ldot {a = lid; b} -> flat (b :: accu) lid
  | Lapply _ -> invalid_arg "Ppx.Longident.flatten"

let flatten_exn lident =
  let lident = to_astlib lident in
  flat [] lident

let last_exn lident =
  let lident = to_astlib lident in
  match to_concrete_exn lident with
  | Lident {a} -> a
  | Ldot {b; _} -> b
  | Lapply _ -> invalid_arg "Ppx.Longident.last"

let unflatten ~init l =
  List.fold_left l ~init ~f:(fun acc b -> (Builder.ldot acc b))

(* for cases without dotted operators (e.g. [parse "A.B.C"]) *)
let parse_simple s =
  match String.split s ~on:'.' with
  | [] -> assert false
  | a :: l -> unflatten ~init:(Builder.lident a) l

(* handle ["A.B.(+.+)"] or ["Vec.(.%.()<-)"] *)
let parse s =
  let open Builder in
  let invalid () =
    invalid_arg (Printf.sprintf "Ppx.Longident.parse: %S" s)
  in
  match String.index s '(', String.rindex_opt s ')' with
  | None, None -> parse_simple  s
  | None, _ | _, None -> invalid ()
  | Some l, Some r ->
      if r <> String.length s - 1 then invalid ();
      let group = if r = l + 1 then "()" else
          String.trim (String.sub s ~pos:(l+1) ~len:(r-l-1))
      in
      if l = 0 then lident group
      else if s.[l - 1] <> '.' then invalid ()
      else
        let before = String.sub s ~pos:0 ~len:(l-1) in
        match String.split before ~on:'.' with
        | [] -> assert false
        | s :: l -> ldot (unflatten ~init:(lident s) l) group

let parse s =
  match Astlib.Conversions.longident_of_ast (parse s) with
  | Some lident -> lident
  | None -> failwith "Built a malformed Longident"
