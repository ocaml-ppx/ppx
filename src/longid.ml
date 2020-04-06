open! Import

module Concrete = struct
  type t =
    | Lident of string
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

  let rec name = function
    | Lident s -> short_name s
    | Ldot (a, b) -> name a ^ "." ^ short_name b
    | Lapply (a, b) -> Printf.sprintf "%s(%s)" (name a) (name b)

  let rec flat accu = function
      Lident s -> s :: accu
    | Ldot(lid, s) -> flat (s :: accu) lid
    | Lapply(_, _) -> invalid_arg "Ppx.Longident.flatten"

  let flatten_exn lid = flat [] lid

  let last_exn = function
      Lident s -> s
    | Ldot(_, s) -> s
    | Lapply(_, _) -> invalid_arg "Ppx.Longident.flatten"

  let unflatten ~init l =
    List.fold_left l ~init ~f:(fun acc s -> Ldot (acc, s))

  (* for cases without dotted operators (e.g. [parse "A.B.C"]) *)
  let parse_simple s =
    match String.split s ~on:'.' with
    | [] -> assert false
    | s :: l -> unflatten ~init:(Lident s) l

  (* handle ["A.B.(+.+)"] or ["Vec.(.%.()<-)"] *)
  let parse s =
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
      if l = 0 then Lident group
      else if s.[l - 1] <> '.' then invalid ()
      else
        let before = String.sub s ~pos:0 ~len:(l-1) in
        match String.split before ~on:'.' with
        | [] -> assert false
        | s :: l -> Ldot(unflatten ~init:(Lident s) l, group)
end

type t = longident

let lident = Longident.lident
let ldot = Longident.ldot
let lapply = Longident.lapply

let rec of_concrete concrete =
  match (concrete : Concrete.t) with
  | Lident name -> Longident.lident name
  | Ldot (a, b) -> Longident.ldot (of_concrete a) b
  | Lapply (a, b) -> Longident.lapply (of_concrete a) (of_concrete b)

let rec to_concrete_opt t =
  Option.bind (Longident.to_concrete_opt t) ~f:(function
    | Lident name -> Some (Concrete.Lident name)
    | Ldot (a, b) ->
      Option.map (to_concrete_opt a) ~f:(fun a ->
        Concrete.Ldot (a, b))
    | Lapply (a, b) ->
      Option.bind (to_concrete_opt a) ~f:(fun a ->
        Option.map (to_concrete_opt b) ~f:(fun b ->
          Concrete.Lapply (a, b))))

let to_concrete_result t =
  match to_concrete_opt t with
  | Some concrete -> Ok concrete
  | None -> Error t

let to_concrete t = Option.value_exn (to_concrete_opt t)

(* Compare without throwing away information if conversion fails. *)
let compare a b =
  Result.compare Concrete.compare compare
    (to_concrete_result a)
    (to_concrete_result b)

let flatten_exn t = Concrete.flatten_exn (to_concrete t)
let last_exn t = Concrete.last_exn (to_concrete t)
let name t = Concrete.name (to_concrete t)
let parse string = of_concrete (Concrete.parse string)
