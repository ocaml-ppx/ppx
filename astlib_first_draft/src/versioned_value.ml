open StdLabels

type t = Versioned_ast.t Astlib_first_draft_ast.Ast.value

let of_ast x : t = Tree x

let to_ast t =
  match (t : t) with
  | Tree x -> Some x
  | _ -> None

let of_list list ~f = Astlib_first_draft_ast.Ast.List (List.map list ~f)

let to_list t ~f =
  match (t : t) with
  | List list -> List.map list ~f |> Optional.all
  | _ -> None

let of_option option ~f = Astlib_first_draft_ast.Ast.Option (Optional.map option ~f)

let to_option t ~f =
  match (t : t) with
  | Option option ->
    (match option with
     | None -> Some None
     | Some x ->
       (match f x with
        | Some y -> Some (Some y)
        | None -> None))
  | _ -> None

let of_location x = Astlib_first_draft_ast.Ast.Location x

let to_location t =
  match (t : t) with
  | Location x -> Some x
  | _ -> None

let of_string x = Astlib_first_draft_ast.Ast.String x

let to_string t =
  match (t : t) with
  | String x -> Some x
  | _ -> None

let of_char x = Astlib_first_draft_ast.Ast.Char x

let to_char t =
  match (t : t) with
  | Char x -> Some x
  | _ -> None

let of_bool x = Astlib_first_draft_ast.Ast.Bool x

let to_bool t =
  match (t : t) with
  | Bool x -> Some x
  | _ -> None
