open Base

type t = Versioned_ast.t Astlib_ast.Ast.value

let of_ast x : t = Tree x

let to_ast t =
  match (t : t) with
  | Tree x -> Some x
  | _ -> None

let of_list list ~f = Astlib_ast.Ast.List (List.map list ~f)

let to_list t ~f =
  match (t : t) with
  | List list -> List.map list ~f |> Option.all
  | _ -> None

let of_option option ~f = Astlib_ast.Ast.Option (Option.map option ~f)

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

let of_string x = Astlib_ast.Ast.String x

let to_string t =
  match (t : t) with
  | String x -> Some x
  | _ -> None
