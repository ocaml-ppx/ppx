open StdLabels

type 'a t = 'a option

let map t ~f =
  match t with
  | None -> None
  | Some x -> Some (f x)

let bind t ~f =
  match t with
  | None -> None
  | Some x -> f x

let all =
  let rec loop list ~acc =
    match list with
    | [] -> Some (List.rev acc)
    | None :: _ -> None
    | Some hd :: tl -> loop tl ~acc:(hd :: acc)
  in
  fun list -> loop list ~acc:[]

module List = struct
  let map t ~f = List.map t ~f |> all
end

module Option = struct
  let map t ~f =
    match t with
    | None -> Some None
    | Some x ->
      match f x with
      | None -> None
      | Some y -> Some (Some y)
end
