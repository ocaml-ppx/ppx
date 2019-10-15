open StdLabels

module Helpers = struct
  module Option = struct
    let map option ~f =
      match option with
      | None -> None
      | Some x -> Some (f x)

    let bind option ~f =
      match option with
      | None -> None
      | Some x -> f x

    let all list =
      let rec loop list acc =
        match list with
        | [] -> Some (List.rev acc)
        | None :: _ -> None
        | Some x :: tail -> loop tail (x :: acc)
      in
      loop list []
  end

  module Loc = struct
    type 'a t = 'a Location.loc

    let map ({ loc; txt } : _ t) ~f = ({ loc; txt = f txt } : _ t)
  end

  module Optional = struct
    module Loc = struct
      let map (loc : _ Loc.t) ~f =
        Option.map (f loc.txt) ~f:(fun txt -> { loc with txt })
    end

    module List = struct
      let map list ~f = Option.all (List.map list ~f)
    end

    module Option = struct
      let map option ~f =
        match option with
        | None -> Some None
        | Some x ->
          match f x with
          | None -> None
          | Some y -> Some (Some y)
    end
  end
end

type t = Node.t Astlib.Ast.data

let of_node x : t = Node x
let of_bool x : t = Bool x
let of_char x : t = Char x
let of_int x : t = Int x
let of_string x : t = String x
let of_location x : t = Location x
let of_loc x ~f : t = Loc (Helpers.Loc.map x ~f)
let of_list x ~f : t = List (List.map x ~f)
let of_option x ~f : t = Option (Helpers.Option.map x ~f)
let of_tuple2 (x1, x2) ~f1 ~f2 : t = Tuple [|f1 x1; f2 x2|]
let of_tuple3 (x1, x2, x3) ~f1 ~f2 ~f3 : t = Tuple [|f1 x1; f2 x2; f3 x3|]
let of_tuple4 (x1, x2, x3, x4) ~f1 ~f2 ~f3 ~f4 : t = Tuple [|f1 x1; f2 x2; f3 x3; f4 x4|]

let to_node : t -> _ = function Node x -> Some x | _ -> None
let to_bool : t -> _ = function Bool x -> Some x | _ -> None
let to_char : t -> _ = function Char x -> Some x | _ -> None
let to_int : t -> _ = function Int x -> Some x | _ -> None
let to_string : t -> _ = function String x -> Some x | _ -> None
let to_location : t -> _ = function Location x -> Some x | _ -> None

let to_loc t ~f =
  match (t : t) with
  | Loc loc -> Helpers.Optional.Loc.map loc ~f
  | _ -> None

let to_list t ~f =
  match (t : t) with
  | List x -> Helpers.Optional.List.map x ~f
  | _ -> None

let to_option t ~f =
  match (t : t) with
  | Option x -> Helpers.Optional.Option.map x ~f
  | _ -> None

let to_tuple2 t ~f1 ~f2 =
  match (t : t) with
  | Tuple [| x1; x2 |] ->
    Helpers.Option.bind (f1 x1) ~f:(fun x1 ->
      Helpers.Option.bind (f2 x2) ~f:(fun x2 ->
        Some (x1, x2)))
  | _ -> None

let to_tuple3 t ~f1 ~f2 ~f3 =
  match (t : t) with
  | Tuple [| x1; x2; x3 |] ->
    Helpers.Option.bind (f1 x1) ~f:(fun x1 ->
      Helpers.Option.bind (f2 x2) ~f:(fun x2 ->
        Helpers.Option.bind (f3 x3) ~f:(fun x3 ->
          Some (x1, x2, x3))))
  | _ -> None

let to_tuple4 t ~f1 ~f2 ~f3 ~f4 =
  match (t : t) with
  | Tuple [| x1; x2; x3; x4 |] ->
    Helpers.Option.bind (f1 x1) ~f:(fun x1 ->
      Helpers.Option.bind (f2 x2) ~f:(fun x2 ->
        Helpers.Option.bind (f3 x3) ~f:(fun x3 ->
          Helpers.Option.bind (f4 x4) ~f:(fun x4 ->
            Some (x1, x2, x3, x4)))))
  | _ -> None
