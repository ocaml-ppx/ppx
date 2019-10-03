open StdLabels

module Option = struct
  let to_list = function
    | None -> []
    | Some x -> [x]
end

module List = struct
  include List

  let concat_map ~f t = concat (map ~f t)

  let filter_map ~f t = concat_map ~f:Option.to_list (map ~f t)

  let filter_mapi ~f t = filter_map ~f:(fun x -> x) (mapi ~f t)

  let filteri ~f t = filter_mapi ~f:(fun i x -> if f i x then Some x else None) t
end

let remove_elt key list k =
  List.filteri list ~f:(fun i x ->
    key i x <> k)

let insert_elt list i x =
  List.mapi list ~f:(fun j y ->
    if i = j then x else y)

let modify_elt key modify list k e =
  List.mapi list ~f:(fun i x ->
    if key i x = k
    then modify e x
    else x)

let edit_elt key modify edit list =
  match (edit : _ Delta.edit) with
  | Remove k -> remove_elt key list k
  | Insert (i, x) -> insert_elt list i x
  | Modify (k, e) -> modify_elt key modify list k e

let edit_list key modify edits input =
  let (_ : int), result =
    List.fold_left edits ~init:(0, input) ~f:(fun (i, acc) edit ->
      (i + 1, edit_elt key modify edit acc))
  in
  result

let alist_key _ (name, _) = name

let edit_alist = edit_list alist_key

let grammar = edit_alist decl
