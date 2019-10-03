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

let list_key i _ = i

let edit_named modify e (name, x) = (name, modify e x)

let edit_alist modify = edit_list alist_key (edit_named modify)

let structural (e : Delta.structural) (_ : Grammar.structural) : Grammar.structural = e

let tuple = edit_list list_key structural

let record = edit_alist structural

let clause (e : Delta.clause) (g : Grammar.clause) : Grammar.clause =
  match e, g with
  | Tuple e, Tuple g -> Tuple (tuple e g)
  | Record e, Record g -> Record (record e g)
  | _ -> failwith (__LOC__ ^ ": clause edit/grammar mismatch")

let variant = edit_alist clause

let nominal (e : Delta.nominal) (g : Grammar.nominal) : Grammar.nominal =
  match e, g with
  | Alias e, Alias g -> Alias (structural e g)
  | Record e, Record g -> Record (record e g)
  | Variant e, Variant g -> Variant (variant e g)
  | _ -> failwith (__LOC__ ^ ": nominal edit/grammar mismatch")

let decl e ({ vars; body } : Grammar.decl) : Grammar.decl =
  { vars; body = nominal e body }

let grammar = edit_alist decl
