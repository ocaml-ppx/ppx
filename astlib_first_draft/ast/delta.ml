open StdLabels

type ('edit, 'self) t =
  | Remove of string
  | Insert of int * 'self
  | Modify of string * 'edit

type nothing = |

type field = (Grammar.data, Grammar.field) t
type clause = (field list, Grammar.clause) t
type kind = (clause list, Grammar.kind) t
type grammar = kind list

let modify list value ~get ~set ~f =
  let list =
    List.fold_left list ~init:(get value) ~f:(fun acc t ->
      f t acc)
  in
  set value list

let rec replace list ~name ~name_of ~f =
  match list with
  | [] ->
    (match f None with
     | None -> []
     | Some x -> [x])
  | head :: tail ->
    if String.equal name (name_of head)
    then (
      match f (Some head) with
      | None -> tail
      | Some x -> x :: tail)
    else head :: replace tail ~name ~name_of ~f

let rec insert list ~index ~value =
  if index = 0
  then value :: list
  else
    match list with
    | [] -> failwith "invalid index"
    | head :: tail -> head :: insert tail ~index:(index - 1) ~value

let apply t list ~name_of ~f =
  match t with
  | Remove name ->
    replace list ~name ~name_of ~f:(function
      | Some _ -> None
      | None -> failwith (name ^ " not found"))
  | Insert (index, value) ->
    replace list ~name:(name_of value) ~name_of ~f:(function
      | None -> None
      | Some _ -> failwith (name_of value ^ " already exists"))
    |> insert ~index ~value
  | Modify (name, edits) ->
    replace list ~name ~name_of ~f:(function
      | None -> failwith (name ^ " not found")
      | Some value -> Some (f edits value))

let apply_to_fields t fields =
  apply t fields
    ~name_of:(fun (field : Grammar.field) -> field.field_name)
    ~f:(fun data (field : Grammar.field) -> { field with data })

let apply_to_clauses t clauses =
  apply t clauses
    ~name_of:(fun (clause : Grammar.clause) -> clause.clause_name)
    ~f:(modify
          ~get:(fun (clause : Grammar.clause) -> clause.fields)
          ~set:(fun (clause : Grammar.clause) fields -> { clause with fields })
          ~f:apply_to_fields)

let apply_to_kinds t kinds =
  apply t kinds
    ~name_of:(fun (kind : Grammar.kind) -> kind.kind_name)
    ~f:(modify
          ~get:(fun (kind : Grammar.kind) -> kind.clauses)
          ~set:(fun (kind : Grammar.kind) clauses -> { kind with clauses })
          ~f:apply_to_clauses)

let apply_to_grammar list grammar =
  modify list grammar
    ~get:(fun grammar -> grammar)
    ~set:(fun _ grammar -> grammar)
    ~f:apply_to_kinds
