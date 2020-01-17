open Stdppx

type t = (string * Astlib.Grammar.record) String.Map.t

let has_desc_field record =
  List.exists record
    ~f:(fun (field_name, _ty) -> String.is_suffix ~suffix:"_desc" field_name)

let from_grammar grammar =
  List.fold_left grammar
    ~init:String.Map.empty
    ~f:(fun acc (name, kind) ->
      match (kind : Astlib.Grammar.kind) with
      | Poly (_, _) -> acc
      | Mono decl ->
        match decl with
        | Alias _
        | Variant _ -> acc
        | Record record ->
          if has_desc_field record then
            String.Map.add acc (name ^ "_desc") (name, record)
          else
            acc)

let shortcut t desc_type =
  String.Map.find t desc_type
