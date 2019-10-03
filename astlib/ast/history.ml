open StdLabels

type conversion =
  { name : string
  ; f :
      'a . 'a Ast.node
      -> to_node:('a -> 'a Ast.node)
      -> of_node:('a Ast.node -> 'a)
      -> 'a Ast.node
  }

type previous_version =
  { version : string
  ; next_version : string
  ; delta_from_next : Delta.grammar
  ; to_next : conversion
  ; of_next : conversion
  }

type conversion_step =
  { src_version : string
  ; dst_version : string
  ; conversion : conversion
  }

type t =
  { current_version : string
  ; current_grammar : Grammar.t
  ; previous_versions_in_chronological_order : previous_version list
  }

let ensure_follows previous_version ~version =
  if not (String.equal previous_version.next_version version)
  then
    failwith
      (Printf.sprintf
         "Astlib.History.create: version %s should be followed by %s; \
          was instead followed by %s"
         previous_version.version
         previous_version.next_version
         version)

let ensure_in_chronological_order previous_versions ~current_version =
  let initial_version =
    List.fold_right
      previous_versions
      ~init:current_version
      ~f:(fun previous_version version ->
        ensure_follows previous_version ~version;
        previous_version.version)
  in
  ignore (initial_version : string)

let create ~current_version ~current_grammar ~previous_versions =
  let previous_versions_in_chronological_order =
    ensure_in_chronological_order previous_versions ~current_version;
    previous_versions
  in
  { current_version; current_grammar; previous_versions_in_chronological_order }

let to_versioned_grammars t =
  List.fold_right
    t.previous_versions_in_chronological_order
    ~init:(t.current_grammar, [(t.current_version, t.current_grammar)])
    ~f:(fun previous_version (current_grammar, list) ->
      let previous_grammar =
        Delta.apply_to_grammar previous_version.delta_from_next current_grammar
      in
      previous_grammar, (previous_version.version, previous_grammar) :: list)
  |> snd

let rec find_previous_version_index list ~version ~index =
  match list with
  | [] -> None
  | previous_version :: list ->
    if String.equal version previous_version.version
    then Some index
    else find_previous_version_index list ~version ~index:(index + 1)

let version_index t ~version =
  if String.equal version t.current_version
  then Some (List.length t.previous_versions_in_chronological_order)
  else
    find_previous_version_index
      t.previous_versions_in_chronological_order
      ~version
      ~index:0

let rec build_downgrade_steps list ~oldest_index ~newest_index ~index ~acc =
  match list with
  | [] -> acc
  | this_version :: newer_versions ->
    if index < oldest_index
    then
      build_downgrade_steps
        newer_versions
        ~oldest_index
        ~newest_index
        ~index:(index + 1)
        ~acc
    else if newest_index <= index
    then acc
    else (
      let step =
        { src_version = this_version.next_version
        ; dst_version = this_version.version
        ; conversion = this_version.of_next
        }
      in
      build_downgrade_steps
        newer_versions
        ~oldest_index
        ~newest_index
        ~index:(index + 1)
        ~acc:(step :: acc))

let downgrade_steps t ~oldest_index ~newest_index =
  build_downgrade_steps
    t.previous_versions_in_chronological_order
    ~oldest_index
    ~newest_index
    ~index:0
    ~acc:[]

let rec build_upgrade_steps list ~oldest_index ~newest_index ~index ~acc =
  match list with
  | [] -> List.rev acc
  | this_version :: newer_versions ->
    if index < oldest_index
    then
      build_upgrade_steps
        newer_versions
        ~oldest_index
        ~newest_index
        ~index:(index + 1)
        ~acc
    else if newest_index <= index
    then List.rev acc
    else (
      let step =
        { src_version = this_version.version
        ; dst_version = this_version.next_version
        ; conversion = this_version.to_next
        }
      in
      build_upgrade_steps
        newer_versions
        ~oldest_index
        ~newest_index
        ~index:(index + 1)
        ~acc:(step :: acc))

let upgrade_steps t ~oldest_index ~newest_index =
  build_upgrade_steps
    t.previous_versions_in_chronological_order
    ~oldest_index
    ~newest_index
    ~index:0
    ~acc:[]

let conversion_steps t ~from_version ~to_version =
  match
    version_index t ~version:from_version,
    version_index t ~version:to_version
  with
  | (None, _) | (_, None) -> None
  | Some from_index, Some to_index ->
    let steps =
      if from_index < to_index
      then upgrade_steps t ~oldest_index:from_index ~newest_index:to_index
      else if from_index > to_index
      then downgrade_steps t ~oldest_index:to_index ~newest_index:from_index
      else []
    in
    Some steps
