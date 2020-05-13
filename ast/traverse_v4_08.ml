(*$ Ppx_ast_cinaps.print_traverse_ml (Astlib.Version.of_string "V4_07") *)
class map =
  object
    inherit Traverse_builtins.map
    inherit Virtual_traverse.V4_07.map
  end

class iter =
  object
    inherit Traverse_builtins.iter
    inherit Virtual_traverse.V4_07.iter
  end

class ['acc] fold =
  object
    inherit ['acc] Traverse_builtins.fold
    inherit ['acc] Virtual_traverse.V4_07.fold
  end

class ['acc] fold_map =
  object
    inherit ['acc] Traverse_builtins.fold_map
    inherit ['acc] Virtual_traverse.V4_07.fold_map
  end

class ['ctx] map_with_context =
  object
    inherit ['ctx] Traverse_builtins.map_with_context
    inherit ['ctx] Virtual_traverse.V4_07.map_with_context
  end

class virtual ['res] lift =
  object
    inherit ['res] Traverse_builtins.lift
    inherit ['res] Virtual_traverse.V4_07.lift
  end
(*$*)
