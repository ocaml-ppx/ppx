open StdLabels

module Current = Version_4_07

let current_version = Current.version

let versions : (module Ast_version_intf.S) list =
  [ (module Unstable_for_testing)
  ; (module Current)
  ]

let history =
  let versioned_grammars =
    List.map versions ~f:(fun (module V : Ast_version_intf.S) ->
      (V.version, V.grammar))
  in
  let conversions =
    List.concat
      (List.map versions ~f:(fun (module V : Ast_version_intf.S) ->
         V.conversions))
  in
  History.create
    ~versioned_grammars
    ~conversions
