open StdLabels

let versions : (module Version_intf.S) list =
  [ (module Version_4_07)
  ; (module Version_4_06)
  ]

let current_version =
  let (module V) = List.hd versions in
  V.version

let history =
  let versioned_grammars =
    List.map versions ~f:(fun (module V : Version_intf.S) ->
      (V.version, V.grammar))
  in
  let conversions =
    List.concat
      (List.map versions ~f:(fun (module V : Version_intf.S) ->
         V.conversions))
  in
  History.create
    ~versioned_grammars
    ~conversions
