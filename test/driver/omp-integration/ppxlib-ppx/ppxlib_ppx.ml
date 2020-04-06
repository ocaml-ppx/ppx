open Ppx

let () =
  Driver.register_transformation "plop"
    ~rules:[
      Context_free.Rule.extension
        (Ext.declare_with_path_arg "plop"
           Expression
           Ast_pattern.(pstr nil)
           (fun ~loc ~path:_ ~arg ->
              let open Ast_builder in
              match arg with
              | None -> estring ~loc "-"
              | Some { loc; txt } -> estring ~loc (Longid.name txt)))]
