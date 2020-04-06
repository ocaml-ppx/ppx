open Ppx

(*
   [[@@deriving foo]] expands to:
   {[
     let _ = [%foo]
   ]}

   and then [[%foo]] expands to ["foo"].
*)

let add_deriver () =
  let str_type_decl =
    Deriving.Generator.make_noarg (
      fun ~loc ~path:_ _ ->
        let expr desc : expression =
          Expression.create
            ~pexp_desc:desc
            ~pexp_loc:loc
            ~pexp_attributes:(Attributes.create [])
        in
        Structure.create [
          Structure_item.create
            ~pstr_loc:loc
            ~pstr_desc:
              (Structure_item_desc.pstr_value
                 Rec_flag.nonrecursive
                 [Value_binding.create
                    ~pvb_pat:
                      (Pattern.create
                         ~ppat_desc:Pattern_desc.ppat_any
                         ~ppat_loc:loc
                         ~ppat_attributes:(Attributes.create []))
                    ~pvb_expr:
                      (expr
                         (Expression_desc.pexp_extension
                            (Extension.create
                               ( { loc; txt="foo" }
                               , Payload.pStr (Structure.create [])))))
                    ~pvb_attributes:(Attributes.create [])
                    ~pvb_loc:loc])
        ]
    )
      ~attributes:[]
  in
  let sig_type_decl =
    Deriving.Generator.make_noarg (
      fun ~loc ~path decl ->
        ignore loc;
        ignore path;
        ignore decl;
        Signature.create []
    )
  in
  Deriving.add "foo"
    ~str_type_decl
    ~sig_type_decl

let () =
  Driver.register_transformation "foo"
    ~rules:[
      Context_free.Rule.extension
        (Ext.declare "foo"
           Expression Ast_pattern.__
           (fun ~loc ~path:_ _payload ->
              Expression.create
                ~pexp_desc:
                  (Expression_desc.pexp_constant (Constant.pconst_string "foo" None))
                ~pexp_loc:loc
                ~pexp_attributes:(Attributes.create [])))
    ]

let (_ : Deriving.t) = add_deriver ()
