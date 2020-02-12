open Stdppx
open Ppx_ast

module Extensions = Ppx_metaquot_expander.Extensions (struct
    let mark_attribute_as_handled_manually attribute =
      Ppx.Attribute.mark_as_handled_manually (Conversion.ast_to_attribute attribute)

    let assert_no_attributes attributes =
      Ppx.assert_no_attributes (Conversion.ast_to_attributes attributes)
  end)

let extensions = List.map Extensions.extensions ~f:Ppx.Extension.of_bootstrap_extension

let () = Ppx.Driver.register_transformation "metaquot" ~extensions
