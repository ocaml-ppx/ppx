open Stdppx

module Extensions = Ppx_metaquot_expander.Extensions (struct
    let mark_attribute_as_handled_manually attribute =
      Ppx.Attr.mark_as_handled_manually attribute

    let assert_no_attributes attributes =
      Ppx.assert_no_attributes attributes
  end)

let extensions = List.map Extensions.extensions ~f:Ppx.Ext.of_bootstrap_extension

let () = Ppx.Driver.register_transformation "metaquot" ~extensions
