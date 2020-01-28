open Stdppx

module Extensions = Ppx_metaquot_expander.Extensions (struct
    let mark_attribute_as_handled_manually = Ppx.Attribute.mark_as_handled_manually
    let assert_no_attributes = Ppx.assert_no_attributes
  end)

let extensions = List.map Extensions.extensions ~f:Ppx.Extension.of_bootstrap_extension

let () = Ppx.Driver.register_transformation "metaquot" ~extensions
