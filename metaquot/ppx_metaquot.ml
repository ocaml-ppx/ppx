let () =
  Ppx.Driver.register_transformation
    "metaquot"
    ~extensions:Ppx_metaquot_expander.extensions
