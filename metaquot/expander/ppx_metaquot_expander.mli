open! Stdppx

module type Driver = sig
  val assert_no_attributes : Ppx_ast.attributes -> unit
  val mark_attribute_as_handled_manually : Ppx_ast.attribute -> unit
end

module Extensions (Driver : Driver) : sig
  val extensions : Ppx_bootstrap.Extension.t list
end
