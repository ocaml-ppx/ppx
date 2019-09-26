open Migrate_parsetree.Ast_407
open Viewlib

module Attribute : sig

  val has_been_used : Parsetree.attribute -> bool

  type ('a, 'i, 'o) view =
       ('a * string * Location.t * Parsetree.payload * Parsetree.attributes, 'i, 'o) View.t
    -> ('a, 'i, 'o) View.t

  val core_type               : (Parsetree.core_type,               'i, 'o) view

  val row_field               : (Parsetree.row_field,               'i, 'o) view

  val object_field : (Parsetree.object_field, 'i, 'o) view

  val pattern                 : (Parsetree.pattern,                 'i, 'o) view

  val expression              : (Parsetree.expression,              'i, 'o) view

  val value_description       : (Parsetree.value_description,       'i, 'o) view

  val type_declaration        : (Parsetree.type_declaration,        'i, 'o) view

  val label_declaration       : (Parsetree.label_declaration,       'i, 'o) view

  val constructor_declaration : (Parsetree.constructor_declaration, 'i, 'o) view

  val type_extension          : (Parsetree.type_extension,          'i, 'o) view

  val extension_constructor   : (Parsetree.extension_constructor,   'i, 'o) view

  val class_type              : (Parsetree.class_type,              'i, 'o) view

  val class_type_field        : (Parsetree.class_type_field,        'i, 'o) view

  val class_infos             : ('a Parsetree.class_infos,          'i, 'o) view

  val class_expr              : (Parsetree.class_expr,              'i, 'o) view

  val class_field             : (Parsetree.class_field,             'i, 'o) view

  val module_type             : (Parsetree.module_type,             'i, 'o) view

  val module_declaration      : (Parsetree.module_declaration,      'i, 'o) view

  val module_type_declaration : (Parsetree.module_type_declaration, 'i, 'o) view

  val open_description        : (Parsetree.open_description,        'i, 'o) view

  val include_infos           : ('a Parsetree.include_infos,        'i, 'o) view

  val module_expr             : (Parsetree.module_expr,             'i, 'o) view

  val pstr_eval               : (Parsetree.structure_item,          'i, 'o) view

  val value_binding           : (Parsetree.value_binding,           'i, 'o) view

  val module_binding          : (Parsetree.module_binding,          'i, 'o) view

end

module Extension : sig

  type ('a, 'i, 'o) view =
       (string * Location.t * Parsetree.payload, 'i, 'o) View.t
    -> ('a, 'i, 'o) View.t

  type ('a, 'i, 'o) view_attr =
       (string * Location.t * Parsetree.payload * Parsetree.attributes, 'i, 'o) View.t
    -> ('a, 'i, 'o) View.t

  val core_type        : (Parsetree.core_type,        'i, 'o) view

  val pattern          : (Parsetree.pattern,          'i, 'o) view

  val expression       : (Parsetree.expression,       'i, 'o) view

  val class_type       : (Parsetree.class_type,       'i, 'o) view

  val class_type_field : (Parsetree.class_type_field, 'i, 'o) view

  val class_expr       : (Parsetree.class_expr,       'i, 'o) view

  val class_field      : (Parsetree.class_field,      'i, 'o) view

  val module_type      : (Parsetree.module_type,      'i, 'o) view

  val signature_item   : (Parsetree.signature_item,   'i, 'o) view_attr

  val module_expr      : (Parsetree.module_expr,      'i, 'o) view

  val structure_item   : (Parsetree.structure_item,   'i, 'o) view_attr

end

module type Payload = sig

  type ('a, 'i, 'o) view = ('a, 'i, 'o) View.t -> (Parsetree.payload, 'i, 'o) View.t

  val structure : (Parsetree.structure,                             'i, 'o) view

  val signature : (Parsetree.signature,                             'i, 'o) view

  val core_type : (Parsetree.core_type,                             'i, 'o) view

  val pattern   : (Parsetree.pattern * Parsetree.expression option, 'i, 'o) view

  val none      : (Parsetree.payload,                               'i, 'i) View.t

  val simple    : (Parsetree.expression * Parsetree.attributes,     'i, 'o) view

end

module Payload : sig
  module Match : Payload
  module Fail  : Payload
end

val report_ppx_error
   : ?loc:Location.t
  -> ('a, Format.formatter, unit, 'b) format4
  -> 'a

val register_ppx_driver
   : name:string
  -> ?reset_args:(unit -> unit)
  -> ?args:(Arg.key * Arg.spec * Arg.doc) list
  -> Migrate_parsetree.Versions.OCaml_407.types Migrate_parsetree.Driver.rewriter
  -> unit

type cases = Parsetree.case list (* this synonym does not exist before 4.06 *)

type 'a mapper = 'a -> 'a

class virtual default_mapper : object

    val virtual name        : string
    val mutable config_opt  : Migrate_parsetree.Driver.config  option
    val mutable cookies_opt : Migrate_parsetree.Driver.cookies option
    val mutable mapper      : Ast_mapper.mapper

    method config  : Migrate_parsetree.Driver.config
    method cookies : Migrate_parsetree.Driver.cookies

    method attribute               : Parsetree.attribute               mapper
    method attributes              : Parsetree.attributes              mapper
    method case                    : Parsetree.case                    mapper
    method cases                   :           cases                   mapper
    method class_declaration       : Parsetree.class_declaration       mapper
    method class_description       : Parsetree.class_description       mapper
    method class_expr              : Parsetree.class_expr              mapper
    method class_field             : Parsetree.class_field             mapper
    method class_signature         : Parsetree.class_signature         mapper
    method class_structure         : Parsetree.class_structure         mapper
    method class_type              : Parsetree.class_type              mapper
    method class_type_declaration  : Parsetree.class_type_declaration  mapper
    method class_type_field        : Parsetree.class_type_field        mapper
    method constructor_declaration : Parsetree.constructor_declaration mapper
    method expr                    : Parsetree.expression              mapper
    method extension               : Parsetree.extension               mapper
    method extension_constructor   : Parsetree.extension_constructor   mapper
    method include_declaration     : Parsetree.include_declaration     mapper
    method include_description     : Parsetree.include_description     mapper
    method label_declaration       : Parsetree.label_declaration       mapper
    method location                : Location.t                        mapper
    method module_binding          : Parsetree.module_binding          mapper
    method module_declaration      : Parsetree.module_declaration      mapper
    method module_expr             : Parsetree.module_expr             mapper
    method module_type             : Parsetree.module_type             mapper
    method module_type_declaration : Parsetree.module_type_declaration mapper
    method open_description        : Parsetree.open_description        mapper
    method pat                     : Parsetree.pattern                 mapper
    method payload                 : Parsetree.payload                 mapper
    method signature               : Parsetree.signature               mapper
    method signature_item          : Parsetree.signature_item          mapper
    method structure               : Parsetree.structure               mapper
    method structure_item          : Parsetree.structure_item          mapper
    method typ                     : Parsetree.core_type               mapper
    method type_declaration        : Parsetree.type_declaration        mapper
    method type_extension          : Parsetree.type_extension          mapper
    method type_kind               : Parsetree.type_kind               mapper
    method value_binding           : Parsetree.value_binding           mapper
    method value_description       : Parsetree.value_description       mapper
    method with_constraint         : Parsetree.with_constraint         mapper

    method expression              : Parsetree.expression              mapper
    method pattern                 : Parsetree.pattern                 mapper
    method core_type               : Parsetree.core_type               mapper

    method signature_item_to_list  : (Parsetree.signature_item -> Parsetree.signature_item list)
                                  -> Parsetree.signature mapper
    method structure_item_to_list  : (Parsetree.structure_item -> Parsetree.structure_item list)
                                  -> Parsetree.structure mapper

    method register : unit

  end
