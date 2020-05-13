open Viewlib

(*$ Ppx_ast_cinaps.print_viewer_mli (Astlib.Version.of_string "v4_08") *)
open Versions
open V4_08
include module type of Viewer_common

val lident'const : (string, 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t

val ldot'const : ((Longident.t * string), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t

val lapply'const : ((Longident.t * Longident.t), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
val longident_loc'const: (Longident.t Astlib.Loc.t, 'i, 'o) View.t -> (Longident_loc.t, 'i, 'o) View.t

val nonrecursive'const : (Rec_flag.t, 'a, 'a) View.t

val recursive'const : (Rec_flag.t, 'a, 'a) View.t

val upto'const : (Direction_flag.t, 'a, 'a) View.t

val downto'const : (Direction_flag.t, 'a, 'a) View.t

val private'const : (Private_flag.t, 'a, 'a) View.t

val public'const : (Private_flag.t, 'a, 'a) View.t

val immutable'const : (Mutable_flag.t, 'a, 'a) View.t

val mutable'const : (Mutable_flag.t, 'a, 'a) View.t

val virtual'const : (Virtual_flag.t, 'a, 'a) View.t

val concrete'const : (Virtual_flag.t, 'a, 'a) View.t

val override'const : (Override_flag.t, 'a, 'a) View.t

val fresh'const : (Override_flag.t, 'a, 'a) View.t

val closed'const : (Closed_flag.t, 'a, 'a) View.t

val open'const : (Closed_flag.t, 'a, 'a) View.t

val nolabel'const : (Arg_label.t, 'a, 'a) View.t

val labelled'const : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t

val optional'const : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t

val covariant'const : (Variance.t, 'a, 'a) View.t

val contravariant'const : (Variance.t, 'a, 'a) View.t

val invariant'const : (Variance.t, 'a, 'a) View.t

val pconst_integer'const : ((string * char option), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t

val pconst_char'const : (char, 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t

val pconst_string'const : ((string * string option), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t

val pconst_float'const : ((string * char option), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t

val attr_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Attribute.t, 'i, 'o) View.t

val attr_payload'match : (Payload.t, 'i, 'o) View.t -> (Attribute.t, 'i, 'o) View.t

val attr_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Attribute.t, 'i, 'o) View.t
val extension'const: ((string Astlib.Loc.t * Payload.t), 'i, 'o) View.t -> (Extension.t, 'i, 'o) View.t
val attributes'const: (Attribute.t list, 'i, 'o) View.t -> (Attributes.t, 'i, 'o) View.t

val pstr'const : (Structure.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t

val psig'const : (Signature.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t

val ptyp'const : (Core_type.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t

val ppat'const : ((Pattern.t * Expression.t option), 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t

val ptyp_desc'match : (Core_type_desc.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_loc_stack'match : (Astlib.Location.t list, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_any'const : (Core_type_desc.t, 'a, 'a) View.t

val tany'const : (Core_type.t, 'a, 'a) View.t

val ptyp_var'const : (string, 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val tvar'const : (string, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_arrow'const : ((Arg_label.t * Core_type.t * Core_type.t), 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val tarrow'const : ((Arg_label.t * Core_type.t * Core_type.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_tuple'const : (Core_type.t list, 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val ttuple'const : (Core_type.t list, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_constr'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val tconstr'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_object'const : ((Object_field.t list * Closed_flag.t), 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val tobject'const : ((Object_field.t list * Closed_flag.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_class'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val tclass'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_alias'const : ((Core_type.t * string), 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val talias'const : ((Core_type.t * string), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_variant'const : ((Row_field.t list * Closed_flag.t * string list option), 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val tvariant'const : ((Row_field.t list * Closed_flag.t * string list option), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_poly'const : ((string Astlib.Loc.t list * Core_type.t), 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val tpoly'const : ((string Astlib.Loc.t list * Core_type.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_package'const : (Package_type.t, 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val tpackage'const : (Package_type.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

val ptyp_extension'const : (Extension.t, 'i, 'o) View.t -> (Core_type_desc.t, 'i, 'o) View.t

val textension'const : (Extension.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
val package_type'const: ((Longident_loc.t * (Longident_loc.t * Core_type.t) list), 'i, 'o) View.t -> (Package_type.t, 'i, 'o) View.t

val prf_desc'match : (Row_field_desc.t, 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t

val prf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t

val prf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t

val rtag'const : ((string Astlib.Loc.t * bool * Core_type.t list), 'i, 'o) View.t -> (Row_field_desc.t, 'i, 'o) View.t

val rfrtag'const : ((string Astlib.Loc.t * bool * Core_type.t list), 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t

val rinherit'const : (Core_type.t, 'i, 'o) View.t -> (Row_field_desc.t, 'i, 'o) View.t

val rfrinherit'const : (Core_type.t, 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t

val pof_desc'match : (Object_field_desc.t, 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t

val pof_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t

val pof_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t

val otag'const : ((string Astlib.Loc.t * Core_type.t), 'i, 'o) View.t -> (Object_field_desc.t, 'i, 'o) View.t

val ofotag'const : ((string Astlib.Loc.t * Core_type.t), 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t

val oinherit'const : (Core_type.t, 'i, 'o) View.t -> (Object_field_desc.t, 'i, 'o) View.t

val ofoinherit'const : (Core_type.t, 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t

val ppat_desc'match : (Pattern_desc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_loc_stack'match : (Astlib.Location.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_any'const : (Pattern_desc.t, 'a, 'a) View.t

val pany'const : (Pattern.t, 'a, 'a) View.t

val ppat_var'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val pvar'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_alias'const : ((Pattern.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val palias'const : ((Pattern.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_constant'const : (Constant.t, 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val pconstant'const : (Constant.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_interval'const : ((Constant.t * Constant.t), 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val pinterval'const : ((Constant.t * Constant.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_tuple'const : (Pattern.t list, 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val ptuple'const : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_construct'const : ((Longident_loc.t * Pattern.t option), 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val pconstruct'const : ((Longident_loc.t * Pattern.t option), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_variant'const : ((string * Pattern.t option), 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val pvariant'const : ((string * Pattern.t option), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_record'const : (((Longident_loc.t * Pattern.t) list * Closed_flag.t), 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val precord'const : (((Longident_loc.t * Pattern.t) list * Closed_flag.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_array'const : (Pattern.t list, 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val parray'const : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_or'const : ((Pattern.t * Pattern.t), 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val por'const : ((Pattern.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_constraint'const : ((Pattern.t * Core_type.t), 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val pconstraint'const : ((Pattern.t * Core_type.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_type'const : (Longident_loc.t, 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val ptype'const : (Longident_loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_lazy'const : (Pattern.t, 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val plazy'const : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_unpack'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val punpack'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_exception'const : (Pattern.t, 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val pexception'const : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_extension'const : (Extension.t, 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val pextension'const : (Extension.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val ppat_open'const : ((Longident_loc.t * Pattern.t), 'i, 'o) View.t -> (Pattern_desc.t, 'i, 'o) View.t

val popen'const : ((Longident_loc.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

val pexp_desc'match : (Expression_desc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_loc_stack'match : (Astlib.Location.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_ident'const : (Longident_loc.t, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eident'const : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_constant'const : (Constant.t, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val econstant'const : (Constant.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_let'const : ((Rec_flag.t * Value_binding.t list * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val elet'const : ((Rec_flag.t * Value_binding.t list * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_function'const : (Case.t list, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val efunction'const : (Case.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_fun'const : ((Arg_label.t * Expression.t option * Pattern.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val efun'const : ((Arg_label.t * Expression.t option * Pattern.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_apply'const : ((Expression.t * (Arg_label.t * Expression.t) list), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eapply'const : ((Expression.t * (Arg_label.t * Expression.t) list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_match'const : ((Expression.t * Case.t list), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val ematch'const : ((Expression.t * Case.t list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_try'const : ((Expression.t * Case.t list), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val etry'const : ((Expression.t * Case.t list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_tuple'const : (Expression.t list, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val etuple'const : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_construct'const : ((Longident_loc.t * Expression.t option), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val econstruct'const : ((Longident_loc.t * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_variant'const : ((string * Expression.t option), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val evariant'const : ((string * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_record'const : (((Longident_loc.t * Expression.t) list * Expression.t option), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val erecord'const : (((Longident_loc.t * Expression.t) list * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_field'const : ((Expression.t * Longident_loc.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val efield'const : ((Expression.t * Longident_loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_setfield'const : ((Expression.t * Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val esetfield'const : ((Expression.t * Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_array'const : (Expression.t list, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val earray'const : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_ifthenelse'const : ((Expression.t * Expression.t * Expression.t option), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eifthenelse'const : ((Expression.t * Expression.t * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_sequence'const : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val esequence'const : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_while'const : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val ewhile'const : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_for'const : ((Pattern.t * Expression.t * Expression.t * Direction_flag.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val efor'const : ((Pattern.t * Expression.t * Expression.t * Direction_flag.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_constraint'const : ((Expression.t * Core_type.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val econstraint'const : ((Expression.t * Core_type.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_coerce'const : ((Expression.t * Core_type.t option * Core_type.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val ecoerce'const : ((Expression.t * Core_type.t option * Core_type.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_send'const : ((Expression.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val esend'const : ((Expression.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_new'const : (Longident_loc.t, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val enew'const : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_setinstvar'const : ((string Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val esetinstvar'const : ((string Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_override'const : ((string Astlib.Loc.t * Expression.t) list, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eoverride'const : ((string Astlib.Loc.t * Expression.t) list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_letmodule'const : ((string Astlib.Loc.t * Module_expr.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eletmodule'const : ((string Astlib.Loc.t * Module_expr.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_letexception'const : ((Extension_constructor.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eletexception'const : ((Extension_constructor.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_assert'const : (Expression.t, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eassert'const : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_lazy'const : (Expression.t, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val elazy'const : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_poly'const : ((Expression.t * Core_type.t option), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val epoly'const : ((Expression.t * Core_type.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_object'const : (Class_structure.t, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eobject'const : (Class_structure.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_newtype'const : ((string Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val enewtype'const : ((string Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_pack'const : (Module_expr.t, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val epack'const : (Module_expr.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_open'const : ((Open_declaration.t * Expression.t), 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eopen'const : ((Open_declaration.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_letop'const : (Letop.t, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eletop'const : (Letop.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_extension'const : (Extension.t, 'i, 'o) View.t -> (Expression_desc.t, 'i, 'o) View.t

val eextension'const : (Extension.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

val pexp_unreachable'const : (Expression_desc.t, 'a, 'a) View.t

val eunreachable'const : (Expression.t, 'a, 'a) View.t

val pc_lhs'match : (Pattern.t, 'i, 'o) View.t -> (Case.t, 'i, 'o) View.t

val pc_guard'match : (Expression.t option, 'i, 'o) View.t -> (Case.t, 'i, 'o) View.t

val pc_rhs'match : (Expression.t, 'i, 'o) View.t -> (Case.t, 'i, 'o) View.t

val let_'match : (Binding_op.t, 'i, 'o) View.t -> (Letop.t, 'i, 'o) View.t

val ands'match : (Binding_op.t list, 'i, 'o) View.t -> (Letop.t, 'i, 'o) View.t

val body'match : (Expression.t, 'i, 'o) View.t -> (Letop.t, 'i, 'o) View.t

val pbop_op'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Binding_op.t, 'i, 'o) View.t

val pbop_pat'match : (Pattern.t, 'i, 'o) View.t -> (Binding_op.t, 'i, 'o) View.t

val pbop_exp'match : (Expression.t, 'i, 'o) View.t -> (Binding_op.t, 'i, 'o) View.t

val pbop_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Binding_op.t, 'i, 'o) View.t

val pval_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

val pval_type'match : (Core_type.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

val pval_prim'match : (string list, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

val pval_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

val pval_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

val ptype_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

val ptype_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

val ptype_cstrs'match : ((Core_type.t * Core_type.t * Astlib.Location.t) list, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

val ptype_kind'match : (Type_kind.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

val ptype_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

val ptype_manifest'match : (Core_type.t option, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

val ptype_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

val ptype_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

val ptype_abstract'const : (Type_kind.t, 'a, 'a) View.t

val ptype_variant'const : (Constructor_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t

val ptype_record'const : (Label_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t

val ptype_open'const : (Type_kind.t, 'a, 'a) View.t

val pld_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

val pld_mutable'match : (Mutable_flag.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

val pld_type'match : (Core_type.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

val pld_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

val pld_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

val pcd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

val pcd_args'match : (Constructor_arguments.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

val pcd_res'match : (Core_type.t option, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

val pcd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

val pcd_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

val pcstr_tuple'const : (Core_type.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t

val pcstr_record'const : (Label_declaration.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t

val ptyext_path'match : (Longident_loc.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

val ptyext_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

val ptyext_constructors'match : (Extension_constructor.t list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

val ptyext_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

val ptyext_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

val ptyext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

val pext_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

val pext_kind'match : (Extension_constructor_kind.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

val pext_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

val pext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

val ptyexn_constructor'match : (Extension_constructor.t, 'i, 'o) View.t -> (Type_exception.t, 'i, 'o) View.t

val ptyexn_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Type_exception.t, 'i, 'o) View.t

val ptyexn_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_exception.t, 'i, 'o) View.t

val pext_decl'const : ((Constructor_arguments.t * Core_type.t option), 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t

val pext_rebind'const : (Longident_loc.t, 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t

val pcty_desc'match : (Class_type_desc.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

val pcty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

val pcty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

val pcty_constr'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Class_type_desc.t, 'i, 'o) View.t

val ctconstr'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

val pcty_signature'const : (Class_signature.t, 'i, 'o) View.t -> (Class_type_desc.t, 'i, 'o) View.t

val ctsignature'const : (Class_signature.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

val pcty_arrow'const : ((Arg_label.t * Core_type.t * Class_type.t), 'i, 'o) View.t -> (Class_type_desc.t, 'i, 'o) View.t

val ctarrow'const : ((Arg_label.t * Core_type.t * Class_type.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

val pcty_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_type_desc.t, 'i, 'o) View.t

val ctextension'const : (Extension.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

val pcty_open'const : ((Open_description.t * Class_type.t), 'i, 'o) View.t -> (Class_type_desc.t, 'i, 'o) View.t

val ctopen'const : ((Open_description.t * Class_type.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

val pcsig_self'match : (Core_type.t, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

val pcsig_fields'match : (Class_type_field.t list, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

val pctf_desc'match : (Class_type_field_desc.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

val pctf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

val pctf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

val pctf_inherit'const : (Class_type.t, 'i, 'o) View.t -> (Class_type_field_desc.t, 'i, 'o) View.t

val ctfinherit'const : (Class_type.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

val pctf_val'const : ((string Astlib.Loc.t * Mutable_flag.t * Virtual_flag.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field_desc.t, 'i, 'o) View.t

val ctfval'const : ((string Astlib.Loc.t * Mutable_flag.t * Virtual_flag.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

val pctf_method'const : ((string Astlib.Loc.t * Private_flag.t * Virtual_flag.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field_desc.t, 'i, 'o) View.t

val ctfmethod'const : ((string Astlib.Loc.t * Private_flag.t * Virtual_flag.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

val pctf_constraint'const : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field_desc.t, 'i, 'o) View.t

val ctfconstraint'const : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

val pctf_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Class_type_field_desc.t, 'i, 'o) View.t

val ctfattribute'const : (Attribute.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

val pctf_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_type_field_desc.t, 'i, 'o) View.t

val ctfextension'const : (Extension.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

val pci_virt'match : (Virtual_flag.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

val pci_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

val pci_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

val pci_expr'match : ('a node, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

val pci_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

val pci_attributes'match : (Attributes.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t
val class_description'const: (Class_type.t Class_infos.t, 'i, 'o) View.t -> (Class_description.t, 'i, 'o) View.t
val class_type_declaration'const: (Class_type.t Class_infos.t, 'i, 'o) View.t -> (Class_type_declaration.t, 'i, 'o) View.t

val pcl_desc'match : (Class_expr_desc.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_constr'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Class_expr_desc.t, 'i, 'o) View.t

val ceconstr'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_structure'const : (Class_structure.t, 'i, 'o) View.t -> (Class_expr_desc.t, 'i, 'o) View.t

val cestructure'const : (Class_structure.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_fun'const : ((Arg_label.t * Expression.t option * Pattern.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr_desc.t, 'i, 'o) View.t

val cefun'const : ((Arg_label.t * Expression.t option * Pattern.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_apply'const : ((Class_expr.t * (Arg_label.t * Expression.t) list), 'i, 'o) View.t -> (Class_expr_desc.t, 'i, 'o) View.t

val ceapply'const : ((Class_expr.t * (Arg_label.t * Expression.t) list), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_let'const : ((Rec_flag.t * Value_binding.t list * Class_expr.t), 'i, 'o) View.t -> (Class_expr_desc.t, 'i, 'o) View.t

val celet'const : ((Rec_flag.t * Value_binding.t list * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_constraint'const : ((Class_expr.t * Class_type.t), 'i, 'o) View.t -> (Class_expr_desc.t, 'i, 'o) View.t

val ceconstraint'const : ((Class_expr.t * Class_type.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_expr_desc.t, 'i, 'o) View.t

val ceextension'const : (Extension.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcl_open'const : ((Open_description.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr_desc.t, 'i, 'o) View.t

val ceopen'const : ((Open_description.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

val pcstr_self'match : (Pattern.t, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

val pcstr_fields'match : (Class_field.t list, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

val pcf_desc'match : (Class_field_desc.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val pcf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val pcf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val pcf_inherit'const : ((Override_flag.t * Class_expr.t * string Astlib.Loc.t option), 'i, 'o) View.t -> (Class_field_desc.t, 'i, 'o) View.t

val cfinherit'const : ((Override_flag.t * Class_expr.t * string Astlib.Loc.t option), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val pcf_val'const : ((string Astlib.Loc.t * Mutable_flag.t * Class_field_kind.t), 'i, 'o) View.t -> (Class_field_desc.t, 'i, 'o) View.t

val cfval'const : ((string Astlib.Loc.t * Mutable_flag.t * Class_field_kind.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val pcf_method'const : ((string Astlib.Loc.t * Private_flag.t * Class_field_kind.t), 'i, 'o) View.t -> (Class_field_desc.t, 'i, 'o) View.t

val cfmethod'const : ((string Astlib.Loc.t * Private_flag.t * Class_field_kind.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val pcf_constraint'const : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_field_desc.t, 'i, 'o) View.t

val cfconstraint'const : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val pcf_initializer'const : (Expression.t, 'i, 'o) View.t -> (Class_field_desc.t, 'i, 'o) View.t

val cfinitializer'const : (Expression.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val pcf_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Class_field_desc.t, 'i, 'o) View.t

val cfattribute'const : (Attribute.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val pcf_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_field_desc.t, 'i, 'o) View.t

val cfextension'const : (Extension.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

val cfk_virtual'const : (Core_type.t, 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t

val cfk_concrete'const : ((Override_flag.t * Expression.t), 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t
val class_declaration'const: (Class_expr.t Class_infos.t, 'i, 'o) View.t -> (Class_declaration.t, 'i, 'o) View.t

val pmty_desc'match : (Module_type_desc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

val pmty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

val pmty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

val pmty_ident'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_type_desc.t, 'i, 'o) View.t

val mtident'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

val pmty_signature'const : (Signature.t, 'i, 'o) View.t -> (Module_type_desc.t, 'i, 'o) View.t

val mtsignature'const : (Signature.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

val pmty_functor'const : ((string Astlib.Loc.t * Module_type.t option * Module_type.t), 'i, 'o) View.t -> (Module_type_desc.t, 'i, 'o) View.t

val mtfunctor'const : ((string Astlib.Loc.t * Module_type.t option * Module_type.t), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

val pmty_with'const : ((Module_type.t * With_constraint.t list), 'i, 'o) View.t -> (Module_type_desc.t, 'i, 'o) View.t

val mtwith'const : ((Module_type.t * With_constraint.t list), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

val pmty_typeof'const : (Module_expr.t, 'i, 'o) View.t -> (Module_type_desc.t, 'i, 'o) View.t

val mttypeof'const : (Module_expr.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

val pmty_extension'const : (Extension.t, 'i, 'o) View.t -> (Module_type_desc.t, 'i, 'o) View.t

val mtextension'const : (Extension.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

val pmty_alias'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_type_desc.t, 'i, 'o) View.t

val mtalias'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
val signature'const: (Signature_item.t list, 'i, 'o) View.t -> (Signature.t, 'i, 'o) View.t

val psig_desc'match : (Signature_item_desc.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_value'const : (Value_description.t, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigvalue'const : (Value_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_type'const : ((Rec_flag.t * Type_declaration.t list), 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigtype'const : ((Rec_flag.t * Type_declaration.t list), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_typesubst'const : (Type_declaration.t list, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigtypesubst'const : (Type_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_typext'const : (Type_extension.t, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigtypext'const : (Type_extension.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_exception'const : (Type_exception.t, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigexception'const : (Type_exception.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_module'const : (Module_declaration.t, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigmodule'const : (Module_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_modsubst'const : (Module_substitution.t, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigmodsubst'const : (Module_substitution.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_recmodule'const : (Module_declaration.t list, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigrecmodule'const : (Module_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_modtype'const : (Module_type_declaration.t, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigmodtype'const : (Module_type_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_open'const : (Open_description.t, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigopen'const : (Open_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_include'const : (Include_description.t, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val siginclude'const : (Include_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_class'const : (Class_description.t list, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigclass'const : (Class_description.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_class_type'const : (Class_type_declaration.t list, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigclass_type'const : (Class_type_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigattribute'const : (Attribute.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val psig_extension'const : ((Extension.t * Attributes.t), 'i, 'o) View.t -> (Signature_item_desc.t, 'i, 'o) View.t

val sigextension'const : ((Extension.t * Attributes.t), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

val pmd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

val pmd_type'match : (Module_type.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

val pmd_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

val pmd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

val pms_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_substitution.t, 'i, 'o) View.t

val pms_manifest'match : (Longident_loc.t, 'i, 'o) View.t -> (Module_substitution.t, 'i, 'o) View.t

val pms_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_substitution.t, 'i, 'o) View.t

val pms_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_substitution.t, 'i, 'o) View.t

val pmtd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

val pmtd_type'match : (Module_type.t option, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

val pmtd_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

val pmtd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

val popen_expr'match : ('a node, 'i, 'o) View.t -> ('a node Open_infos.t, 'i, 'o) View.t

val popen_override'match : (Override_flag.t, 'i, 'o) View.t -> ('a node Open_infos.t, 'i, 'o) View.t

val popen_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node Open_infos.t, 'i, 'o) View.t

val popen_attributes'match : (Attributes.t, 'i, 'o) View.t -> ('a node Open_infos.t, 'i, 'o) View.t
val open_description'const: (Longident_loc.t Open_infos.t, 'i, 'o) View.t -> (Open_description.t, 'i, 'o) View.t
val open_declaration'const: (Module_expr.t Open_infos.t, 'i, 'o) View.t -> (Open_declaration.t, 'i, 'o) View.t

val pincl_mod'match : ('a node, 'i, 'o) View.t -> ('a node Include_infos.t, 'i, 'o) View.t

val pincl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node Include_infos.t, 'i, 'o) View.t

val pincl_attributes'match : (Attributes.t, 'i, 'o) View.t -> ('a node Include_infos.t, 'i, 'o) View.t
val include_description'const: (Module_type.t Include_infos.t, 'i, 'o) View.t -> (Include_description.t, 'i, 'o) View.t
val include_declaration'const: (Module_expr.t Include_infos.t, 'i, 'o) View.t -> (Include_declaration.t, 'i, 'o) View.t

val pwith_type'const : ((Longident_loc.t * Type_declaration.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t

val pwith_module'const : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t

val pwith_typesubst'const : ((Longident_loc.t * Type_declaration.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t

val pwith_modsubst'const : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t

val pmod_desc'match : (Module_expr_desc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

val pmod_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

val pmod_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

val pmod_ident'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_expr_desc.t, 'i, 'o) View.t

val meident'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

val pmod_structure'const : (Structure.t, 'i, 'o) View.t -> (Module_expr_desc.t, 'i, 'o) View.t

val mestructure'const : (Structure.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

val pmod_functor'const : ((string Astlib.Loc.t * Module_type.t option * Module_expr.t), 'i, 'o) View.t -> (Module_expr_desc.t, 'i, 'o) View.t

val mefunctor'const : ((string Astlib.Loc.t * Module_type.t option * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

val pmod_apply'const : ((Module_expr.t * Module_expr.t), 'i, 'o) View.t -> (Module_expr_desc.t, 'i, 'o) View.t

val meapply'const : ((Module_expr.t * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

val pmod_constraint'const : ((Module_expr.t * Module_type.t), 'i, 'o) View.t -> (Module_expr_desc.t, 'i, 'o) View.t

val meconstraint'const : ((Module_expr.t * Module_type.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

val pmod_unpack'const : (Expression.t, 'i, 'o) View.t -> (Module_expr_desc.t, 'i, 'o) View.t

val meunpack'const : (Expression.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

val pmod_extension'const : (Extension.t, 'i, 'o) View.t -> (Module_expr_desc.t, 'i, 'o) View.t

val meextension'const : (Extension.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
val structure'const: (Structure_item.t list, 'i, 'o) View.t -> (Structure.t, 'i, 'o) View.t

val pstr_desc'match : (Structure_item_desc.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_eval'const : ((Expression.t * Attributes.t), 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val streval'const : ((Expression.t * Attributes.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_value'const : ((Rec_flag.t * Value_binding.t list), 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strvalue'const : ((Rec_flag.t * Value_binding.t list), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_primitive'const : (Value_description.t, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strprimitive'const : (Value_description.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_type'const : ((Rec_flag.t * Type_declaration.t list), 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strtype'const : ((Rec_flag.t * Type_declaration.t list), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_typext'const : (Type_extension.t, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strtypext'const : (Type_extension.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_exception'const : (Type_exception.t, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strexception'const : (Type_exception.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_module'const : (Module_binding.t, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strmodule'const : (Module_binding.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_recmodule'const : (Module_binding.t list, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strrecmodule'const : (Module_binding.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_modtype'const : (Module_type_declaration.t, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strmodtype'const : (Module_type_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_open'const : (Open_declaration.t, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val stropen'const : (Open_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_class'const : (Class_declaration.t list, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strclass'const : (Class_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_class_type'const : (Class_type_declaration.t list, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strclass_type'const : (Class_type_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_include'const : (Include_declaration.t, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strinclude'const : (Include_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strattribute'const : (Attribute.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pstr_extension'const : ((Extension.t * Attributes.t), 'i, 'o) View.t -> (Structure_item_desc.t, 'i, 'o) View.t

val strextension'const : ((Extension.t * Attributes.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

val pvb_pat'match : (Pattern.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

val pvb_expr'match : (Expression.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

val pvb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

val pvb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

val pmb_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

val pmb_expr'match : (Module_expr.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

val pmb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

val pmb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

val ptop_def'const : (Structure.t, 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t

val ptop_dir'const : (Toplevel_directive.t, 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t

val pdir_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Toplevel_directive.t, 'i, 'o) View.t

val pdir_arg'match : (Directive_argument.t option, 'i, 'o) View.t -> (Toplevel_directive.t, 'i, 'o) View.t

val pdir_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Toplevel_directive.t, 'i, 'o) View.t

val pdira_desc'match : (Directive_argument_desc.t, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t

val pdira_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t

val pdir_string'const : (string, 'i, 'o) View.t -> (Directive_argument_desc.t, 'i, 'o) View.t

val dastring'const : (string, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t

val pdir_int'const : ((string * char option), 'i, 'o) View.t -> (Directive_argument_desc.t, 'i, 'o) View.t

val daint'const : ((string * char option), 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t

val pdir_ident'const : (Longident.t, 'i, 'o) View.t -> (Directive_argument_desc.t, 'i, 'o) View.t

val daident'const : (Longident.t, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t

val pdir_bool'const : (bool, 'i, 'o) View.t -> (Directive_argument_desc.t, 'i, 'o) View.t

val dabool'const : (bool, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
(*$*)
