module Fixed_ocaml : module type of Migrate_parsetree.OCaml_407
module Fixed_ast : module type of Migrate_parsetree.Ast_407
open Fixed_ast


val fixed : string

val make_str
   : string
  -> string Location.loc

val uncapitalize_str
   : string Location.loc
  -> string Location.loc


val make_ident
   : ?modname:string
  -> name:string
  -> unit
  -> Longident.t

val make_ident_loc
   : Location.t
  -> ?modname:string
  -> name:string
  -> unit
  -> Longident.t Location.loc


val make_exp_apply
   : Location.t
  -> Longident.t
  -> Parsetree.expression list
  -> Parsetree.expression

val make_exp_construct
   : Location.t
  -> Longident.t
  -> Parsetree.expression list
  -> Parsetree.expression

val make_exp_ident
   : Location.t
  -> ?modname:string
  -> name:string
  -> unit
  -> Parsetree.expression

val make_exp_fun
   : labelled:bool
  -> param_name:string
  -> Parsetree.expression
  -> Parsetree.expression

val make_exp_funs
   : labelled:bool
  -> param_names:string list
  -> Parsetree.expression
  -> Parsetree.expression

val make_exp_list
   : Parsetree.expression list
  -> Parsetree.expression


val make_pat_construct
   : Location.t
  -> Longident.t
  -> Parsetree.pattern list
  -> Parsetree.pattern

val make_pat_var
   : Location.t
  -> name:string
  -> Parsetree.pattern


val make_typ_arrow
   : Parsetree.core_type
  -> Parsetree.core_type
  -> Parsetree.core_type

val make_typ_arrows
   : Parsetree.core_type list
  -> Parsetree.core_type

val make_typ_constr
   : module_name:string
  -> type_name:string
  -> type_params:Parsetree.core_type list
  -> Parsetree.core_type

val make_typ_tuple
   : Parsetree.core_type list
  -> Parsetree.core_type

val make_typ_var
   : name:string
  -> Parsetree.core_type

val qualify_core_type
   : types:(string * string) list
  -> Parsetree.core_type
  -> Parsetree.core_type
