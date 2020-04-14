(*$ Ppx_ast_cinaps.print_virtual_traverse_ml (Astlib.Version.of_string "unstable_for_testing") *)
open Versions.Unstable_for_testing

class virtual map =
  object (self)
    method virtual bool : bool -> bool
    method virtual char : char -> char
    method virtual int : int -> int
    method virtual list : 'a . ('a -> 'a) -> 'a list -> 'a list
    method virtual option : 'a . ('a -> 'a) -> 'a option -> 'a option
    method virtual string : string -> string
    method virtual location : Astlib.Location.t -> Astlib.Location.t
    method virtual loc : 'a . ('a -> 'a) -> 'a Astlib.Loc.t -> 'a Astlib.Loc.t
    method directive_argument : Directive_argument.t -> Directive_argument.t  =
      fun directive_argument ->
        let concrete = Directive_argument.to_concrete directive_argument in
        match (concrete : Directive_argument.concrete) with
        | Pdir_bool x0 ->
          let x0 = self#bool x0 in
          Directive_argument.of_concrete (Pdir_bool x0)
        | Pdir_ident x0 ->
          let x0 = self#longident x0 in
          Directive_argument.of_concrete (Pdir_ident x0)
        | Pdir_int (x0, x1) ->
          let x0 = self#option self#char x0 in
          let x1 = self#string x1 in
          Directive_argument.of_concrete (Pdir_int (x0, x1))
        | Pdir_string x0 ->
          let x0 = self#string x0 in
          Directive_argument.of_concrete (Pdir_string x0)
        | Pdir_none ->
          Directive_argument.of_concrete Pdir_none
    method toplevel_phrase : Toplevel_phrase.t -> Toplevel_phrase.t  =
      fun toplevel_phrase ->
        let concrete = Toplevel_phrase.to_concrete toplevel_phrase in
        match (concrete : Toplevel_phrase.concrete) with
        | Ptop_dir (x0, x1) ->
          let x0 = self#directive_argument x0 in
          let x1 = self#string x1 in
          Toplevel_phrase.of_concrete (Ptop_dir (x0, x1))
        | Ptop_def x0 ->
          let x0 = self#structure x0 in
          Toplevel_phrase.of_concrete (Ptop_def x0)
    method module_binding : Module_binding.t -> Module_binding.t  =
      fun module_binding ->
        let concrete = Module_binding.to_concrete module_binding in
        let { pmb_loc; pmb_attributes; pmb_expr; pmb_name } : Module_binding.concrete = concrete in
        let pmb_loc = self#location pmb_loc in
        let pmb_attributes = self#attributes pmb_attributes in
        let pmb_expr = self#module_expr pmb_expr in
        let pmb_name = self#loc self#string pmb_name in
        Module_binding.of_concrete { pmb_loc; pmb_attributes; pmb_expr; pmb_name }
    method value_binding : Value_binding.t -> Value_binding.t  =
      fun value_binding ->
        let concrete = Value_binding.to_concrete value_binding in
        let { pvb_loc; pvb_attributes; pvb_expr; pvb_pat } : Value_binding.concrete = concrete in
        let pvb_loc = self#location pvb_loc in
        let pvb_attributes = self#attributes pvb_attributes in
        let pvb_expr = self#expression pvb_expr in
        let pvb_pat = self#pattern pvb_pat in
        Value_binding.of_concrete { pvb_loc; pvb_attributes; pvb_expr; pvb_pat }
    method structure_item_desc : Structure_item_desc.t -> Structure_item_desc.t  =
      fun structure_item_desc ->
        let concrete = Structure_item_desc.to_concrete structure_item_desc in
        match (concrete : Structure_item_desc.concrete) with
        | Pstr_extension (x0, x1) ->
          let x0 = self#attributes x0 in
          let x1 = self#extension x1 in
          Structure_item_desc.of_concrete (Pstr_extension (x0, x1))
        | Pstr_attribute x0 ->
          let x0 = self#attribute x0 in
          Structure_item_desc.of_concrete (Pstr_attribute x0)
        | Pstr_include x0 ->
          let x0 = self#include_declaration x0 in
          Structure_item_desc.of_concrete (Pstr_include x0)
        | Pstr_class_type x0 ->
          let x0 = self#list self#class_type_declaration x0 in
          Structure_item_desc.of_concrete (Pstr_class_type x0)
        | Pstr_class x0 ->
          let x0 = self#list self#class_declaration x0 in
          Structure_item_desc.of_concrete (Pstr_class x0)
        | Pstr_open x0 ->
          let x0 = self#open_description x0 in
          Structure_item_desc.of_concrete (Pstr_open x0)
        | Pstr_modtype x0 ->
          let x0 = self#module_type_declaration x0 in
          Structure_item_desc.of_concrete (Pstr_modtype x0)
        | Pstr_recmodule x0 ->
          let x0 = self#list self#module_binding x0 in
          Structure_item_desc.of_concrete (Pstr_recmodule x0)
        | Pstr_module x0 ->
          let x0 = self#module_binding x0 in
          Structure_item_desc.of_concrete (Pstr_module x0)
        | Pstr_exception x0 ->
          let x0 = self#extension_constructor x0 in
          Structure_item_desc.of_concrete (Pstr_exception x0)
        | Pstr_typext x0 ->
          let x0 = self#type_extension x0 in
          Structure_item_desc.of_concrete (Pstr_typext x0)
        | Pstr_type (x0, x1) ->
          let x0 = self#list self#type_declaration x0 in
          let x1 = self#rec_flag x1 in
          Structure_item_desc.of_concrete (Pstr_type (x0, x1))
        | Pstr_primitive x0 ->
          let x0 = self#value_description x0 in
          Structure_item_desc.of_concrete (Pstr_primitive x0)
        | Pstr_value (x0, x1) ->
          let x0 = self#list self#value_binding x0 in
          let x1 = self#rec_flag x1 in
          Structure_item_desc.of_concrete (Pstr_value (x0, x1))
        | Pstr_eval (x0, x1) ->
          let x0 = self#attributes x0 in
          let x1 = self#expression x1 in
          Structure_item_desc.of_concrete (Pstr_eval (x0, x1))
    method structure_item : Structure_item.t -> Structure_item.t  =
      fun structure_item ->
        let concrete = Structure_item.to_concrete structure_item in
        let { pstr_loc; pstr_desc } : Structure_item.concrete = concrete in
        let pstr_loc = self#location pstr_loc in
        let pstr_desc = self#structure_item_desc pstr_desc in
        Structure_item.of_concrete { pstr_loc; pstr_desc }
    method structure : Structure.t -> Structure.t  =
      fun structure ->
        let concrete = Structure.to_concrete structure in
        let concrete = self#list self#structure_item concrete in
        Structure.of_concrete concrete
    method module_expr_desc : Module_expr_desc.t -> Module_expr_desc.t  =
      fun module_expr_desc ->
        let concrete = Module_expr_desc.to_concrete module_expr_desc in
        match (concrete : Module_expr_desc.concrete) with
        | Pmod_extension x0 ->
          let x0 = self#extension x0 in
          Module_expr_desc.of_concrete (Pmod_extension x0)
        | Pmod_unpack x0 ->
          let x0 = self#expression x0 in
          Module_expr_desc.of_concrete (Pmod_unpack x0)
        | Pmod_constraint (x0, x1) ->
          let x0 = self#module_type x0 in
          let x1 = self#module_expr x1 in
          Module_expr_desc.of_concrete (Pmod_constraint (x0, x1))
        | Pmod_apply (x0, x1) ->
          let x0 = self#module_expr x0 in
          let x1 = self#module_expr x1 in
          Module_expr_desc.of_concrete (Pmod_apply (x0, x1))
        | Pmod_functor (x0, x1, x2) ->
          let x0 = self#module_expr x0 in
          let x1 = self#option self#module_type x1 in
          let x2 = self#loc self#string x2 in
          Module_expr_desc.of_concrete (Pmod_functor (x0, x1, x2))
        | Pmod_structure x0 ->
          let x0 = self#structure x0 in
          Module_expr_desc.of_concrete (Pmod_structure x0)
        | Pmod_ident x0 ->
          let x0 = self#longident_loc x0 in
          Module_expr_desc.of_concrete (Pmod_ident x0)
    method module_expr : Module_expr.t -> Module_expr.t  =
      fun module_expr ->
        let concrete = Module_expr.to_concrete module_expr in
        let { pmod_attributes; pmod_loc; pmod_desc } : Module_expr.concrete = concrete in
        let pmod_attributes = self#attributes pmod_attributes in
        let pmod_loc = self#location pmod_loc in
        let pmod_desc = self#module_expr_desc pmod_desc in
        Module_expr.of_concrete { pmod_attributes; pmod_loc; pmod_desc }
    method with_constraint : With_constraint.t -> With_constraint.t  =
      fun with_constraint ->
        let concrete = With_constraint.to_concrete with_constraint in
        match (concrete : With_constraint.concrete) with
        | Pwith_modsubst (x0, x1) ->
          let x0 = self#longident_loc x0 in
          let x1 = self#longident_loc x1 in
          With_constraint.of_concrete (Pwith_modsubst (x0, x1))
        | Pwith_typesubst (x0, x1) ->
          let x0 = self#type_declaration x0 in
          let x1 = self#longident_loc x1 in
          With_constraint.of_concrete (Pwith_typesubst (x0, x1))
        | Pwith_module (x0, x1) ->
          let x0 = self#longident_loc x0 in
          let x1 = self#longident_loc x1 in
          With_constraint.of_concrete (Pwith_module (x0, x1))
        | Pwith_type (x0, x1) ->
          let x0 = self#type_declaration x0 in
          let x1 = self#longident_loc x1 in
          With_constraint.of_concrete (Pwith_type (x0, x1))
    method include_declaration : Include_declaration.t -> Include_declaration.t  =
      fun include_declaration ->
        let concrete = Include_declaration.to_concrete include_declaration in
        let concrete = self#include_infos_module_expr concrete in
        Include_declaration.of_concrete concrete
    method include_description : Include_description.t -> Include_description.t  =
      fun include_description ->
        let concrete = Include_description.to_concrete include_description in
        let concrete = self#include_infos_module_type concrete in
        Include_description.of_concrete concrete
    method include_infos_module_expr : Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t  =
      fun include_infos ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_expr.t Include_infos.concrete = concrete in
        let pincl_attributes = self#attributes pincl_attributes in
        let pincl_loc = self#location pincl_loc in
        let pincl_mod = self#module_expr pincl_mod in
        Include_infos.of_concrete { pincl_attributes; pincl_loc; pincl_mod }
    method include_infos_module_type : Module_type.t Include_infos.t -> Module_type.t Include_infos.t  =
      fun include_infos ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_type.t Include_infos.concrete = concrete in
        let pincl_attributes = self#attributes pincl_attributes in
        let pincl_loc = self#location pincl_loc in
        let pincl_mod = self#module_type pincl_mod in
        Include_infos.of_concrete { pincl_attributes; pincl_loc; pincl_mod }
    method open_description : Open_description.t -> Open_description.t  =
      fun open_description ->
        let concrete = Open_description.to_concrete open_description in
        let { popen_attributes; popen_loc; popen_override; popen_lid } : Open_description.concrete = concrete in
        let popen_attributes = self#attributes popen_attributes in
        let popen_loc = self#location popen_loc in
        let popen_override = self#override_flag popen_override in
        let popen_lid = self#longident_loc popen_lid in
        Open_description.of_concrete { popen_attributes; popen_loc; popen_override; popen_lid }
    method module_type_declaration : Module_type_declaration.t -> Module_type_declaration.t  =
      fun module_type_declaration ->
        let concrete = Module_type_declaration.to_concrete module_type_declaration in
        let { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name } : Module_type_declaration.concrete = concrete in
        let pmtd_loc = self#location pmtd_loc in
        let pmtd_attributes = self#attributes pmtd_attributes in
        let pmtd_type = self#option self#module_type pmtd_type in
        let pmtd_name = self#loc self#string pmtd_name in
        Module_type_declaration.of_concrete { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name }
    method module_declaration : Module_declaration.t -> Module_declaration.t  =
      fun module_declaration ->
        let concrete = Module_declaration.to_concrete module_declaration in
        let { pmd_loc; pmd_attributes; pmd_type; pmd_name } : Module_declaration.concrete = concrete in
        let pmd_loc = self#location pmd_loc in
        let pmd_attributes = self#attributes pmd_attributes in
        let pmd_type = self#module_type pmd_type in
        let pmd_name = self#loc self#string pmd_name in
        Module_declaration.of_concrete { pmd_loc; pmd_attributes; pmd_type; pmd_name }
    method signature_item_desc : Signature_item_desc.t -> Signature_item_desc.t  =
      fun signature_item_desc ->
        let concrete = Signature_item_desc.to_concrete signature_item_desc in
        match (concrete : Signature_item_desc.concrete) with
        | Psig_extension (x0, x1) ->
          let x0 = self#attributes x0 in
          let x1 = self#extension x1 in
          Signature_item_desc.of_concrete (Psig_extension (x0, x1))
        | Psig_attribute x0 ->
          let x0 = self#attribute x0 in
          Signature_item_desc.of_concrete (Psig_attribute x0)
        | Psig_class_type x0 ->
          let x0 = self#list self#class_type_declaration x0 in
          Signature_item_desc.of_concrete (Psig_class_type x0)
        | Psig_class x0 ->
          let x0 = self#list self#class_description x0 in
          Signature_item_desc.of_concrete (Psig_class x0)
        | Psig_include x0 ->
          let x0 = self#include_description x0 in
          Signature_item_desc.of_concrete (Psig_include x0)
        | Psig_open x0 ->
          let x0 = self#open_description x0 in
          Signature_item_desc.of_concrete (Psig_open x0)
        | Psig_modtype x0 ->
          let x0 = self#module_type_declaration x0 in
          Signature_item_desc.of_concrete (Psig_modtype x0)
        | Psig_recmodule x0 ->
          let x0 = self#list self#module_declaration x0 in
          Signature_item_desc.of_concrete (Psig_recmodule x0)
        | Psig_module x0 ->
          let x0 = self#module_declaration x0 in
          Signature_item_desc.of_concrete (Psig_module x0)
        | Psig_exception x0 ->
          let x0 = self#extension_constructor x0 in
          Signature_item_desc.of_concrete (Psig_exception x0)
        | Psig_typext x0 ->
          let x0 = self#type_extension x0 in
          Signature_item_desc.of_concrete (Psig_typext x0)
        | Psig_type (x0, x1) ->
          let x0 = self#list self#type_declaration x0 in
          let x1 = self#rec_flag x1 in
          Signature_item_desc.of_concrete (Psig_type (x0, x1))
        | Psig_value x0 ->
          let x0 = self#value_description x0 in
          Signature_item_desc.of_concrete (Psig_value x0)
    method signature_item : Signature_item.t -> Signature_item.t  =
      fun signature_item ->
        let concrete = Signature_item.to_concrete signature_item in
        let { psig_loc; psig_desc } : Signature_item.concrete = concrete in
        let psig_loc = self#location psig_loc in
        let psig_desc = self#signature_item_desc psig_desc in
        Signature_item.of_concrete { psig_loc; psig_desc }
    method signature : Signature.t -> Signature.t  =
      fun signature ->
        let concrete = Signature.to_concrete signature in
        let concrete = self#list self#signature_item concrete in
        Signature.of_concrete concrete
    method module_type_desc : Module_type_desc.t -> Module_type_desc.t  =
      fun module_type_desc ->
        let concrete = Module_type_desc.to_concrete module_type_desc in
        match (concrete : Module_type_desc.concrete) with
        | Pmty_alias x0 ->
          let x0 = self#longident_loc x0 in
          Module_type_desc.of_concrete (Pmty_alias x0)
        | Pmty_extension x0 ->
          let x0 = self#extension x0 in
          Module_type_desc.of_concrete (Pmty_extension x0)
        | Pmty_typeof x0 ->
          let x0 = self#module_expr x0 in
          Module_type_desc.of_concrete (Pmty_typeof x0)
        | Pmty_with (x0, x1) ->
          let x0 = self#list self#with_constraint x0 in
          let x1 = self#module_type x1 in
          Module_type_desc.of_concrete (Pmty_with (x0, x1))
        | Pmty_functor (x0, x1, x2) ->
          let x0 = self#module_type x0 in
          let x1 = self#option self#module_type x1 in
          let x2 = self#loc self#string x2 in
          Module_type_desc.of_concrete (Pmty_functor (x0, x1, x2))
        | Pmty_signature x0 ->
          let x0 = self#signature x0 in
          Module_type_desc.of_concrete (Pmty_signature x0)
        | Pmty_ident x0 ->
          let x0 = self#longident_loc x0 in
          Module_type_desc.of_concrete (Pmty_ident x0)
    method module_type : Module_type.t -> Module_type.t  =
      fun module_type ->
        let concrete = Module_type.to_concrete module_type in
        let { pmty_attributes; pmty_loc; pmty_desc } : Module_type.concrete = concrete in
        let pmty_attributes = self#attributes pmty_attributes in
        let pmty_loc = self#location pmty_loc in
        let pmty_desc = self#module_type_desc pmty_desc in
        Module_type.of_concrete { pmty_attributes; pmty_loc; pmty_desc }
    method class_declaration : Class_declaration.t -> Class_declaration.t  =
      fun class_declaration ->
        let concrete = Class_declaration.to_concrete class_declaration in
        let concrete = self#class_infos_class_expr concrete in
        Class_declaration.of_concrete concrete
    method class_field_kind : Class_field_kind.t -> Class_field_kind.t  =
      fun class_field_kind ->
        let concrete = Class_field_kind.to_concrete class_field_kind in
        match (concrete : Class_field_kind.concrete) with
        | Cfk_concrete (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#override_flag x1 in
          Class_field_kind.of_concrete (Cfk_concrete (x0, x1))
        | Cfk_virtual x0 ->
          let x0 = self#core_type x0 in
          Class_field_kind.of_concrete (Cfk_virtual x0)
    method class_field_desc : Class_field_desc.t -> Class_field_desc.t  =
      fun class_field_desc ->
        let concrete = Class_field_desc.to_concrete class_field_desc in
        match (concrete : Class_field_desc.concrete) with
        | Pcf_extension x0 ->
          let x0 = self#extension x0 in
          Class_field_desc.of_concrete (Pcf_extension x0)
        | Pcf_attribute x0 ->
          let x0 = self#attribute x0 in
          Class_field_desc.of_concrete (Pcf_attribute x0)
        | Pcf_initializer x0 ->
          let x0 = self#expression x0 in
          Class_field_desc.of_concrete (Pcf_initializer x0)
        | Pcf_constraint x0 ->
          let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in (x0, x1)) x0 in
          Class_field_desc.of_concrete (Pcf_constraint x0)
        | Pcf_method x0 ->
          let x0 = (fun (x0, x1, x2) -> let x0 = self#class_field_kind x0 in let x1 = self#private_flag x1 in let x2 = self#loc self#string x2 in (x0, x1, x2)) x0 in
          Class_field_desc.of_concrete (Pcf_method x0)
        | Pcf_val x0 ->
          let x0 = (fun (x0, x1, x2) -> let x0 = self#class_field_kind x0 in let x1 = self#mutable_flag x1 in let x2 = self#loc self#string x2 in (x0, x1, x2)) x0 in
          Class_field_desc.of_concrete (Pcf_val x0)
        | Pcf_inherit (x0, x1, x2) ->
          let x0 = self#option (self#loc self#string) x0 in
          let x1 = self#class_expr x1 in
          let x2 = self#override_flag x2 in
          Class_field_desc.of_concrete (Pcf_inherit (x0, x1, x2))
    method class_field : Class_field.t -> Class_field.t  =
      fun class_field ->
        let concrete = Class_field.to_concrete class_field in
        let { pcf_attributes; pcf_loc; pcf_desc } : Class_field.concrete = concrete in
        let pcf_attributes = self#attributes pcf_attributes in
        let pcf_loc = self#location pcf_loc in
        let pcf_desc = self#class_field_desc pcf_desc in
        Class_field.of_concrete { pcf_attributes; pcf_loc; pcf_desc }
    method class_structure : Class_structure.t -> Class_structure.t  =
      fun class_structure ->
        let concrete = Class_structure.to_concrete class_structure in
        let { pcstr_fields; pcstr_self } : Class_structure.concrete = concrete in
        let pcstr_fields = self#list self#class_field pcstr_fields in
        let pcstr_self = self#pattern pcstr_self in
        Class_structure.of_concrete { pcstr_fields; pcstr_self }
    method class_expr_desc : Class_expr_desc.t -> Class_expr_desc.t  =
      fun class_expr_desc ->
        let concrete = Class_expr_desc.to_concrete class_expr_desc in
        match (concrete : Class_expr_desc.concrete) with
        | Pcl_open (x0, x1, x2) ->
          let x0 = self#class_expr x0 in
          let x1 = self#longident_loc x1 in
          let x2 = self#override_flag x2 in
          Class_expr_desc.of_concrete (Pcl_open (x0, x1, x2))
        | Pcl_extension x0 ->
          let x0 = self#extension x0 in
          Class_expr_desc.of_concrete (Pcl_extension x0)
        | Pcl_constraint (x0, x1) ->
          let x0 = self#class_type x0 in
          let x1 = self#class_expr x1 in
          Class_expr_desc.of_concrete (Pcl_constraint (x0, x1))
        | Pcl_let (x0, x1, x2) ->
          let x0 = self#class_expr x0 in
          let x1 = self#list self#value_binding x1 in
          let x2 = self#rec_flag x2 in
          Class_expr_desc.of_concrete (Pcl_let (x0, x1, x2))
        | Pcl_apply (x0, x1) ->
          let x0 = self#list (fun (x0, x1) -> let x0 = self#expression x0 in let x1 = self#arg_label x1 in (x0, x1)) x0 in
          let x1 = self#class_expr x1 in
          Class_expr_desc.of_concrete (Pcl_apply (x0, x1))
        | Pcl_fun (x0, x1, x2, x3) ->
          let x0 = self#class_expr x0 in
          let x1 = self#pattern x1 in
          let x2 = self#option self#expression x2 in
          let x3 = self#arg_label x3 in
          Class_expr_desc.of_concrete (Pcl_fun (x0, x1, x2, x3))
        | Pcl_structure x0 ->
          let x0 = self#class_structure x0 in
          Class_expr_desc.of_concrete (Pcl_structure x0)
        | Pcl_constr (x0, x1) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#longident_loc x1 in
          Class_expr_desc.of_concrete (Pcl_constr (x0, x1))
    method class_expr : Class_expr.t -> Class_expr.t  =
      fun class_expr ->
        let concrete = Class_expr.to_concrete class_expr in
        let { pcl_attributes; pcl_loc; pcl_desc } : Class_expr.concrete = concrete in
        let pcl_attributes = self#attributes pcl_attributes in
        let pcl_loc = self#location pcl_loc in
        let pcl_desc = self#class_expr_desc pcl_desc in
        Class_expr.of_concrete { pcl_attributes; pcl_loc; pcl_desc }
    method class_type_declaration : Class_type_declaration.t -> Class_type_declaration.t  =
      fun class_type_declaration ->
        let concrete = Class_type_declaration.to_concrete class_type_declaration in
        let concrete = self#class_infos_class_type concrete in
        Class_type_declaration.of_concrete concrete
    method class_description : Class_description.t -> Class_description.t  =
      fun class_description ->
        let concrete = Class_description.to_concrete class_description in
        let concrete = self#class_infos_class_type concrete in
        Class_description.of_concrete concrete
    method class_infos_class_expr : Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t  =
      fun class_infos ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_expr.t Class_infos.concrete = concrete in
        let pci_attributes = self#attributes pci_attributes in
        let pci_loc = self#location pci_loc in
        let pci_expr = self#class_expr pci_expr in
        let pci_name = self#loc self#string pci_name in
        let pci_params = self#list (fun (x0, x1) -> let x0 = self#variance x0 in let x1 = self#core_type x1 in (x0, x1)) pci_params in
        let pci_virt = self#virtual_flag pci_virt in
        Class_infos.of_concrete { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt }
    method class_infos_class_type : Class_type.t Class_infos.t -> Class_type.t Class_infos.t  =
      fun class_infos ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_type.t Class_infos.concrete = concrete in
        let pci_attributes = self#attributes pci_attributes in
        let pci_loc = self#location pci_loc in
        let pci_expr = self#class_type pci_expr in
        let pci_name = self#loc self#string pci_name in
        let pci_params = self#list (fun (x0, x1) -> let x0 = self#variance x0 in let x1 = self#core_type x1 in (x0, x1)) pci_params in
        let pci_virt = self#virtual_flag pci_virt in
        Class_infos.of_concrete { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt }
    method class_type_field_desc : Class_type_field_desc.t -> Class_type_field_desc.t  =
      fun class_type_field_desc ->
        let concrete = Class_type_field_desc.to_concrete class_type_field_desc in
        match (concrete : Class_type_field_desc.concrete) with
        | Pctf_extension x0 ->
          let x0 = self#extension x0 in
          Class_type_field_desc.of_concrete (Pctf_extension x0)
        | Pctf_attribute x0 ->
          let x0 = self#attribute x0 in
          Class_type_field_desc.of_concrete (Pctf_attribute x0)
        | Pctf_constraint x0 ->
          let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in (x0, x1)) x0 in
          Class_type_field_desc.of_concrete (Pctf_constraint x0)
        | Pctf_method x0 ->
          let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#core_type x0 in let x1 = self#virtual_flag x1 in let x2 = self#private_flag x2 in let x3 = self#loc self#string x3 in (x0, x1, x2, x3)) x0 in
          Class_type_field_desc.of_concrete (Pctf_method x0)
        | Pctf_val x0 ->
          let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#core_type x0 in let x1 = self#virtual_flag x1 in let x2 = self#mutable_flag x2 in let x3 = self#loc self#string x3 in (x0, x1, x2, x3)) x0 in
          Class_type_field_desc.of_concrete (Pctf_val x0)
        | Pctf_inherit x0 ->
          let x0 = self#class_type x0 in
          Class_type_field_desc.of_concrete (Pctf_inherit x0)
    method class_type_field : Class_type_field.t -> Class_type_field.t  =
      fun class_type_field ->
        let concrete = Class_type_field.to_concrete class_type_field in
        let { pctf_attributes; pctf_loc; pctf_desc } : Class_type_field.concrete = concrete in
        let pctf_attributes = self#attributes pctf_attributes in
        let pctf_loc = self#location pctf_loc in
        let pctf_desc = self#class_type_field_desc pctf_desc in
        Class_type_field.of_concrete { pctf_attributes; pctf_loc; pctf_desc }
    method class_signature : Class_signature.t -> Class_signature.t  =
      fun class_signature ->
        let concrete = Class_signature.to_concrete class_signature in
        let { pcsig_fields; pcsig_self } : Class_signature.concrete = concrete in
        let pcsig_fields = self#list self#class_type_field pcsig_fields in
        let pcsig_self = self#core_type pcsig_self in
        Class_signature.of_concrete { pcsig_fields; pcsig_self }
    method class_type_desc : Class_type_desc.t -> Class_type_desc.t  =
      fun class_type_desc ->
        let concrete = Class_type_desc.to_concrete class_type_desc in
        match (concrete : Class_type_desc.concrete) with
        | Pcty_open (x0, x1, x2) ->
          let x0 = self#class_type x0 in
          let x1 = self#longident_loc x1 in
          let x2 = self#override_flag x2 in
          Class_type_desc.of_concrete (Pcty_open (x0, x1, x2))
        | Pcty_extension x0 ->
          let x0 = self#extension x0 in
          Class_type_desc.of_concrete (Pcty_extension x0)
        | Pcty_arrow (x0, x1, x2) ->
          let x0 = self#class_type x0 in
          let x1 = self#core_type x1 in
          let x2 = self#arg_label x2 in
          Class_type_desc.of_concrete (Pcty_arrow (x0, x1, x2))
        | Pcty_signature x0 ->
          let x0 = self#class_signature x0 in
          Class_type_desc.of_concrete (Pcty_signature x0)
        | Pcty_constr (x0, x1) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#longident_loc x1 in
          Class_type_desc.of_concrete (Pcty_constr (x0, x1))
    method class_type : Class_type.t -> Class_type.t  =
      fun class_type ->
        let concrete = Class_type.to_concrete class_type in
        let { pcty_attributes; pcty_loc; pcty_desc } : Class_type.concrete = concrete in
        let pcty_attributes = self#attributes pcty_attributes in
        let pcty_loc = self#location pcty_loc in
        let pcty_desc = self#class_type_desc pcty_desc in
        Class_type.of_concrete { pcty_attributes; pcty_loc; pcty_desc }
    method extension_constructor_kind : Extension_constructor_kind.t -> Extension_constructor_kind.t  =
      fun extension_constructor_kind ->
        let concrete = Extension_constructor_kind.to_concrete extension_constructor_kind in
        match (concrete : Extension_constructor_kind.concrete) with
        | Pext_rebind x0 ->
          let x0 = self#longident_loc x0 in
          Extension_constructor_kind.of_concrete (Pext_rebind x0)
        | Pext_decl (x0, x1) ->
          let x0 = self#option self#core_type x0 in
          let x1 = self#constructor_arguments x1 in
          Extension_constructor_kind.of_concrete (Pext_decl (x0, x1))
    method extension_constructor : Extension_constructor.t -> Extension_constructor.t  =
      fun extension_constructor ->
        let concrete = Extension_constructor.to_concrete extension_constructor in
        let { pext_attributes; pext_loc; pext_kind; pext_name } : Extension_constructor.concrete = concrete in
        let pext_attributes = self#attributes pext_attributes in
        let pext_loc = self#location pext_loc in
        let pext_kind = self#extension_constructor_kind pext_kind in
        let pext_name = self#loc self#string pext_name in
        Extension_constructor.of_concrete { pext_attributes; pext_loc; pext_kind; pext_name }
    method type_extension : Type_extension.t -> Type_extension.t  =
      fun type_extension ->
        let concrete = Type_extension.to_concrete type_extension in
        let { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path } : Type_extension.concrete = concrete in
        let ptyext_attributes = self#attributes ptyext_attributes in
        let ptyext_private = self#private_flag ptyext_private in
        let ptyext_constructors = self#list self#extension_constructor ptyext_constructors in
        let ptyext_params = self#list (fun (x0, x1) -> let x0 = self#variance x0 in let x1 = self#core_type x1 in (x0, x1)) ptyext_params in
        let ptyext_path = self#longident_loc ptyext_path in
        Type_extension.of_concrete { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path }
    method constructor_arguments : Constructor_arguments.t -> Constructor_arguments.t  =
      fun constructor_arguments ->
        let concrete = Constructor_arguments.to_concrete constructor_arguments in
        match (concrete : Constructor_arguments.concrete) with
        | Pcstr_record x0 ->
          let x0 = self#list self#label_declaration x0 in
          Constructor_arguments.of_concrete (Pcstr_record x0)
        | Pcstr_tuple x0 ->
          let x0 = self#list self#core_type x0 in
          Constructor_arguments.of_concrete (Pcstr_tuple x0)
    method constructor_declaration : Constructor_declaration.t -> Constructor_declaration.t  =
      fun constructor_declaration ->
        let concrete = Constructor_declaration.to_concrete constructor_declaration in
        let { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name } : Constructor_declaration.concrete = concrete in
        let pcd_attributes = self#attributes pcd_attributes in
        let pcd_loc = self#location pcd_loc in
        let pcd_res = self#option self#core_type pcd_res in
        let pcd_args = self#constructor_arguments pcd_args in
        let pcd_name = self#loc self#string pcd_name in
        Constructor_declaration.of_concrete { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name }
    method label_declaration : Label_declaration.t -> Label_declaration.t  =
      fun label_declaration ->
        let concrete = Label_declaration.to_concrete label_declaration in
        let { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name } : Label_declaration.concrete = concrete in
        let pld_attributes = self#attributes pld_attributes in
        let pld_loc = self#location pld_loc in
        let pld_type = self#core_type pld_type in
        let pld_mutable = self#mutable_flag pld_mutable in
        let pld_name = self#loc self#string pld_name in
        Label_declaration.of_concrete { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name }
    method type_kind : Type_kind.t -> Type_kind.t  =
      fun type_kind ->
        let concrete = Type_kind.to_concrete type_kind in
        match (concrete : Type_kind.concrete) with
        | Ptype_open ->
          Type_kind.of_concrete Ptype_open
        | Ptype_record x0 ->
          let x0 = self#list self#label_declaration x0 in
          Type_kind.of_concrete (Ptype_record x0)
        | Ptype_variant x0 ->
          let x0 = self#list self#constructor_declaration x0 in
          Type_kind.of_concrete (Ptype_variant x0)
        | Ptype_abstract ->
          Type_kind.of_concrete Ptype_abstract
    method type_declaration : Type_declaration.t -> Type_declaration.t  =
      fun type_declaration ->
        let concrete = Type_declaration.to_concrete type_declaration in
        let { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name } : Type_declaration.concrete = concrete in
        let ptype_loc = self#location ptype_loc in
        let ptype_attributes = self#attributes ptype_attributes in
        let ptype_manifest = self#option self#core_type ptype_manifest in
        let ptype_private = self#private_flag ptype_private in
        let ptype_kind = self#type_kind ptype_kind in
        let ptype_cstrs = self#list (fun (x0, x1, x2) -> let x0 = self#location x0 in let x1 = self#core_type x1 in let x2 = self#core_type x2 in (x0, x1, x2)) ptype_cstrs in
        let ptype_params = self#list (fun (x0, x1) -> let x0 = self#variance x0 in let x1 = self#core_type x1 in (x0, x1)) ptype_params in
        let ptype_name = self#loc self#string ptype_name in
        Type_declaration.of_concrete { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name }
    method value_description : Value_description.t -> Value_description.t  =
      fun value_description ->
        let concrete = Value_description.to_concrete value_description in
        let { pval_loc; pval_attributes; pval_prim; pval_type; pval_name } : Value_description.concrete = concrete in
        let pval_loc = self#location pval_loc in
        let pval_attributes = self#attributes pval_attributes in
        let pval_prim = self#list self#string pval_prim in
        let pval_type = self#core_type pval_type in
        let pval_name = self#loc self#string pval_name in
        Value_description.of_concrete { pval_loc; pval_attributes; pval_prim; pval_type; pval_name }
    method case : Case.t -> Case.t  =
      fun case ->
        let concrete = Case.to_concrete case in
        let { pc_rhs; pc_guard; pc_lhs } : Case.concrete = concrete in
        let pc_rhs = self#expression pc_rhs in
        let pc_guard = self#option self#expression pc_guard in
        let pc_lhs = self#pattern pc_lhs in
        Case.of_concrete { pc_rhs; pc_guard; pc_lhs }
    method expression_desc : Expression_desc.t -> Expression_desc.t  =
      fun expression_desc ->
        let concrete = Expression_desc.to_concrete expression_desc in
        match (concrete : Expression_desc.concrete) with
        | Pexp_unreachable ->
          Expression_desc.of_concrete Pexp_unreachable
        | Pexp_extension x0 ->
          let x0 = self#extension x0 in
          Expression_desc.of_concrete (Pexp_extension x0)
        | Pexp_open (x0, x1, x2) ->
          let x0 = self#expression x0 in
          let x1 = self#longident_loc x1 in
          let x2 = self#override_flag x2 in
          Expression_desc.of_concrete (Pexp_open (x0, x1, x2))
        | Pexp_pack x0 ->
          let x0 = self#module_expr x0 in
          Expression_desc.of_concrete (Pexp_pack x0)
        | Pexp_newtype (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#loc self#string x1 in
          Expression_desc.of_concrete (Pexp_newtype (x0, x1))
        | Pexp_object x0 ->
          let x0 = self#class_structure x0 in
          Expression_desc.of_concrete (Pexp_object x0)
        | Pexp_poly (x0, x1) ->
          let x0 = self#option self#core_type x0 in
          let x1 = self#expression x1 in
          Expression_desc.of_concrete (Pexp_poly (x0, x1))
        | Pexp_lazy x0 ->
          let x0 = self#expression x0 in
          Expression_desc.of_concrete (Pexp_lazy x0)
        | Pexp_assert x0 ->
          let x0 = self#expression x0 in
          Expression_desc.of_concrete (Pexp_assert x0)
        | Pexp_letexception (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#extension_constructor x1 in
          Expression_desc.of_concrete (Pexp_letexception (x0, x1))
        | Pexp_letmodule (x0, x1, x2) ->
          let x0 = self#expression x0 in
          let x1 = self#module_expr x1 in
          let x2 = self#loc self#string x2 in
          Expression_desc.of_concrete (Pexp_letmodule (x0, x1, x2))
        | Pexp_override x0 ->
          let x0 = self#list (fun (x0, x1) -> let x0 = self#expression x0 in let x1 = self#loc self#string x1 in (x0, x1)) x0 in
          Expression_desc.of_concrete (Pexp_override x0)
        | Pexp_setinstvar (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#loc self#string x1 in
          Expression_desc.of_concrete (Pexp_setinstvar (x0, x1))
        | Pexp_new x0 ->
          let x0 = self#longident_loc x0 in
          Expression_desc.of_concrete (Pexp_new x0)
        | Pexp_send (x0, x1) ->
          let x0 = self#loc self#string x0 in
          let x1 = self#expression x1 in
          Expression_desc.of_concrete (Pexp_send (x0, x1))
        | Pexp_coerce (x0, x1, x2) ->
          let x0 = self#core_type x0 in
          let x1 = self#option self#core_type x1 in
          let x2 = self#expression x2 in
          Expression_desc.of_concrete (Pexp_coerce (x0, x1, x2))
        | Pexp_constraint (x0, x1) ->
          let x0 = self#core_type x0 in
          let x1 = self#expression x1 in
          Expression_desc.of_concrete (Pexp_constraint (x0, x1))
        | Pexp_for (x0, x1, x2, x3, x4) ->
          let x0 = self#expression x0 in
          let x1 = self#direction_flag x1 in
          let x2 = self#expression x2 in
          let x3 = self#expression x3 in
          let x4 = self#pattern x4 in
          Expression_desc.of_concrete (Pexp_for (x0, x1, x2, x3, x4))
        | Pexp_while (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#expression x1 in
          Expression_desc.of_concrete (Pexp_while (x0, x1))
        | Pexp_sequence (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#expression x1 in
          Expression_desc.of_concrete (Pexp_sequence (x0, x1))
        | Pexp_ifthenelse (x0, x1, x2) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#expression x1 in
          let x2 = self#expression x2 in
          Expression_desc.of_concrete (Pexp_ifthenelse (x0, x1, x2))
        | Pexp_array x0 ->
          let x0 = self#list self#expression x0 in
          Expression_desc.of_concrete (Pexp_array x0)
        | Pexp_setfield (x0, x1, x2) ->
          let x0 = self#expression x0 in
          let x1 = self#longident_loc x1 in
          let x2 = self#expression x2 in
          Expression_desc.of_concrete (Pexp_setfield (x0, x1, x2))
        | Pexp_field (x0, x1) ->
          let x0 = self#longident_loc x0 in
          let x1 = self#expression x1 in
          Expression_desc.of_concrete (Pexp_field (x0, x1))
        | Pexp_record (x0, x1) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#list (fun (x0, x1) -> let x0 = self#expression x0 in let x1 = self#longident_loc x1 in (x0, x1)) x1 in
          Expression_desc.of_concrete (Pexp_record (x0, x1))
        | Pexp_variant (x0, x1) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#string x1 in
          Expression_desc.of_concrete (Pexp_variant (x0, x1))
        | Pexp_construct (x0, x1) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#longident_loc x1 in
          Expression_desc.of_concrete (Pexp_construct (x0, x1))
        | Pexp_tuple x0 ->
          let x0 = self#list self#expression x0 in
          Expression_desc.of_concrete (Pexp_tuple x0)
        | Pexp_try (x0, x1) ->
          let x0 = self#list self#case x0 in
          let x1 = self#expression x1 in
          Expression_desc.of_concrete (Pexp_try (x0, x1))
        | Pexp_match (x0, x1) ->
          let x0 = self#list self#case x0 in
          let x1 = self#expression x1 in
          Expression_desc.of_concrete (Pexp_match (x0, x1))
        | Pexp_apply (x0, x1) ->
          let x0 = self#list (fun (x0, x1) -> let x0 = self#expression x0 in let x1 = self#arg_label x1 in (x0, x1)) x0 in
          let x1 = self#expression x1 in
          Expression_desc.of_concrete (Pexp_apply (x0, x1))
        | Pexp_fun (x0, x1, x2, x3) ->
          let x0 = self#expression x0 in
          let x1 = self#pattern x1 in
          let x2 = self#option self#expression x2 in
          let x3 = self#arg_label x3 in
          Expression_desc.of_concrete (Pexp_fun (x0, x1, x2, x3))
        | Pexp_function x0 ->
          let x0 = self#list self#case x0 in
          Expression_desc.of_concrete (Pexp_function x0)
        | Pexp_let (x0, x1, x2) ->
          let x0 = self#expression x0 in
          let x1 = self#list self#value_binding x1 in
          let x2 = self#rec_flag x2 in
          Expression_desc.of_concrete (Pexp_let (x0, x1, x2))
        | Pexp_constant x0 ->
          let x0 = self#constant x0 in
          Expression_desc.of_concrete (Pexp_constant x0)
        | Pexp_ident x0 ->
          let x0 = self#longident_loc x0 in
          Expression_desc.of_concrete (Pexp_ident x0)
    method expression : Expression.t -> Expression.t  =
      fun expression ->
        let concrete = Expression.to_concrete expression in
        let { pexp_attributes; pexp_loc; pexp_desc } : Expression.concrete = concrete in
        let pexp_attributes = self#attributes pexp_attributes in
        let pexp_loc = self#location pexp_loc in
        let pexp_desc = self#expression_desc pexp_desc in
        Expression.of_concrete { pexp_attributes; pexp_loc; pexp_desc }
    method pattern_desc : Pattern_desc.t -> Pattern_desc.t  =
      fun pattern_desc ->
        let concrete = Pattern_desc.to_concrete pattern_desc in
        match (concrete : Pattern_desc.concrete) with
        | Ppat_open (x0, x1) ->
          let x0 = self#pattern x0 in
          let x1 = self#longident_loc x1 in
          Pattern_desc.of_concrete (Ppat_open (x0, x1))
        | Ppat_extension x0 ->
          let x0 = self#extension x0 in
          Pattern_desc.of_concrete (Ppat_extension x0)
        | Ppat_exception x0 ->
          let x0 = self#pattern x0 in
          Pattern_desc.of_concrete (Ppat_exception x0)
        | Ppat_unpack x0 ->
          let x0 = self#loc self#string x0 in
          Pattern_desc.of_concrete (Ppat_unpack x0)
        | Ppat_lazy x0 ->
          let x0 = self#pattern x0 in
          Pattern_desc.of_concrete (Ppat_lazy x0)
        | Ppat_type x0 ->
          let x0 = self#longident_loc x0 in
          Pattern_desc.of_concrete (Ppat_type x0)
        | Ppat_constraint (x0, x1) ->
          let x0 = self#core_type x0 in
          let x1 = self#pattern x1 in
          Pattern_desc.of_concrete (Ppat_constraint (x0, x1))
        | Ppat_or (x0, x1) ->
          let x0 = self#pattern x0 in
          let x1 = self#pattern x1 in
          Pattern_desc.of_concrete (Ppat_or (x0, x1))
        | Ppat_array x0 ->
          let x0 = self#list self#pattern x0 in
          Pattern_desc.of_concrete (Ppat_array x0)
        | Ppat_record (x0, x1) ->
          let x0 = self#closed_flag x0 in
          let x1 = self#list (fun (x0, x1) -> let x0 = self#pattern x0 in let x1 = self#longident_loc x1 in (x0, x1)) x1 in
          Pattern_desc.of_concrete (Ppat_record (x0, x1))
        | Ppat_variant (x0, x1) ->
          let x0 = self#option self#pattern x0 in
          let x1 = self#string x1 in
          Pattern_desc.of_concrete (Ppat_variant (x0, x1))
        | Ppat_construct (x0, x1) ->
          let x0 = self#option self#pattern x0 in
          let x1 = self#longident_loc x1 in
          Pattern_desc.of_concrete (Ppat_construct (x0, x1))
        | Ppat_tuple x0 ->
          let x0 = self#list self#pattern x0 in
          Pattern_desc.of_concrete (Ppat_tuple x0)
        | Ppat_interval (x0, x1) ->
          let x0 = self#constant x0 in
          let x1 = self#constant x1 in
          Pattern_desc.of_concrete (Ppat_interval (x0, x1))
        | Ppat_constant x0 ->
          let x0 = self#constant x0 in
          Pattern_desc.of_concrete (Ppat_constant x0)
        | Ppat_alias (x0, x1) ->
          let x0 = self#loc self#string x0 in
          let x1 = self#pattern x1 in
          Pattern_desc.of_concrete (Ppat_alias (x0, x1))
        | Ppat_var x0 ->
          let x0 = self#loc self#string x0 in
          Pattern_desc.of_concrete (Ppat_var x0)
        | Ppat_any ->
          Pattern_desc.of_concrete Ppat_any
    method pattern : Pattern.t -> Pattern.t  =
      fun pattern ->
        let concrete = Pattern.to_concrete pattern in
        let { ppat_attributes; ppat_loc; ppat_desc } : Pattern.concrete = concrete in
        let ppat_attributes = self#attributes ppat_attributes in
        let ppat_loc = self#location ppat_loc in
        let ppat_desc = self#pattern_desc ppat_desc in
        Pattern.of_concrete { ppat_attributes; ppat_loc; ppat_desc }
    method object_field : Object_field.t -> Object_field.t  =
      fun object_field ->
        let concrete = Object_field.to_concrete object_field in
        match (concrete : Object_field.concrete) with
        | Oinherit x0 ->
          let x0 = self#core_type x0 in
          Object_field.of_concrete (Oinherit x0)
        | Otag (x0, x1, x2) ->
          let x0 = self#core_type x0 in
          let x1 = self#attributes x1 in
          let x2 = self#loc self#string x2 in
          Object_field.of_concrete (Otag (x0, x1, x2))
    method row_field : Row_field.t -> Row_field.t  =
      fun row_field ->
        let concrete = Row_field.to_concrete row_field in
        match (concrete : Row_field.concrete) with
        | Rinherit x0 ->
          let x0 = self#core_type x0 in
          Row_field.of_concrete (Rinherit x0)
        | Rtag (x0, x1, x2, x3) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#bool x1 in
          let x2 = self#attributes x2 in
          let x3 = self#loc self#string x3 in
          Row_field.of_concrete (Rtag (x0, x1, x2, x3))
    method package_type : Package_type.t -> Package_type.t  =
      fun package_type ->
        let concrete = Package_type.to_concrete package_type in
        let (x0, x1) = concrete in
        let x0 = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#longident_loc x1 in (x0, x1)) x0 in
        let x1 = self#longident_loc x1 in
        Package_type.of_concrete (x0, x1)
    method core_type_desc : Core_type_desc.t -> Core_type_desc.t  =
      fun core_type_desc ->
        let concrete = Core_type_desc.to_concrete core_type_desc in
        match (concrete : Core_type_desc.concrete) with
        | Ptyp_extension x0 ->
          let x0 = self#extension x0 in
          Core_type_desc.of_concrete (Ptyp_extension x0)
        | Ptyp_package x0 ->
          let x0 = self#package_type x0 in
          Core_type_desc.of_concrete (Ptyp_package x0)
        | Ptyp_poly (x0, x1) ->
          let x0 = self#core_type x0 in
          let x1 = self#list (self#loc self#string) x1 in
          Core_type_desc.of_concrete (Ptyp_poly (x0, x1))
        | Ptyp_variant (x0, x1, x2) ->
          let x0 = self#option (self#list self#string) x0 in
          let x1 = self#closed_flag x1 in
          let x2 = self#list self#row_field x2 in
          Core_type_desc.of_concrete (Ptyp_variant (x0, x1, x2))
        | Ptyp_alias (x0, x1) ->
          let x0 = self#string x0 in
          let x1 = self#core_type x1 in
          Core_type_desc.of_concrete (Ptyp_alias (x0, x1))
        | Ptyp_class (x0, x1) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#longident_loc x1 in
          Core_type_desc.of_concrete (Ptyp_class (x0, x1))
        | Ptyp_object (x0, x1) ->
          let x0 = self#closed_flag x0 in
          let x1 = self#list self#object_field x1 in
          Core_type_desc.of_concrete (Ptyp_object (x0, x1))
        | Ptyp_constr (x0, x1) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#longident_loc x1 in
          Core_type_desc.of_concrete (Ptyp_constr (x0, x1))
        | Ptyp_tuple x0 ->
          let x0 = self#list self#core_type x0 in
          Core_type_desc.of_concrete (Ptyp_tuple x0)
        | Ptyp_arrow (x0, x1, x2) ->
          let x0 = self#core_type x0 in
          let x1 = self#core_type x1 in
          let x2 = self#arg_label x2 in
          Core_type_desc.of_concrete (Ptyp_arrow (x0, x1, x2))
        | Ptyp_var x0 ->
          let x0 = self#string x0 in
          Core_type_desc.of_concrete (Ptyp_var x0)
        | Ptyp_any ->
          Core_type_desc.of_concrete Ptyp_any
    method core_type : Core_type.t -> Core_type.t  =
      fun core_type ->
        let concrete = Core_type.to_concrete core_type in
        let { ptyp_attributes; ptyp_loc; ptyp_desc } : Core_type.concrete = concrete in
        let ptyp_attributes = self#attributes ptyp_attributes in
        let ptyp_loc = self#location ptyp_loc in
        let ptyp_desc = self#core_type_desc ptyp_desc in
        Core_type.of_concrete { ptyp_attributes; ptyp_loc; ptyp_desc }
    method payload : Payload.t -> Payload.t  =
      fun payload ->
        let concrete = Payload.to_concrete payload in
        match (concrete : Payload.concrete) with
        | PPat (x0, x1) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#pattern x1 in
          Payload.of_concrete (PPat (x0, x1))
        | PTyp x0 ->
          let x0 = self#core_type x0 in
          Payload.of_concrete (PTyp x0)
        | PSig x0 ->
          let x0 = self#signature x0 in
          Payload.of_concrete (PSig x0)
        | PStr x0 ->
          let x0 = self#structure x0 in
          Payload.of_concrete (PStr x0)
    method attributes : Attributes.t -> Attributes.t  =
      fun attributes ->
        let concrete = Attributes.to_concrete attributes in
        let concrete = self#list self#attribute concrete in
        Attributes.of_concrete concrete
    method extension : Extension.t -> Extension.t  =
      fun extension ->
        let concrete = Extension.to_concrete extension in
        let (x0, x1) = concrete in
        let x0 = self#payload x0 in
        let x1 = self#loc self#string x1 in
        Extension.of_concrete (x0, x1)
    method attribute : Attribute.t -> Attribute.t  =
      fun attribute ->
        let concrete = Attribute.to_concrete attribute in
        let (x0, x1) = concrete in
        let x0 = self#payload x0 in
        let x1 = self#loc self#string x1 in
        Attribute.of_concrete (x0, x1)
    method constant : Constant.t -> Constant.t  =
      fun constant ->
        let concrete = Constant.to_concrete constant in
        match (concrete : Constant.concrete) with
        | Pconst_float (x0, x1) ->
          let x0 = self#option self#char x0 in
          let x1 = self#string x1 in
          Constant.of_concrete (Pconst_float (x0, x1))
        | Pconst_string (x0, x1) ->
          let x0 = self#option self#string x0 in
          let x1 = self#string x1 in
          Constant.of_concrete (Pconst_string (x0, x1))
        | Pconst_char x0 ->
          let x0 = self#char x0 in
          Constant.of_concrete (Pconst_char x0)
        | Pconst_integer (x0, x1) ->
          let x0 = self#option self#char x0 in
          let x1 = self#string x1 in
          Constant.of_concrete (Pconst_integer (x0, x1))
    method variance : Variance.t -> Variance.t  =
      fun variance ->
        let concrete = Variance.to_concrete variance in
        match (concrete : Variance.concrete) with
        | Invariant ->
          Variance.of_concrete Invariant
        | Contravariant ->
          Variance.of_concrete Contravariant
        | Covariant ->
          Variance.of_concrete Covariant
    method arg_label : Arg_label.t -> Arg_label.t  =
      fun arg_label ->
        let concrete = Arg_label.to_concrete arg_label in
        match (concrete : Arg_label.concrete) with
        | Optional x0 ->
          let x0 = self#string x0 in
          Arg_label.of_concrete (Optional x0)
        | Labelled x0 ->
          let x0 = self#string x0 in
          Arg_label.of_concrete (Labelled x0)
        | Nolabel ->
          Arg_label.of_concrete Nolabel
    method closed_flag : Closed_flag.t -> Closed_flag.t  =
      fun closed_flag ->
        let concrete = Closed_flag.to_concrete closed_flag in
        match (concrete : Closed_flag.concrete) with
        | Open ->
          Closed_flag.of_concrete Open
        | Closed ->
          Closed_flag.of_concrete Closed
    method override_flag : Override_flag.t -> Override_flag.t  =
      fun override_flag ->
        let concrete = Override_flag.to_concrete override_flag in
        match (concrete : Override_flag.concrete) with
        | Fresh ->
          Override_flag.of_concrete Fresh
        | Override ->
          Override_flag.of_concrete Override
    method virtual_flag : Virtual_flag.t -> Virtual_flag.t  =
      fun virtual_flag ->
        let concrete = Virtual_flag.to_concrete virtual_flag in
        match (concrete : Virtual_flag.concrete) with
        | Concrete ->
          Virtual_flag.of_concrete Concrete
        | Virtual ->
          Virtual_flag.of_concrete Virtual
    method mutable_flag : Mutable_flag.t -> Mutable_flag.t  =
      fun mutable_flag ->
        let concrete = Mutable_flag.to_concrete mutable_flag in
        match (concrete : Mutable_flag.concrete) with
        | Mutable ->
          Mutable_flag.of_concrete Mutable
        | Immutable ->
          Mutable_flag.of_concrete Immutable
    method private_flag : Private_flag.t -> Private_flag.t  =
      fun private_flag ->
        let concrete = Private_flag.to_concrete private_flag in
        match (concrete : Private_flag.concrete) with
        | Public ->
          Private_flag.of_concrete Public
        | Private ->
          Private_flag.of_concrete Private
    method direction_flag : Direction_flag.t -> Direction_flag.t  =
      fun direction_flag ->
        let concrete = Direction_flag.to_concrete direction_flag in
        match (concrete : Direction_flag.concrete) with
        | Downto ->
          Direction_flag.of_concrete Downto
        | Upto ->
          Direction_flag.of_concrete Upto
    method rec_flag : Rec_flag.t -> Rec_flag.t  =
      fun rec_flag ->
        let concrete = Rec_flag.to_concrete rec_flag in
        match (concrete : Rec_flag.concrete) with
        | Recursive ->
          Rec_flag.of_concrete Recursive
        | Nonrecursive ->
          Rec_flag.of_concrete Nonrecursive
    method longident_loc : Longident_loc.t -> Longident_loc.t  =
      fun longident_loc ->
        let concrete = Longident_loc.to_concrete longident_loc in
        let concrete = self#loc self#longident concrete in
        Longident_loc.of_concrete concrete
    method longident : Longident.t -> Longident.t  =
      fun longident ->
        let concrete = Longident.to_concrete longident in
        match (concrete : Longident.concrete) with
        | Lapply (x0, x1) ->
          let x0 = self#longident x0 in
          let x1 = self#longident x1 in
          Longident.of_concrete (Lapply (x0, x1))
        | Ldot (x0, x1) ->
          let x0 = self#string x0 in
          let x1 = self#longident x1 in
          Longident.of_concrete (Ldot (x0, x1))
        | Lident x0 ->
          let x0 = self#string x0 in
          Longident.of_concrete (Lident x0)
  end

class virtual iter =
  object (self)
    method virtual bool : bool -> unit
    method virtual char : char -> unit
    method virtual int : int -> unit
    method virtual list : 'a . ('a -> unit) -> 'a list -> unit
    method virtual option : 'a . ('a -> unit) -> 'a option -> unit
    method virtual string : string -> unit
    method virtual location : Astlib.Location.t -> unit
    method virtual loc : 'a . ('a -> unit) -> 'a Astlib.Loc.t -> unit
    method directive_argument : Directive_argument.t -> unit  =
      fun directive_argument ->
        let concrete = Directive_argument.to_concrete directive_argument in
        match (concrete : Directive_argument.concrete) with
        | Pdir_bool x0 ->
          self#bool x0
        | Pdir_ident x0 ->
          self#longident x0
        | Pdir_int (x0, x1) ->
          self#option self#char x0;
          self#string x1
        | Pdir_string x0 ->
          self#string x0
        | Pdir_none ->
          ()
    method toplevel_phrase : Toplevel_phrase.t -> unit  =
      fun toplevel_phrase ->
        let concrete = Toplevel_phrase.to_concrete toplevel_phrase in
        match (concrete : Toplevel_phrase.concrete) with
        | Ptop_dir (x0, x1) ->
          self#directive_argument x0;
          self#string x1
        | Ptop_def x0 ->
          self#structure x0
    method module_binding : Module_binding.t -> unit  =
      fun module_binding ->
        let concrete = Module_binding.to_concrete module_binding in
        let { pmb_loc; pmb_attributes; pmb_expr; pmb_name } : Module_binding.concrete = concrete in
        self#location pmb_loc;
        self#attributes pmb_attributes;
        self#module_expr pmb_expr;
        self#loc self#string pmb_name
    method value_binding : Value_binding.t -> unit  =
      fun value_binding ->
        let concrete = Value_binding.to_concrete value_binding in
        let { pvb_loc; pvb_attributes; pvb_expr; pvb_pat } : Value_binding.concrete = concrete in
        self#location pvb_loc;
        self#attributes pvb_attributes;
        self#expression pvb_expr;
        self#pattern pvb_pat
    method structure_item_desc : Structure_item_desc.t -> unit  =
      fun structure_item_desc ->
        let concrete = Structure_item_desc.to_concrete structure_item_desc in
        match (concrete : Structure_item_desc.concrete) with
        | Pstr_extension (x0, x1) ->
          self#attributes x0;
          self#extension x1
        | Pstr_attribute x0 ->
          self#attribute x0
        | Pstr_include x0 ->
          self#include_declaration x0
        | Pstr_class_type x0 ->
          self#list self#class_type_declaration x0
        | Pstr_class x0 ->
          self#list self#class_declaration x0
        | Pstr_open x0 ->
          self#open_description x0
        | Pstr_modtype x0 ->
          self#module_type_declaration x0
        | Pstr_recmodule x0 ->
          self#list self#module_binding x0
        | Pstr_module x0 ->
          self#module_binding x0
        | Pstr_exception x0 ->
          self#extension_constructor x0
        | Pstr_typext x0 ->
          self#type_extension x0
        | Pstr_type (x0, x1) ->
          self#list self#type_declaration x0;
          self#rec_flag x1
        | Pstr_primitive x0 ->
          self#value_description x0
        | Pstr_value (x0, x1) ->
          self#list self#value_binding x0;
          self#rec_flag x1
        | Pstr_eval (x0, x1) ->
          self#attributes x0;
          self#expression x1
    method structure_item : Structure_item.t -> unit  =
      fun structure_item ->
        let concrete = Structure_item.to_concrete structure_item in
        let { pstr_loc; pstr_desc } : Structure_item.concrete = concrete in
        self#location pstr_loc;
        self#structure_item_desc pstr_desc
    method structure : Structure.t -> unit  =
      fun structure ->
        let concrete = Structure.to_concrete structure in
        self#list self#structure_item concrete
    method module_expr_desc : Module_expr_desc.t -> unit  =
      fun module_expr_desc ->
        let concrete = Module_expr_desc.to_concrete module_expr_desc in
        match (concrete : Module_expr_desc.concrete) with
        | Pmod_extension x0 ->
          self#extension x0
        | Pmod_unpack x0 ->
          self#expression x0
        | Pmod_constraint (x0, x1) ->
          self#module_type x0;
          self#module_expr x1
        | Pmod_apply (x0, x1) ->
          self#module_expr x0;
          self#module_expr x1
        | Pmod_functor (x0, x1, x2) ->
          self#module_expr x0;
          self#option self#module_type x1;
          self#loc self#string x2
        | Pmod_structure x0 ->
          self#structure x0
        | Pmod_ident x0 ->
          self#longident_loc x0
    method module_expr : Module_expr.t -> unit  =
      fun module_expr ->
        let concrete = Module_expr.to_concrete module_expr in
        let { pmod_attributes; pmod_loc; pmod_desc } : Module_expr.concrete = concrete in
        self#attributes pmod_attributes;
        self#location pmod_loc;
        self#module_expr_desc pmod_desc
    method with_constraint : With_constraint.t -> unit  =
      fun with_constraint ->
        let concrete = With_constraint.to_concrete with_constraint in
        match (concrete : With_constraint.concrete) with
        | Pwith_modsubst (x0, x1) ->
          self#longident_loc x0;
          self#longident_loc x1
        | Pwith_typesubst (x0, x1) ->
          self#type_declaration x0;
          self#longident_loc x1
        | Pwith_module (x0, x1) ->
          self#longident_loc x0;
          self#longident_loc x1
        | Pwith_type (x0, x1) ->
          self#type_declaration x0;
          self#longident_loc x1
    method include_declaration : Include_declaration.t -> unit  =
      fun include_declaration ->
        let concrete = Include_declaration.to_concrete include_declaration in
        self#include_infos_module_expr concrete
    method include_description : Include_description.t -> unit  =
      fun include_description ->
        let concrete = Include_description.to_concrete include_description in
        self#include_infos_module_type concrete
    method include_infos_module_expr : Module_expr.t Include_infos.t -> unit  =
      fun include_infos ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_expr.t Include_infos.concrete = concrete in
        self#attributes pincl_attributes;
        self#location pincl_loc;
        self#module_expr pincl_mod
    method include_infos_module_type : Module_type.t Include_infos.t -> unit  =
      fun include_infos ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_type.t Include_infos.concrete = concrete in
        self#attributes pincl_attributes;
        self#location pincl_loc;
        self#module_type pincl_mod
    method open_description : Open_description.t -> unit  =
      fun open_description ->
        let concrete = Open_description.to_concrete open_description in
        let { popen_attributes; popen_loc; popen_override; popen_lid } : Open_description.concrete = concrete in
        self#attributes popen_attributes;
        self#location popen_loc;
        self#override_flag popen_override;
        self#longident_loc popen_lid
    method module_type_declaration : Module_type_declaration.t -> unit  =
      fun module_type_declaration ->
        let concrete = Module_type_declaration.to_concrete module_type_declaration in
        let { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name } : Module_type_declaration.concrete = concrete in
        self#location pmtd_loc;
        self#attributes pmtd_attributes;
        self#option self#module_type pmtd_type;
        self#loc self#string pmtd_name
    method module_declaration : Module_declaration.t -> unit  =
      fun module_declaration ->
        let concrete = Module_declaration.to_concrete module_declaration in
        let { pmd_loc; pmd_attributes; pmd_type; pmd_name } : Module_declaration.concrete = concrete in
        self#location pmd_loc;
        self#attributes pmd_attributes;
        self#module_type pmd_type;
        self#loc self#string pmd_name
    method signature_item_desc : Signature_item_desc.t -> unit  =
      fun signature_item_desc ->
        let concrete = Signature_item_desc.to_concrete signature_item_desc in
        match (concrete : Signature_item_desc.concrete) with
        | Psig_extension (x0, x1) ->
          self#attributes x0;
          self#extension x1
        | Psig_attribute x0 ->
          self#attribute x0
        | Psig_class_type x0 ->
          self#list self#class_type_declaration x0
        | Psig_class x0 ->
          self#list self#class_description x0
        | Psig_include x0 ->
          self#include_description x0
        | Psig_open x0 ->
          self#open_description x0
        | Psig_modtype x0 ->
          self#module_type_declaration x0
        | Psig_recmodule x0 ->
          self#list self#module_declaration x0
        | Psig_module x0 ->
          self#module_declaration x0
        | Psig_exception x0 ->
          self#extension_constructor x0
        | Psig_typext x0 ->
          self#type_extension x0
        | Psig_type (x0, x1) ->
          self#list self#type_declaration x0;
          self#rec_flag x1
        | Psig_value x0 ->
          self#value_description x0
    method signature_item : Signature_item.t -> unit  =
      fun signature_item ->
        let concrete = Signature_item.to_concrete signature_item in
        let { psig_loc; psig_desc } : Signature_item.concrete = concrete in
        self#location psig_loc;
        self#signature_item_desc psig_desc
    method signature : Signature.t -> unit  =
      fun signature ->
        let concrete = Signature.to_concrete signature in
        self#list self#signature_item concrete
    method module_type_desc : Module_type_desc.t -> unit  =
      fun module_type_desc ->
        let concrete = Module_type_desc.to_concrete module_type_desc in
        match (concrete : Module_type_desc.concrete) with
        | Pmty_alias x0 ->
          self#longident_loc x0
        | Pmty_extension x0 ->
          self#extension x0
        | Pmty_typeof x0 ->
          self#module_expr x0
        | Pmty_with (x0, x1) ->
          self#list self#with_constraint x0;
          self#module_type x1
        | Pmty_functor (x0, x1, x2) ->
          self#module_type x0;
          self#option self#module_type x1;
          self#loc self#string x2
        | Pmty_signature x0 ->
          self#signature x0
        | Pmty_ident x0 ->
          self#longident_loc x0
    method module_type : Module_type.t -> unit  =
      fun module_type ->
        let concrete = Module_type.to_concrete module_type in
        let { pmty_attributes; pmty_loc; pmty_desc } : Module_type.concrete = concrete in
        self#attributes pmty_attributes;
        self#location pmty_loc;
        self#module_type_desc pmty_desc
    method class_declaration : Class_declaration.t -> unit  =
      fun class_declaration ->
        let concrete = Class_declaration.to_concrete class_declaration in
        self#class_infos_class_expr concrete
    method class_field_kind : Class_field_kind.t -> unit  =
      fun class_field_kind ->
        let concrete = Class_field_kind.to_concrete class_field_kind in
        match (concrete : Class_field_kind.concrete) with
        | Cfk_concrete (x0, x1) ->
          self#expression x0;
          self#override_flag x1
        | Cfk_virtual x0 ->
          self#core_type x0
    method class_field_desc : Class_field_desc.t -> unit  =
      fun class_field_desc ->
        let concrete = Class_field_desc.to_concrete class_field_desc in
        match (concrete : Class_field_desc.concrete) with
        | Pcf_extension x0 ->
          self#extension x0
        | Pcf_attribute x0 ->
          self#attribute x0
        | Pcf_initializer x0 ->
          self#expression x0
        | Pcf_constraint x0 ->
          (fun (x0, x1) -> self#core_type x0; self#core_type x1) x0
        | Pcf_method x0 ->
          (fun (x0, x1, x2) -> self#class_field_kind x0; self#private_flag x1; self#loc self#string x2) x0
        | Pcf_val x0 ->
          (fun (x0, x1, x2) -> self#class_field_kind x0; self#mutable_flag x1; self#loc self#string x2) x0
        | Pcf_inherit (x0, x1, x2) ->
          self#option (self#loc self#string) x0;
          self#class_expr x1;
          self#override_flag x2
    method class_field : Class_field.t -> unit  =
      fun class_field ->
        let concrete = Class_field.to_concrete class_field in
        let { pcf_attributes; pcf_loc; pcf_desc } : Class_field.concrete = concrete in
        self#attributes pcf_attributes;
        self#location pcf_loc;
        self#class_field_desc pcf_desc
    method class_structure : Class_structure.t -> unit  =
      fun class_structure ->
        let concrete = Class_structure.to_concrete class_structure in
        let { pcstr_fields; pcstr_self } : Class_structure.concrete = concrete in
        self#list self#class_field pcstr_fields;
        self#pattern pcstr_self
    method class_expr_desc : Class_expr_desc.t -> unit  =
      fun class_expr_desc ->
        let concrete = Class_expr_desc.to_concrete class_expr_desc in
        match (concrete : Class_expr_desc.concrete) with
        | Pcl_open (x0, x1, x2) ->
          self#class_expr x0;
          self#longident_loc x1;
          self#override_flag x2
        | Pcl_extension x0 ->
          self#extension x0
        | Pcl_constraint (x0, x1) ->
          self#class_type x0;
          self#class_expr x1
        | Pcl_let (x0, x1, x2) ->
          self#class_expr x0;
          self#list self#value_binding x1;
          self#rec_flag x2
        | Pcl_apply (x0, x1) ->
          self#list (fun (x0, x1) -> self#expression x0; self#arg_label x1) x0;
          self#class_expr x1
        | Pcl_fun (x0, x1, x2, x3) ->
          self#class_expr x0;
          self#pattern x1;
          self#option self#expression x2;
          self#arg_label x3
        | Pcl_structure x0 ->
          self#class_structure x0
        | Pcl_constr (x0, x1) ->
          self#list self#core_type x0;
          self#longident_loc x1
    method class_expr : Class_expr.t -> unit  =
      fun class_expr ->
        let concrete = Class_expr.to_concrete class_expr in
        let { pcl_attributes; pcl_loc; pcl_desc } : Class_expr.concrete = concrete in
        self#attributes pcl_attributes;
        self#location pcl_loc;
        self#class_expr_desc pcl_desc
    method class_type_declaration : Class_type_declaration.t -> unit  =
      fun class_type_declaration ->
        let concrete = Class_type_declaration.to_concrete class_type_declaration in
        self#class_infos_class_type concrete
    method class_description : Class_description.t -> unit  =
      fun class_description ->
        let concrete = Class_description.to_concrete class_description in
        self#class_infos_class_type concrete
    method class_infos_class_expr : Class_expr.t Class_infos.t -> unit  =
      fun class_infos ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_expr.t Class_infos.concrete = concrete in
        self#attributes pci_attributes;
        self#location pci_loc;
        self#class_expr pci_expr;
        self#loc self#string pci_name;
        self#list (fun (x0, x1) -> self#variance x0; self#core_type x1) pci_params;
        self#virtual_flag pci_virt
    method class_infos_class_type : Class_type.t Class_infos.t -> unit  =
      fun class_infos ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_type.t Class_infos.concrete = concrete in
        self#attributes pci_attributes;
        self#location pci_loc;
        self#class_type pci_expr;
        self#loc self#string pci_name;
        self#list (fun (x0, x1) -> self#variance x0; self#core_type x1) pci_params;
        self#virtual_flag pci_virt
    method class_type_field_desc : Class_type_field_desc.t -> unit  =
      fun class_type_field_desc ->
        let concrete = Class_type_field_desc.to_concrete class_type_field_desc in
        match (concrete : Class_type_field_desc.concrete) with
        | Pctf_extension x0 ->
          self#extension x0
        | Pctf_attribute x0 ->
          self#attribute x0
        | Pctf_constraint x0 ->
          (fun (x0, x1) -> self#core_type x0; self#core_type x1) x0
        | Pctf_method x0 ->
          (fun (x0, x1, x2, x3) -> self#core_type x0; self#virtual_flag x1; self#private_flag x2; self#loc self#string x3) x0
        | Pctf_val x0 ->
          (fun (x0, x1, x2, x3) -> self#core_type x0; self#virtual_flag x1; self#mutable_flag x2; self#loc self#string x3) x0
        | Pctf_inherit x0 ->
          self#class_type x0
    method class_type_field : Class_type_field.t -> unit  =
      fun class_type_field ->
        let concrete = Class_type_field.to_concrete class_type_field in
        let { pctf_attributes; pctf_loc; pctf_desc } : Class_type_field.concrete = concrete in
        self#attributes pctf_attributes;
        self#location pctf_loc;
        self#class_type_field_desc pctf_desc
    method class_signature : Class_signature.t -> unit  =
      fun class_signature ->
        let concrete = Class_signature.to_concrete class_signature in
        let { pcsig_fields; pcsig_self } : Class_signature.concrete = concrete in
        self#list self#class_type_field pcsig_fields;
        self#core_type pcsig_self
    method class_type_desc : Class_type_desc.t -> unit  =
      fun class_type_desc ->
        let concrete = Class_type_desc.to_concrete class_type_desc in
        match (concrete : Class_type_desc.concrete) with
        | Pcty_open (x0, x1, x2) ->
          self#class_type x0;
          self#longident_loc x1;
          self#override_flag x2
        | Pcty_extension x0 ->
          self#extension x0
        | Pcty_arrow (x0, x1, x2) ->
          self#class_type x0;
          self#core_type x1;
          self#arg_label x2
        | Pcty_signature x0 ->
          self#class_signature x0
        | Pcty_constr (x0, x1) ->
          self#list self#core_type x0;
          self#longident_loc x1
    method class_type : Class_type.t -> unit  =
      fun class_type ->
        let concrete = Class_type.to_concrete class_type in
        let { pcty_attributes; pcty_loc; pcty_desc } : Class_type.concrete = concrete in
        self#attributes pcty_attributes;
        self#location pcty_loc;
        self#class_type_desc pcty_desc
    method extension_constructor_kind : Extension_constructor_kind.t -> unit  =
      fun extension_constructor_kind ->
        let concrete = Extension_constructor_kind.to_concrete extension_constructor_kind in
        match (concrete : Extension_constructor_kind.concrete) with
        | Pext_rebind x0 ->
          self#longident_loc x0
        | Pext_decl (x0, x1) ->
          self#option self#core_type x0;
          self#constructor_arguments x1
    method extension_constructor : Extension_constructor.t -> unit  =
      fun extension_constructor ->
        let concrete = Extension_constructor.to_concrete extension_constructor in
        let { pext_attributes; pext_loc; pext_kind; pext_name } : Extension_constructor.concrete = concrete in
        self#attributes pext_attributes;
        self#location pext_loc;
        self#extension_constructor_kind pext_kind;
        self#loc self#string pext_name
    method type_extension : Type_extension.t -> unit  =
      fun type_extension ->
        let concrete = Type_extension.to_concrete type_extension in
        let { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path } : Type_extension.concrete = concrete in
        self#attributes ptyext_attributes;
        self#private_flag ptyext_private;
        self#list self#extension_constructor ptyext_constructors;
        self#list (fun (x0, x1) -> self#variance x0; self#core_type x1) ptyext_params;
        self#longident_loc ptyext_path
    method constructor_arguments : Constructor_arguments.t -> unit  =
      fun constructor_arguments ->
        let concrete = Constructor_arguments.to_concrete constructor_arguments in
        match (concrete : Constructor_arguments.concrete) with
        | Pcstr_record x0 ->
          self#list self#label_declaration x0
        | Pcstr_tuple x0 ->
          self#list self#core_type x0
    method constructor_declaration : Constructor_declaration.t -> unit  =
      fun constructor_declaration ->
        let concrete = Constructor_declaration.to_concrete constructor_declaration in
        let { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name } : Constructor_declaration.concrete = concrete in
        self#attributes pcd_attributes;
        self#location pcd_loc;
        self#option self#core_type pcd_res;
        self#constructor_arguments pcd_args;
        self#loc self#string pcd_name
    method label_declaration : Label_declaration.t -> unit  =
      fun label_declaration ->
        let concrete = Label_declaration.to_concrete label_declaration in
        let { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name } : Label_declaration.concrete = concrete in
        self#attributes pld_attributes;
        self#location pld_loc;
        self#core_type pld_type;
        self#mutable_flag pld_mutable;
        self#loc self#string pld_name
    method type_kind : Type_kind.t -> unit  =
      fun type_kind ->
        let concrete = Type_kind.to_concrete type_kind in
        match (concrete : Type_kind.concrete) with
        | Ptype_open ->
          ()
        | Ptype_record x0 ->
          self#list self#label_declaration x0
        | Ptype_variant x0 ->
          self#list self#constructor_declaration x0
        | Ptype_abstract ->
          ()
    method type_declaration : Type_declaration.t -> unit  =
      fun type_declaration ->
        let concrete = Type_declaration.to_concrete type_declaration in
        let { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name } : Type_declaration.concrete = concrete in
        self#location ptype_loc;
        self#attributes ptype_attributes;
        self#option self#core_type ptype_manifest;
        self#private_flag ptype_private;
        self#type_kind ptype_kind;
        self#list (fun (x0, x1, x2) -> self#location x0; self#core_type x1; self#core_type x2) ptype_cstrs;
        self#list (fun (x0, x1) -> self#variance x0; self#core_type x1) ptype_params;
        self#loc self#string ptype_name
    method value_description : Value_description.t -> unit  =
      fun value_description ->
        let concrete = Value_description.to_concrete value_description in
        let { pval_loc; pval_attributes; pval_prim; pval_type; pval_name } : Value_description.concrete = concrete in
        self#location pval_loc;
        self#attributes pval_attributes;
        self#list self#string pval_prim;
        self#core_type pval_type;
        self#loc self#string pval_name
    method case : Case.t -> unit  =
      fun case ->
        let concrete = Case.to_concrete case in
        let { pc_rhs; pc_guard; pc_lhs } : Case.concrete = concrete in
        self#expression pc_rhs;
        self#option self#expression pc_guard;
        self#pattern pc_lhs
    method expression_desc : Expression_desc.t -> unit  =
      fun expression_desc ->
        let concrete = Expression_desc.to_concrete expression_desc in
        match (concrete : Expression_desc.concrete) with
        | Pexp_unreachable ->
          ()
        | Pexp_extension x0 ->
          self#extension x0
        | Pexp_open (x0, x1, x2) ->
          self#expression x0;
          self#longident_loc x1;
          self#override_flag x2
        | Pexp_pack x0 ->
          self#module_expr x0
        | Pexp_newtype (x0, x1) ->
          self#expression x0;
          self#loc self#string x1
        | Pexp_object x0 ->
          self#class_structure x0
        | Pexp_poly (x0, x1) ->
          self#option self#core_type x0;
          self#expression x1
        | Pexp_lazy x0 ->
          self#expression x0
        | Pexp_assert x0 ->
          self#expression x0
        | Pexp_letexception (x0, x1) ->
          self#expression x0;
          self#extension_constructor x1
        | Pexp_letmodule (x0, x1, x2) ->
          self#expression x0;
          self#module_expr x1;
          self#loc self#string x2
        | Pexp_override x0 ->
          self#list (fun (x0, x1) -> self#expression x0; self#loc self#string x1) x0
        | Pexp_setinstvar (x0, x1) ->
          self#expression x0;
          self#loc self#string x1
        | Pexp_new x0 ->
          self#longident_loc x0
        | Pexp_send (x0, x1) ->
          self#loc self#string x0;
          self#expression x1
        | Pexp_coerce (x0, x1, x2) ->
          self#core_type x0;
          self#option self#core_type x1;
          self#expression x2
        | Pexp_constraint (x0, x1) ->
          self#core_type x0;
          self#expression x1
        | Pexp_for (x0, x1, x2, x3, x4) ->
          self#expression x0;
          self#direction_flag x1;
          self#expression x2;
          self#expression x3;
          self#pattern x4
        | Pexp_while (x0, x1) ->
          self#expression x0;
          self#expression x1
        | Pexp_sequence (x0, x1) ->
          self#expression x0;
          self#expression x1
        | Pexp_ifthenelse (x0, x1, x2) ->
          self#option self#expression x0;
          self#expression x1;
          self#expression x2
        | Pexp_array x0 ->
          self#list self#expression x0
        | Pexp_setfield (x0, x1, x2) ->
          self#expression x0;
          self#longident_loc x1;
          self#expression x2
        | Pexp_field (x0, x1) ->
          self#longident_loc x0;
          self#expression x1
        | Pexp_record (x0, x1) ->
          self#option self#expression x0;
          self#list (fun (x0, x1) -> self#expression x0; self#longident_loc x1) x1
        | Pexp_variant (x0, x1) ->
          self#option self#expression x0;
          self#string x1
        | Pexp_construct (x0, x1) ->
          self#option self#expression x0;
          self#longident_loc x1
        | Pexp_tuple x0 ->
          self#list self#expression x0
        | Pexp_try (x0, x1) ->
          self#list self#case x0;
          self#expression x1
        | Pexp_match (x0, x1) ->
          self#list self#case x0;
          self#expression x1
        | Pexp_apply (x0, x1) ->
          self#list (fun (x0, x1) -> self#expression x0; self#arg_label x1) x0;
          self#expression x1
        | Pexp_fun (x0, x1, x2, x3) ->
          self#expression x0;
          self#pattern x1;
          self#option self#expression x2;
          self#arg_label x3
        | Pexp_function x0 ->
          self#list self#case x0
        | Pexp_let (x0, x1, x2) ->
          self#expression x0;
          self#list self#value_binding x1;
          self#rec_flag x2
        | Pexp_constant x0 ->
          self#constant x0
        | Pexp_ident x0 ->
          self#longident_loc x0
    method expression : Expression.t -> unit  =
      fun expression ->
        let concrete = Expression.to_concrete expression in
        let { pexp_attributes; pexp_loc; pexp_desc } : Expression.concrete = concrete in
        self#attributes pexp_attributes;
        self#location pexp_loc;
        self#expression_desc pexp_desc
    method pattern_desc : Pattern_desc.t -> unit  =
      fun pattern_desc ->
        let concrete = Pattern_desc.to_concrete pattern_desc in
        match (concrete : Pattern_desc.concrete) with
        | Ppat_open (x0, x1) ->
          self#pattern x0;
          self#longident_loc x1
        | Ppat_extension x0 ->
          self#extension x0
        | Ppat_exception x0 ->
          self#pattern x0
        | Ppat_unpack x0 ->
          self#loc self#string x0
        | Ppat_lazy x0 ->
          self#pattern x0
        | Ppat_type x0 ->
          self#longident_loc x0
        | Ppat_constraint (x0, x1) ->
          self#core_type x0;
          self#pattern x1
        | Ppat_or (x0, x1) ->
          self#pattern x0;
          self#pattern x1
        | Ppat_array x0 ->
          self#list self#pattern x0
        | Ppat_record (x0, x1) ->
          self#closed_flag x0;
          self#list (fun (x0, x1) -> self#pattern x0; self#longident_loc x1) x1
        | Ppat_variant (x0, x1) ->
          self#option self#pattern x0;
          self#string x1
        | Ppat_construct (x0, x1) ->
          self#option self#pattern x0;
          self#longident_loc x1
        | Ppat_tuple x0 ->
          self#list self#pattern x0
        | Ppat_interval (x0, x1) ->
          self#constant x0;
          self#constant x1
        | Ppat_constant x0 ->
          self#constant x0
        | Ppat_alias (x0, x1) ->
          self#loc self#string x0;
          self#pattern x1
        | Ppat_var x0 ->
          self#loc self#string x0
        | Ppat_any ->
          ()
    method pattern : Pattern.t -> unit  =
      fun pattern ->
        let concrete = Pattern.to_concrete pattern in
        let { ppat_attributes; ppat_loc; ppat_desc } : Pattern.concrete = concrete in
        self#attributes ppat_attributes;
        self#location ppat_loc;
        self#pattern_desc ppat_desc
    method object_field : Object_field.t -> unit  =
      fun object_field ->
        let concrete = Object_field.to_concrete object_field in
        match (concrete : Object_field.concrete) with
        | Oinherit x0 ->
          self#core_type x0
        | Otag (x0, x1, x2) ->
          self#core_type x0;
          self#attributes x1;
          self#loc self#string x2
    method row_field : Row_field.t -> unit  =
      fun row_field ->
        let concrete = Row_field.to_concrete row_field in
        match (concrete : Row_field.concrete) with
        | Rinherit x0 ->
          self#core_type x0
        | Rtag (x0, x1, x2, x3) ->
          self#list self#core_type x0;
          self#bool x1;
          self#attributes x2;
          self#loc self#string x3
    method package_type : Package_type.t -> unit  =
      fun package_type ->
        let concrete = Package_type.to_concrete package_type in
        let (x0, x1) = concrete in
        self#list (fun (x0, x1) -> self#core_type x0; self#longident_loc x1) x0;
        self#longident_loc x1
    method core_type_desc : Core_type_desc.t -> unit  =
      fun core_type_desc ->
        let concrete = Core_type_desc.to_concrete core_type_desc in
        match (concrete : Core_type_desc.concrete) with
        | Ptyp_extension x0 ->
          self#extension x0
        | Ptyp_package x0 ->
          self#package_type x0
        | Ptyp_poly (x0, x1) ->
          self#core_type x0;
          self#list (self#loc self#string) x1
        | Ptyp_variant (x0, x1, x2) ->
          self#option (self#list self#string) x0;
          self#closed_flag x1;
          self#list self#row_field x2
        | Ptyp_alias (x0, x1) ->
          self#string x0;
          self#core_type x1
        | Ptyp_class (x0, x1) ->
          self#list self#core_type x0;
          self#longident_loc x1
        | Ptyp_object (x0, x1) ->
          self#closed_flag x0;
          self#list self#object_field x1
        | Ptyp_constr (x0, x1) ->
          self#list self#core_type x0;
          self#longident_loc x1
        | Ptyp_tuple x0 ->
          self#list self#core_type x0
        | Ptyp_arrow (x0, x1, x2) ->
          self#core_type x0;
          self#core_type x1;
          self#arg_label x2
        | Ptyp_var x0 ->
          self#string x0
        | Ptyp_any ->
          ()
    method core_type : Core_type.t -> unit  =
      fun core_type ->
        let concrete = Core_type.to_concrete core_type in
        let { ptyp_attributes; ptyp_loc; ptyp_desc } : Core_type.concrete = concrete in
        self#attributes ptyp_attributes;
        self#location ptyp_loc;
        self#core_type_desc ptyp_desc
    method payload : Payload.t -> unit  =
      fun payload ->
        let concrete = Payload.to_concrete payload in
        match (concrete : Payload.concrete) with
        | PPat (x0, x1) ->
          self#option self#expression x0;
          self#pattern x1
        | PTyp x0 ->
          self#core_type x0
        | PSig x0 ->
          self#signature x0
        | PStr x0 ->
          self#structure x0
    method attributes : Attributes.t -> unit  =
      fun attributes ->
        let concrete = Attributes.to_concrete attributes in
        self#list self#attribute concrete
    method extension : Extension.t -> unit  =
      fun extension ->
        let concrete = Extension.to_concrete extension in
        let (x0, x1) = concrete in
        self#payload x0;
        self#loc self#string x1
    method attribute : Attribute.t -> unit  =
      fun attribute ->
        let concrete = Attribute.to_concrete attribute in
        let (x0, x1) = concrete in
        self#payload x0;
        self#loc self#string x1
    method constant : Constant.t -> unit  =
      fun constant ->
        let concrete = Constant.to_concrete constant in
        match (concrete : Constant.concrete) with
        | Pconst_float (x0, x1) ->
          self#option self#char x0;
          self#string x1
        | Pconst_string (x0, x1) ->
          self#option self#string x0;
          self#string x1
        | Pconst_char x0 ->
          self#char x0
        | Pconst_integer (x0, x1) ->
          self#option self#char x0;
          self#string x1
    method variance : Variance.t -> unit  =
      fun variance ->
        let concrete = Variance.to_concrete variance in
        match (concrete : Variance.concrete) with
        | Invariant ->
          ()
        | Contravariant ->
          ()
        | Covariant ->
          ()
    method arg_label : Arg_label.t -> unit  =
      fun arg_label ->
        let concrete = Arg_label.to_concrete arg_label in
        match (concrete : Arg_label.concrete) with
        | Optional x0 ->
          self#string x0
        | Labelled x0 ->
          self#string x0
        | Nolabel ->
          ()
    method closed_flag : Closed_flag.t -> unit  =
      fun closed_flag ->
        let concrete = Closed_flag.to_concrete closed_flag in
        match (concrete : Closed_flag.concrete) with
        | Open ->
          ()
        | Closed ->
          ()
    method override_flag : Override_flag.t -> unit  =
      fun override_flag ->
        let concrete = Override_flag.to_concrete override_flag in
        match (concrete : Override_flag.concrete) with
        | Fresh ->
          ()
        | Override ->
          ()
    method virtual_flag : Virtual_flag.t -> unit  =
      fun virtual_flag ->
        let concrete = Virtual_flag.to_concrete virtual_flag in
        match (concrete : Virtual_flag.concrete) with
        | Concrete ->
          ()
        | Virtual ->
          ()
    method mutable_flag : Mutable_flag.t -> unit  =
      fun mutable_flag ->
        let concrete = Mutable_flag.to_concrete mutable_flag in
        match (concrete : Mutable_flag.concrete) with
        | Mutable ->
          ()
        | Immutable ->
          ()
    method private_flag : Private_flag.t -> unit  =
      fun private_flag ->
        let concrete = Private_flag.to_concrete private_flag in
        match (concrete : Private_flag.concrete) with
        | Public ->
          ()
        | Private ->
          ()
    method direction_flag : Direction_flag.t -> unit  =
      fun direction_flag ->
        let concrete = Direction_flag.to_concrete direction_flag in
        match (concrete : Direction_flag.concrete) with
        | Downto ->
          ()
        | Upto ->
          ()
    method rec_flag : Rec_flag.t -> unit  =
      fun rec_flag ->
        let concrete = Rec_flag.to_concrete rec_flag in
        match (concrete : Rec_flag.concrete) with
        | Recursive ->
          ()
        | Nonrecursive ->
          ()
    method longident_loc : Longident_loc.t -> unit  =
      fun longident_loc ->
        let concrete = Longident_loc.to_concrete longident_loc in
        self#loc self#longident concrete
    method longident : Longident.t -> unit  =
      fun longident ->
        let concrete = Longident.to_concrete longident in
        match (concrete : Longident.concrete) with
        | Lapply (x0, x1) ->
          self#longident x0;
          self#longident x1
        | Ldot (x0, x1) ->
          self#string x0;
          self#longident x1
        | Lident x0 ->
          self#string x0
  end

class virtual ['acc] fold =
  object (self)
    method virtual bool : bool -> 'acc -> 'acc
    method virtual char : char -> 'acc -> 'acc
    method virtual int : int -> 'acc -> 'acc
    method virtual list : 'a . ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
    method virtual option : 'a . ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
    method virtual string : string -> 'acc -> 'acc
    method virtual location : Astlib.Location.t -> 'acc -> 'acc
    method virtual loc : 'a . ('a -> 'acc -> 'acc) -> 'a Astlib.Loc.t -> 'acc -> 'acc
    method directive_argument : Directive_argument.t -> 'acc -> 'acc  =
      fun directive_argument acc ->
        let concrete = Directive_argument.to_concrete directive_argument in
        match (concrete : Directive_argument.concrete) with
        | Pdir_bool x0 ->
          let acc = self#bool x0 acc in
          acc
        | Pdir_ident x0 ->
          let acc = self#longident x0 acc in
          acc
        | Pdir_int (x0, x1) ->
          let acc = self#option self#char x0 acc in
          let acc = self#string x1 acc in
          acc
        | Pdir_string x0 ->
          let acc = self#string x0 acc in
          acc
        | Pdir_none ->
          acc
    method toplevel_phrase : Toplevel_phrase.t -> 'acc -> 'acc  =
      fun toplevel_phrase acc ->
        let concrete = Toplevel_phrase.to_concrete toplevel_phrase in
        match (concrete : Toplevel_phrase.concrete) with
        | Ptop_dir (x0, x1) ->
          let acc = self#directive_argument x0 acc in
          let acc = self#string x1 acc in
          acc
        | Ptop_def x0 ->
          let acc = self#structure x0 acc in
          acc
    method module_binding : Module_binding.t -> 'acc -> 'acc  =
      fun module_binding acc ->
        let concrete = Module_binding.to_concrete module_binding in
        let { pmb_loc; pmb_attributes; pmb_expr; pmb_name } : Module_binding.concrete = concrete in
        let acc = self#location pmb_loc acc in
        let acc = self#attributes pmb_attributes acc in
        let acc = self#module_expr pmb_expr acc in
        let acc = self#loc self#string pmb_name acc in
        acc
    method value_binding : Value_binding.t -> 'acc -> 'acc  =
      fun value_binding acc ->
        let concrete = Value_binding.to_concrete value_binding in
        let { pvb_loc; pvb_attributes; pvb_expr; pvb_pat } : Value_binding.concrete = concrete in
        let acc = self#location pvb_loc acc in
        let acc = self#attributes pvb_attributes acc in
        let acc = self#expression pvb_expr acc in
        let acc = self#pattern pvb_pat acc in
        acc
    method structure_item_desc : Structure_item_desc.t -> 'acc -> 'acc  =
      fun structure_item_desc acc ->
        let concrete = Structure_item_desc.to_concrete structure_item_desc in
        match (concrete : Structure_item_desc.concrete) with
        | Pstr_extension (x0, x1) ->
          let acc = self#attributes x0 acc in
          let acc = self#extension x1 acc in
          acc
        | Pstr_attribute x0 ->
          let acc = self#attribute x0 acc in
          acc
        | Pstr_include x0 ->
          let acc = self#include_declaration x0 acc in
          acc
        | Pstr_class_type x0 ->
          let acc = self#list self#class_type_declaration x0 acc in
          acc
        | Pstr_class x0 ->
          let acc = self#list self#class_declaration x0 acc in
          acc
        | Pstr_open x0 ->
          let acc = self#open_description x0 acc in
          acc
        | Pstr_modtype x0 ->
          let acc = self#module_type_declaration x0 acc in
          acc
        | Pstr_recmodule x0 ->
          let acc = self#list self#module_binding x0 acc in
          acc
        | Pstr_module x0 ->
          let acc = self#module_binding x0 acc in
          acc
        | Pstr_exception x0 ->
          let acc = self#extension_constructor x0 acc in
          acc
        | Pstr_typext x0 ->
          let acc = self#type_extension x0 acc in
          acc
        | Pstr_type (x0, x1) ->
          let acc = self#list self#type_declaration x0 acc in
          let acc = self#rec_flag x1 acc in
          acc
        | Pstr_primitive x0 ->
          let acc = self#value_description x0 acc in
          acc
        | Pstr_value (x0, x1) ->
          let acc = self#list self#value_binding x0 acc in
          let acc = self#rec_flag x1 acc in
          acc
        | Pstr_eval (x0, x1) ->
          let acc = self#attributes x0 acc in
          let acc = self#expression x1 acc in
          acc
    method structure_item : Structure_item.t -> 'acc -> 'acc  =
      fun structure_item acc ->
        let concrete = Structure_item.to_concrete structure_item in
        let { pstr_loc; pstr_desc } : Structure_item.concrete = concrete in
        let acc = self#location pstr_loc acc in
        let acc = self#structure_item_desc pstr_desc acc in
        acc
    method structure : Structure.t -> 'acc -> 'acc  =
      fun structure acc ->
        let concrete = Structure.to_concrete structure in
        let acc = self#list self#structure_item concrete acc in
        acc
    method module_expr_desc : Module_expr_desc.t -> 'acc -> 'acc  =
      fun module_expr_desc acc ->
        let concrete = Module_expr_desc.to_concrete module_expr_desc in
        match (concrete : Module_expr_desc.concrete) with
        | Pmod_extension x0 ->
          let acc = self#extension x0 acc in
          acc
        | Pmod_unpack x0 ->
          let acc = self#expression x0 acc in
          acc
        | Pmod_constraint (x0, x1) ->
          let acc = self#module_type x0 acc in
          let acc = self#module_expr x1 acc in
          acc
        | Pmod_apply (x0, x1) ->
          let acc = self#module_expr x0 acc in
          let acc = self#module_expr x1 acc in
          acc
        | Pmod_functor (x0, x1, x2) ->
          let acc = self#module_expr x0 acc in
          let acc = self#option self#module_type x1 acc in
          let acc = self#loc self#string x2 acc in
          acc
        | Pmod_structure x0 ->
          let acc = self#structure x0 acc in
          acc
        | Pmod_ident x0 ->
          let acc = self#longident_loc x0 acc in
          acc
    method module_expr : Module_expr.t -> 'acc -> 'acc  =
      fun module_expr acc ->
        let concrete = Module_expr.to_concrete module_expr in
        let { pmod_attributes; pmod_loc; pmod_desc } : Module_expr.concrete = concrete in
        let acc = self#attributes pmod_attributes acc in
        let acc = self#location pmod_loc acc in
        let acc = self#module_expr_desc pmod_desc acc in
        acc
    method with_constraint : With_constraint.t -> 'acc -> 'acc  =
      fun with_constraint acc ->
        let concrete = With_constraint.to_concrete with_constraint in
        match (concrete : With_constraint.concrete) with
        | Pwith_modsubst (x0, x1) ->
          let acc = self#longident_loc x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
        | Pwith_typesubst (x0, x1) ->
          let acc = self#type_declaration x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
        | Pwith_module (x0, x1) ->
          let acc = self#longident_loc x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
        | Pwith_type (x0, x1) ->
          let acc = self#type_declaration x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
    method include_declaration : Include_declaration.t -> 'acc -> 'acc  =
      fun include_declaration acc ->
        let concrete = Include_declaration.to_concrete include_declaration in
        let acc = self#include_infos_module_expr concrete acc in
        acc
    method include_description : Include_description.t -> 'acc -> 'acc  =
      fun include_description acc ->
        let concrete = Include_description.to_concrete include_description in
        let acc = self#include_infos_module_type concrete acc in
        acc
    method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> 'acc  =
      fun include_infos acc ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_expr.t Include_infos.concrete = concrete in
        let acc = self#attributes pincl_attributes acc in
        let acc = self#location pincl_loc acc in
        let acc = self#module_expr pincl_mod acc in
        acc
    method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> 'acc  =
      fun include_infos acc ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_type.t Include_infos.concrete = concrete in
        let acc = self#attributes pincl_attributes acc in
        let acc = self#location pincl_loc acc in
        let acc = self#module_type pincl_mod acc in
        acc
    method open_description : Open_description.t -> 'acc -> 'acc  =
      fun open_description acc ->
        let concrete = Open_description.to_concrete open_description in
        let { popen_attributes; popen_loc; popen_override; popen_lid } : Open_description.concrete = concrete in
        let acc = self#attributes popen_attributes acc in
        let acc = self#location popen_loc acc in
        let acc = self#override_flag popen_override acc in
        let acc = self#longident_loc popen_lid acc in
        acc
    method module_type_declaration : Module_type_declaration.t -> 'acc -> 'acc  =
      fun module_type_declaration acc ->
        let concrete = Module_type_declaration.to_concrete module_type_declaration in
        let { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name } : Module_type_declaration.concrete = concrete in
        let acc = self#location pmtd_loc acc in
        let acc = self#attributes pmtd_attributes acc in
        let acc = self#option self#module_type pmtd_type acc in
        let acc = self#loc self#string pmtd_name acc in
        acc
    method module_declaration : Module_declaration.t -> 'acc -> 'acc  =
      fun module_declaration acc ->
        let concrete = Module_declaration.to_concrete module_declaration in
        let { pmd_loc; pmd_attributes; pmd_type; pmd_name } : Module_declaration.concrete = concrete in
        let acc = self#location pmd_loc acc in
        let acc = self#attributes pmd_attributes acc in
        let acc = self#module_type pmd_type acc in
        let acc = self#loc self#string pmd_name acc in
        acc
    method signature_item_desc : Signature_item_desc.t -> 'acc -> 'acc  =
      fun signature_item_desc acc ->
        let concrete = Signature_item_desc.to_concrete signature_item_desc in
        match (concrete : Signature_item_desc.concrete) with
        | Psig_extension (x0, x1) ->
          let acc = self#attributes x0 acc in
          let acc = self#extension x1 acc in
          acc
        | Psig_attribute x0 ->
          let acc = self#attribute x0 acc in
          acc
        | Psig_class_type x0 ->
          let acc = self#list self#class_type_declaration x0 acc in
          acc
        | Psig_class x0 ->
          let acc = self#list self#class_description x0 acc in
          acc
        | Psig_include x0 ->
          let acc = self#include_description x0 acc in
          acc
        | Psig_open x0 ->
          let acc = self#open_description x0 acc in
          acc
        | Psig_modtype x0 ->
          let acc = self#module_type_declaration x0 acc in
          acc
        | Psig_recmodule x0 ->
          let acc = self#list self#module_declaration x0 acc in
          acc
        | Psig_module x0 ->
          let acc = self#module_declaration x0 acc in
          acc
        | Psig_exception x0 ->
          let acc = self#extension_constructor x0 acc in
          acc
        | Psig_typext x0 ->
          let acc = self#type_extension x0 acc in
          acc
        | Psig_type (x0, x1) ->
          let acc = self#list self#type_declaration x0 acc in
          let acc = self#rec_flag x1 acc in
          acc
        | Psig_value x0 ->
          let acc = self#value_description x0 acc in
          acc
    method signature_item : Signature_item.t -> 'acc -> 'acc  =
      fun signature_item acc ->
        let concrete = Signature_item.to_concrete signature_item in
        let { psig_loc; psig_desc } : Signature_item.concrete = concrete in
        let acc = self#location psig_loc acc in
        let acc = self#signature_item_desc psig_desc acc in
        acc
    method signature : Signature.t -> 'acc -> 'acc  =
      fun signature acc ->
        let concrete = Signature.to_concrete signature in
        let acc = self#list self#signature_item concrete acc in
        acc
    method module_type_desc : Module_type_desc.t -> 'acc -> 'acc  =
      fun module_type_desc acc ->
        let concrete = Module_type_desc.to_concrete module_type_desc in
        match (concrete : Module_type_desc.concrete) with
        | Pmty_alias x0 ->
          let acc = self#longident_loc x0 acc in
          acc
        | Pmty_extension x0 ->
          let acc = self#extension x0 acc in
          acc
        | Pmty_typeof x0 ->
          let acc = self#module_expr x0 acc in
          acc
        | Pmty_with (x0, x1) ->
          let acc = self#list self#with_constraint x0 acc in
          let acc = self#module_type x1 acc in
          acc
        | Pmty_functor (x0, x1, x2) ->
          let acc = self#module_type x0 acc in
          let acc = self#option self#module_type x1 acc in
          let acc = self#loc self#string x2 acc in
          acc
        | Pmty_signature x0 ->
          let acc = self#signature x0 acc in
          acc
        | Pmty_ident x0 ->
          let acc = self#longident_loc x0 acc in
          acc
    method module_type : Module_type.t -> 'acc -> 'acc  =
      fun module_type acc ->
        let concrete = Module_type.to_concrete module_type in
        let { pmty_attributes; pmty_loc; pmty_desc } : Module_type.concrete = concrete in
        let acc = self#attributes pmty_attributes acc in
        let acc = self#location pmty_loc acc in
        let acc = self#module_type_desc pmty_desc acc in
        acc
    method class_declaration : Class_declaration.t -> 'acc -> 'acc  =
      fun class_declaration acc ->
        let concrete = Class_declaration.to_concrete class_declaration in
        let acc = self#class_infos_class_expr concrete acc in
        acc
    method class_field_kind : Class_field_kind.t -> 'acc -> 'acc  =
      fun class_field_kind acc ->
        let concrete = Class_field_kind.to_concrete class_field_kind in
        match (concrete : Class_field_kind.concrete) with
        | Cfk_concrete (x0, x1) ->
          let acc = self#expression x0 acc in
          let acc = self#override_flag x1 acc in
          acc
        | Cfk_virtual x0 ->
          let acc = self#core_type x0 acc in
          acc
    method class_field_desc : Class_field_desc.t -> 'acc -> 'acc  =
      fun class_field_desc acc ->
        let concrete = Class_field_desc.to_concrete class_field_desc in
        match (concrete : Class_field_desc.concrete) with
        | Pcf_extension x0 ->
          let acc = self#extension x0 acc in
          acc
        | Pcf_attribute x0 ->
          let acc = self#attribute x0 acc in
          acc
        | Pcf_initializer x0 ->
          let acc = self#expression x0 acc in
          acc
        | Pcf_constraint x0 ->
          let acc = (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#core_type x1 acc in acc) x0 acc in
          acc
        | Pcf_method x0 ->
          let acc = (fun (x0, x1, x2) acc -> let acc = self#class_field_kind x0 acc in let acc = self#private_flag x1 acc in let acc = self#loc self#string x2 acc in acc) x0 acc in
          acc
        | Pcf_val x0 ->
          let acc = (fun (x0, x1, x2) acc -> let acc = self#class_field_kind x0 acc in let acc = self#mutable_flag x1 acc in let acc = self#loc self#string x2 acc in acc) x0 acc in
          acc
        | Pcf_inherit (x0, x1, x2) ->
          let acc = self#option (self#loc self#string) x0 acc in
          let acc = self#class_expr x1 acc in
          let acc = self#override_flag x2 acc in
          acc
    method class_field : Class_field.t -> 'acc -> 'acc  =
      fun class_field acc ->
        let concrete = Class_field.to_concrete class_field in
        let { pcf_attributes; pcf_loc; pcf_desc } : Class_field.concrete = concrete in
        let acc = self#attributes pcf_attributes acc in
        let acc = self#location pcf_loc acc in
        let acc = self#class_field_desc pcf_desc acc in
        acc
    method class_structure : Class_structure.t -> 'acc -> 'acc  =
      fun class_structure acc ->
        let concrete = Class_structure.to_concrete class_structure in
        let { pcstr_fields; pcstr_self } : Class_structure.concrete = concrete in
        let acc = self#list self#class_field pcstr_fields acc in
        let acc = self#pattern pcstr_self acc in
        acc
    method class_expr_desc : Class_expr_desc.t -> 'acc -> 'acc  =
      fun class_expr_desc acc ->
        let concrete = Class_expr_desc.to_concrete class_expr_desc in
        match (concrete : Class_expr_desc.concrete) with
        | Pcl_open (x0, x1, x2) ->
          let acc = self#class_expr x0 acc in
          let acc = self#longident_loc x1 acc in
          let acc = self#override_flag x2 acc in
          acc
        | Pcl_extension x0 ->
          let acc = self#extension x0 acc in
          acc
        | Pcl_constraint (x0, x1) ->
          let acc = self#class_type x0 acc in
          let acc = self#class_expr x1 acc in
          acc
        | Pcl_let (x0, x1, x2) ->
          let acc = self#class_expr x0 acc in
          let acc = self#list self#value_binding x1 acc in
          let acc = self#rec_flag x2 acc in
          acc
        | Pcl_apply (x0, x1) ->
          let acc = self#list (fun (x0, x1) acc -> let acc = self#expression x0 acc in let acc = self#arg_label x1 acc in acc) x0 acc in
          let acc = self#class_expr x1 acc in
          acc
        | Pcl_fun (x0, x1, x2, x3) ->
          let acc = self#class_expr x0 acc in
          let acc = self#pattern x1 acc in
          let acc = self#option self#expression x2 acc in
          let acc = self#arg_label x3 acc in
          acc
        | Pcl_structure x0 ->
          let acc = self#class_structure x0 acc in
          acc
        | Pcl_constr (x0, x1) ->
          let acc = self#list self#core_type x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
    method class_expr : Class_expr.t -> 'acc -> 'acc  =
      fun class_expr acc ->
        let concrete = Class_expr.to_concrete class_expr in
        let { pcl_attributes; pcl_loc; pcl_desc } : Class_expr.concrete = concrete in
        let acc = self#attributes pcl_attributes acc in
        let acc = self#location pcl_loc acc in
        let acc = self#class_expr_desc pcl_desc acc in
        acc
    method class_type_declaration : Class_type_declaration.t -> 'acc -> 'acc  =
      fun class_type_declaration acc ->
        let concrete = Class_type_declaration.to_concrete class_type_declaration in
        let acc = self#class_infos_class_type concrete acc in
        acc
    method class_description : Class_description.t -> 'acc -> 'acc  =
      fun class_description acc ->
        let concrete = Class_description.to_concrete class_description in
        let acc = self#class_infos_class_type concrete acc in
        acc
    method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> 'acc  =
      fun class_infos acc ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_expr.t Class_infos.concrete = concrete in
        let acc = self#attributes pci_attributes acc in
        let acc = self#location pci_loc acc in
        let acc = self#class_expr pci_expr acc in
        let acc = self#loc self#string pci_name acc in
        let acc = self#list (fun (x0, x1) acc -> let acc = self#variance x0 acc in let acc = self#core_type x1 acc in acc) pci_params acc in
        let acc = self#virtual_flag pci_virt acc in
        acc
    method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> 'acc  =
      fun class_infos acc ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_type.t Class_infos.concrete = concrete in
        let acc = self#attributes pci_attributes acc in
        let acc = self#location pci_loc acc in
        let acc = self#class_type pci_expr acc in
        let acc = self#loc self#string pci_name acc in
        let acc = self#list (fun (x0, x1) acc -> let acc = self#variance x0 acc in let acc = self#core_type x1 acc in acc) pci_params acc in
        let acc = self#virtual_flag pci_virt acc in
        acc
    method class_type_field_desc : Class_type_field_desc.t -> 'acc -> 'acc  =
      fun class_type_field_desc acc ->
        let concrete = Class_type_field_desc.to_concrete class_type_field_desc in
        match (concrete : Class_type_field_desc.concrete) with
        | Pctf_extension x0 ->
          let acc = self#extension x0 acc in
          acc
        | Pctf_attribute x0 ->
          let acc = self#attribute x0 acc in
          acc
        | Pctf_constraint x0 ->
          let acc = (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#core_type x1 acc in acc) x0 acc in
          acc
        | Pctf_method x0 ->
          let acc = (fun (x0, x1, x2, x3) acc -> let acc = self#core_type x0 acc in let acc = self#virtual_flag x1 acc in let acc = self#private_flag x2 acc in let acc = self#loc self#string x3 acc in acc) x0 acc in
          acc
        | Pctf_val x0 ->
          let acc = (fun (x0, x1, x2, x3) acc -> let acc = self#core_type x0 acc in let acc = self#virtual_flag x1 acc in let acc = self#mutable_flag x2 acc in let acc = self#loc self#string x3 acc in acc) x0 acc in
          acc
        | Pctf_inherit x0 ->
          let acc = self#class_type x0 acc in
          acc
    method class_type_field : Class_type_field.t -> 'acc -> 'acc  =
      fun class_type_field acc ->
        let concrete = Class_type_field.to_concrete class_type_field in
        let { pctf_attributes; pctf_loc; pctf_desc } : Class_type_field.concrete = concrete in
        let acc = self#attributes pctf_attributes acc in
        let acc = self#location pctf_loc acc in
        let acc = self#class_type_field_desc pctf_desc acc in
        acc
    method class_signature : Class_signature.t -> 'acc -> 'acc  =
      fun class_signature acc ->
        let concrete = Class_signature.to_concrete class_signature in
        let { pcsig_fields; pcsig_self } : Class_signature.concrete = concrete in
        let acc = self#list self#class_type_field pcsig_fields acc in
        let acc = self#core_type pcsig_self acc in
        acc
    method class_type_desc : Class_type_desc.t -> 'acc -> 'acc  =
      fun class_type_desc acc ->
        let concrete = Class_type_desc.to_concrete class_type_desc in
        match (concrete : Class_type_desc.concrete) with
        | Pcty_open (x0, x1, x2) ->
          let acc = self#class_type x0 acc in
          let acc = self#longident_loc x1 acc in
          let acc = self#override_flag x2 acc in
          acc
        | Pcty_extension x0 ->
          let acc = self#extension x0 acc in
          acc
        | Pcty_arrow (x0, x1, x2) ->
          let acc = self#class_type x0 acc in
          let acc = self#core_type x1 acc in
          let acc = self#arg_label x2 acc in
          acc
        | Pcty_signature x0 ->
          let acc = self#class_signature x0 acc in
          acc
        | Pcty_constr (x0, x1) ->
          let acc = self#list self#core_type x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
    method class_type : Class_type.t -> 'acc -> 'acc  =
      fun class_type acc ->
        let concrete = Class_type.to_concrete class_type in
        let { pcty_attributes; pcty_loc; pcty_desc } : Class_type.concrete = concrete in
        let acc = self#attributes pcty_attributes acc in
        let acc = self#location pcty_loc acc in
        let acc = self#class_type_desc pcty_desc acc in
        acc
    method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> 'acc  =
      fun extension_constructor_kind acc ->
        let concrete = Extension_constructor_kind.to_concrete extension_constructor_kind in
        match (concrete : Extension_constructor_kind.concrete) with
        | Pext_rebind x0 ->
          let acc = self#longident_loc x0 acc in
          acc
        | Pext_decl (x0, x1) ->
          let acc = self#option self#core_type x0 acc in
          let acc = self#constructor_arguments x1 acc in
          acc
    method extension_constructor : Extension_constructor.t -> 'acc -> 'acc  =
      fun extension_constructor acc ->
        let concrete = Extension_constructor.to_concrete extension_constructor in
        let { pext_attributes; pext_loc; pext_kind; pext_name } : Extension_constructor.concrete = concrete in
        let acc = self#attributes pext_attributes acc in
        let acc = self#location pext_loc acc in
        let acc = self#extension_constructor_kind pext_kind acc in
        let acc = self#loc self#string pext_name acc in
        acc
    method type_extension : Type_extension.t -> 'acc -> 'acc  =
      fun type_extension acc ->
        let concrete = Type_extension.to_concrete type_extension in
        let { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path } : Type_extension.concrete = concrete in
        let acc = self#attributes ptyext_attributes acc in
        let acc = self#private_flag ptyext_private acc in
        let acc = self#list self#extension_constructor ptyext_constructors acc in
        let acc = self#list (fun (x0, x1) acc -> let acc = self#variance x0 acc in let acc = self#core_type x1 acc in acc) ptyext_params acc in
        let acc = self#longident_loc ptyext_path acc in
        acc
    method constructor_arguments : Constructor_arguments.t -> 'acc -> 'acc  =
      fun constructor_arguments acc ->
        let concrete = Constructor_arguments.to_concrete constructor_arguments in
        match (concrete : Constructor_arguments.concrete) with
        | Pcstr_record x0 ->
          let acc = self#list self#label_declaration x0 acc in
          acc
        | Pcstr_tuple x0 ->
          let acc = self#list self#core_type x0 acc in
          acc
    method constructor_declaration : Constructor_declaration.t -> 'acc -> 'acc  =
      fun constructor_declaration acc ->
        let concrete = Constructor_declaration.to_concrete constructor_declaration in
        let { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name } : Constructor_declaration.concrete = concrete in
        let acc = self#attributes pcd_attributes acc in
        let acc = self#location pcd_loc acc in
        let acc = self#option self#core_type pcd_res acc in
        let acc = self#constructor_arguments pcd_args acc in
        let acc = self#loc self#string pcd_name acc in
        acc
    method label_declaration : Label_declaration.t -> 'acc -> 'acc  =
      fun label_declaration acc ->
        let concrete = Label_declaration.to_concrete label_declaration in
        let { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name } : Label_declaration.concrete = concrete in
        let acc = self#attributes pld_attributes acc in
        let acc = self#location pld_loc acc in
        let acc = self#core_type pld_type acc in
        let acc = self#mutable_flag pld_mutable acc in
        let acc = self#loc self#string pld_name acc in
        acc
    method type_kind : Type_kind.t -> 'acc -> 'acc  =
      fun type_kind acc ->
        let concrete = Type_kind.to_concrete type_kind in
        match (concrete : Type_kind.concrete) with
        | Ptype_open ->
          acc
        | Ptype_record x0 ->
          let acc = self#list self#label_declaration x0 acc in
          acc
        | Ptype_variant x0 ->
          let acc = self#list self#constructor_declaration x0 acc in
          acc
        | Ptype_abstract ->
          acc
    method type_declaration : Type_declaration.t -> 'acc -> 'acc  =
      fun type_declaration acc ->
        let concrete = Type_declaration.to_concrete type_declaration in
        let { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name } : Type_declaration.concrete = concrete in
        let acc = self#location ptype_loc acc in
        let acc = self#attributes ptype_attributes acc in
        let acc = self#option self#core_type ptype_manifest acc in
        let acc = self#private_flag ptype_private acc in
        let acc = self#type_kind ptype_kind acc in
        let acc = self#list (fun (x0, x1, x2) acc -> let acc = self#location x0 acc in let acc = self#core_type x1 acc in let acc = self#core_type x2 acc in acc) ptype_cstrs acc in
        let acc = self#list (fun (x0, x1) acc -> let acc = self#variance x0 acc in let acc = self#core_type x1 acc in acc) ptype_params acc in
        let acc = self#loc self#string ptype_name acc in
        acc
    method value_description : Value_description.t -> 'acc -> 'acc  =
      fun value_description acc ->
        let concrete = Value_description.to_concrete value_description in
        let { pval_loc; pval_attributes; pval_prim; pval_type; pval_name } : Value_description.concrete = concrete in
        let acc = self#location pval_loc acc in
        let acc = self#attributes pval_attributes acc in
        let acc = self#list self#string pval_prim acc in
        let acc = self#core_type pval_type acc in
        let acc = self#loc self#string pval_name acc in
        acc
    method case : Case.t -> 'acc -> 'acc  =
      fun case acc ->
        let concrete = Case.to_concrete case in
        let { pc_rhs; pc_guard; pc_lhs } : Case.concrete = concrete in
        let acc = self#expression pc_rhs acc in
        let acc = self#option self#expression pc_guard acc in
        let acc = self#pattern pc_lhs acc in
        acc
    method expression_desc : Expression_desc.t -> 'acc -> 'acc  =
      fun expression_desc acc ->
        let concrete = Expression_desc.to_concrete expression_desc in
        match (concrete : Expression_desc.concrete) with
        | Pexp_unreachable ->
          acc
        | Pexp_extension x0 ->
          let acc = self#extension x0 acc in
          acc
        | Pexp_open (x0, x1, x2) ->
          let acc = self#expression x0 acc in
          let acc = self#longident_loc x1 acc in
          let acc = self#override_flag x2 acc in
          acc
        | Pexp_pack x0 ->
          let acc = self#module_expr x0 acc in
          acc
        | Pexp_newtype (x0, x1) ->
          let acc = self#expression x0 acc in
          let acc = self#loc self#string x1 acc in
          acc
        | Pexp_object x0 ->
          let acc = self#class_structure x0 acc in
          acc
        | Pexp_poly (x0, x1) ->
          let acc = self#option self#core_type x0 acc in
          let acc = self#expression x1 acc in
          acc
        | Pexp_lazy x0 ->
          let acc = self#expression x0 acc in
          acc
        | Pexp_assert x0 ->
          let acc = self#expression x0 acc in
          acc
        | Pexp_letexception (x0, x1) ->
          let acc = self#expression x0 acc in
          let acc = self#extension_constructor x1 acc in
          acc
        | Pexp_letmodule (x0, x1, x2) ->
          let acc = self#expression x0 acc in
          let acc = self#module_expr x1 acc in
          let acc = self#loc self#string x2 acc in
          acc
        | Pexp_override x0 ->
          let acc = self#list (fun (x0, x1) acc -> let acc = self#expression x0 acc in let acc = self#loc self#string x1 acc in acc) x0 acc in
          acc
        | Pexp_setinstvar (x0, x1) ->
          let acc = self#expression x0 acc in
          let acc = self#loc self#string x1 acc in
          acc
        | Pexp_new x0 ->
          let acc = self#longident_loc x0 acc in
          acc
        | Pexp_send (x0, x1) ->
          let acc = self#loc self#string x0 acc in
          let acc = self#expression x1 acc in
          acc
        | Pexp_coerce (x0, x1, x2) ->
          let acc = self#core_type x0 acc in
          let acc = self#option self#core_type x1 acc in
          let acc = self#expression x2 acc in
          acc
        | Pexp_constraint (x0, x1) ->
          let acc = self#core_type x0 acc in
          let acc = self#expression x1 acc in
          acc
        | Pexp_for (x0, x1, x2, x3, x4) ->
          let acc = self#expression x0 acc in
          let acc = self#direction_flag x1 acc in
          let acc = self#expression x2 acc in
          let acc = self#expression x3 acc in
          let acc = self#pattern x4 acc in
          acc
        | Pexp_while (x0, x1) ->
          let acc = self#expression x0 acc in
          let acc = self#expression x1 acc in
          acc
        | Pexp_sequence (x0, x1) ->
          let acc = self#expression x0 acc in
          let acc = self#expression x1 acc in
          acc
        | Pexp_ifthenelse (x0, x1, x2) ->
          let acc = self#option self#expression x0 acc in
          let acc = self#expression x1 acc in
          let acc = self#expression x2 acc in
          acc
        | Pexp_array x0 ->
          let acc = self#list self#expression x0 acc in
          acc
        | Pexp_setfield (x0, x1, x2) ->
          let acc = self#expression x0 acc in
          let acc = self#longident_loc x1 acc in
          let acc = self#expression x2 acc in
          acc
        | Pexp_field (x0, x1) ->
          let acc = self#longident_loc x0 acc in
          let acc = self#expression x1 acc in
          acc
        | Pexp_record (x0, x1) ->
          let acc = self#option self#expression x0 acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#expression x0 acc in let acc = self#longident_loc x1 acc in acc) x1 acc in
          acc
        | Pexp_variant (x0, x1) ->
          let acc = self#option self#expression x0 acc in
          let acc = self#string x1 acc in
          acc
        | Pexp_construct (x0, x1) ->
          let acc = self#option self#expression x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
        | Pexp_tuple x0 ->
          let acc = self#list self#expression x0 acc in
          acc
        | Pexp_try (x0, x1) ->
          let acc = self#list self#case x0 acc in
          let acc = self#expression x1 acc in
          acc
        | Pexp_match (x0, x1) ->
          let acc = self#list self#case x0 acc in
          let acc = self#expression x1 acc in
          acc
        | Pexp_apply (x0, x1) ->
          let acc = self#list (fun (x0, x1) acc -> let acc = self#expression x0 acc in let acc = self#arg_label x1 acc in acc) x0 acc in
          let acc = self#expression x1 acc in
          acc
        | Pexp_fun (x0, x1, x2, x3) ->
          let acc = self#expression x0 acc in
          let acc = self#pattern x1 acc in
          let acc = self#option self#expression x2 acc in
          let acc = self#arg_label x3 acc in
          acc
        | Pexp_function x0 ->
          let acc = self#list self#case x0 acc in
          acc
        | Pexp_let (x0, x1, x2) ->
          let acc = self#expression x0 acc in
          let acc = self#list self#value_binding x1 acc in
          let acc = self#rec_flag x2 acc in
          acc
        | Pexp_constant x0 ->
          let acc = self#constant x0 acc in
          acc
        | Pexp_ident x0 ->
          let acc = self#longident_loc x0 acc in
          acc
    method expression : Expression.t -> 'acc -> 'acc  =
      fun expression acc ->
        let concrete = Expression.to_concrete expression in
        let { pexp_attributes; pexp_loc; pexp_desc } : Expression.concrete = concrete in
        let acc = self#attributes pexp_attributes acc in
        let acc = self#location pexp_loc acc in
        let acc = self#expression_desc pexp_desc acc in
        acc
    method pattern_desc : Pattern_desc.t -> 'acc -> 'acc  =
      fun pattern_desc acc ->
        let concrete = Pattern_desc.to_concrete pattern_desc in
        match (concrete : Pattern_desc.concrete) with
        | Ppat_open (x0, x1) ->
          let acc = self#pattern x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
        | Ppat_extension x0 ->
          let acc = self#extension x0 acc in
          acc
        | Ppat_exception x0 ->
          let acc = self#pattern x0 acc in
          acc
        | Ppat_unpack x0 ->
          let acc = self#loc self#string x0 acc in
          acc
        | Ppat_lazy x0 ->
          let acc = self#pattern x0 acc in
          acc
        | Ppat_type x0 ->
          let acc = self#longident_loc x0 acc in
          acc
        | Ppat_constraint (x0, x1) ->
          let acc = self#core_type x0 acc in
          let acc = self#pattern x1 acc in
          acc
        | Ppat_or (x0, x1) ->
          let acc = self#pattern x0 acc in
          let acc = self#pattern x1 acc in
          acc
        | Ppat_array x0 ->
          let acc = self#list self#pattern x0 acc in
          acc
        | Ppat_record (x0, x1) ->
          let acc = self#closed_flag x0 acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#pattern x0 acc in let acc = self#longident_loc x1 acc in acc) x1 acc in
          acc
        | Ppat_variant (x0, x1) ->
          let acc = self#option self#pattern x0 acc in
          let acc = self#string x1 acc in
          acc
        | Ppat_construct (x0, x1) ->
          let acc = self#option self#pattern x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
        | Ppat_tuple x0 ->
          let acc = self#list self#pattern x0 acc in
          acc
        | Ppat_interval (x0, x1) ->
          let acc = self#constant x0 acc in
          let acc = self#constant x1 acc in
          acc
        | Ppat_constant x0 ->
          let acc = self#constant x0 acc in
          acc
        | Ppat_alias (x0, x1) ->
          let acc = self#loc self#string x0 acc in
          let acc = self#pattern x1 acc in
          acc
        | Ppat_var x0 ->
          let acc = self#loc self#string x0 acc in
          acc
        | Ppat_any ->
          acc
    method pattern : Pattern.t -> 'acc -> 'acc  =
      fun pattern acc ->
        let concrete = Pattern.to_concrete pattern in
        let { ppat_attributes; ppat_loc; ppat_desc } : Pattern.concrete = concrete in
        let acc = self#attributes ppat_attributes acc in
        let acc = self#location ppat_loc acc in
        let acc = self#pattern_desc ppat_desc acc in
        acc
    method object_field : Object_field.t -> 'acc -> 'acc  =
      fun object_field acc ->
        let concrete = Object_field.to_concrete object_field in
        match (concrete : Object_field.concrete) with
        | Oinherit x0 ->
          let acc = self#core_type x0 acc in
          acc
        | Otag (x0, x1, x2) ->
          let acc = self#core_type x0 acc in
          let acc = self#attributes x1 acc in
          let acc = self#loc self#string x2 acc in
          acc
    method row_field : Row_field.t -> 'acc -> 'acc  =
      fun row_field acc ->
        let concrete = Row_field.to_concrete row_field in
        match (concrete : Row_field.concrete) with
        | Rinherit x0 ->
          let acc = self#core_type x0 acc in
          acc
        | Rtag (x0, x1, x2, x3) ->
          let acc = self#list self#core_type x0 acc in
          let acc = self#bool x1 acc in
          let acc = self#attributes x2 acc in
          let acc = self#loc self#string x3 acc in
          acc
    method package_type : Package_type.t -> 'acc -> 'acc  =
      fun package_type acc ->
        let concrete = Package_type.to_concrete package_type in
        let (x0, x1) = concrete in
        let acc = self#list (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#longident_loc x1 acc in acc) x0 acc in
        let acc = self#longident_loc x1 acc in
        acc
    method core_type_desc : Core_type_desc.t -> 'acc -> 'acc  =
      fun core_type_desc acc ->
        let concrete = Core_type_desc.to_concrete core_type_desc in
        match (concrete : Core_type_desc.concrete) with
        | Ptyp_extension x0 ->
          let acc = self#extension x0 acc in
          acc
        | Ptyp_package x0 ->
          let acc = self#package_type x0 acc in
          acc
        | Ptyp_poly (x0, x1) ->
          let acc = self#core_type x0 acc in
          let acc = self#list (self#loc self#string) x1 acc in
          acc
        | Ptyp_variant (x0, x1, x2) ->
          let acc = self#option (self#list self#string) x0 acc in
          let acc = self#closed_flag x1 acc in
          let acc = self#list self#row_field x2 acc in
          acc
        | Ptyp_alias (x0, x1) ->
          let acc = self#string x0 acc in
          let acc = self#core_type x1 acc in
          acc
        | Ptyp_class (x0, x1) ->
          let acc = self#list self#core_type x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
        | Ptyp_object (x0, x1) ->
          let acc = self#closed_flag x0 acc in
          let acc = self#list self#object_field x1 acc in
          acc
        | Ptyp_constr (x0, x1) ->
          let acc = self#list self#core_type x0 acc in
          let acc = self#longident_loc x1 acc in
          acc
        | Ptyp_tuple x0 ->
          let acc = self#list self#core_type x0 acc in
          acc
        | Ptyp_arrow (x0, x1, x2) ->
          let acc = self#core_type x0 acc in
          let acc = self#core_type x1 acc in
          let acc = self#arg_label x2 acc in
          acc
        | Ptyp_var x0 ->
          let acc = self#string x0 acc in
          acc
        | Ptyp_any ->
          acc
    method core_type : Core_type.t -> 'acc -> 'acc  =
      fun core_type acc ->
        let concrete = Core_type.to_concrete core_type in
        let { ptyp_attributes; ptyp_loc; ptyp_desc } : Core_type.concrete = concrete in
        let acc = self#attributes ptyp_attributes acc in
        let acc = self#location ptyp_loc acc in
        let acc = self#core_type_desc ptyp_desc acc in
        acc
    method payload : Payload.t -> 'acc -> 'acc  =
      fun payload acc ->
        let concrete = Payload.to_concrete payload in
        match (concrete : Payload.concrete) with
        | PPat (x0, x1) ->
          let acc = self#option self#expression x0 acc in
          let acc = self#pattern x1 acc in
          acc
        | PTyp x0 ->
          let acc = self#core_type x0 acc in
          acc
        | PSig x0 ->
          let acc = self#signature x0 acc in
          acc
        | PStr x0 ->
          let acc = self#structure x0 acc in
          acc
    method attributes : Attributes.t -> 'acc -> 'acc  =
      fun attributes acc ->
        let concrete = Attributes.to_concrete attributes in
        let acc = self#list self#attribute concrete acc in
        acc
    method extension : Extension.t -> 'acc -> 'acc  =
      fun extension acc ->
        let concrete = Extension.to_concrete extension in
        let (x0, x1) = concrete in
        let acc = self#payload x0 acc in
        let acc = self#loc self#string x1 acc in
        acc
    method attribute : Attribute.t -> 'acc -> 'acc  =
      fun attribute acc ->
        let concrete = Attribute.to_concrete attribute in
        let (x0, x1) = concrete in
        let acc = self#payload x0 acc in
        let acc = self#loc self#string x1 acc in
        acc
    method constant : Constant.t -> 'acc -> 'acc  =
      fun constant acc ->
        let concrete = Constant.to_concrete constant in
        match (concrete : Constant.concrete) with
        | Pconst_float (x0, x1) ->
          let acc = self#option self#char x0 acc in
          let acc = self#string x1 acc in
          acc
        | Pconst_string (x0, x1) ->
          let acc = self#option self#string x0 acc in
          let acc = self#string x1 acc in
          acc
        | Pconst_char x0 ->
          let acc = self#char x0 acc in
          acc
        | Pconst_integer (x0, x1) ->
          let acc = self#option self#char x0 acc in
          let acc = self#string x1 acc in
          acc
    method variance : Variance.t -> 'acc -> 'acc  =
      fun variance acc ->
        let concrete = Variance.to_concrete variance in
        match (concrete : Variance.concrete) with
        | Invariant ->
          acc
        | Contravariant ->
          acc
        | Covariant ->
          acc
    method arg_label : Arg_label.t -> 'acc -> 'acc  =
      fun arg_label acc ->
        let concrete = Arg_label.to_concrete arg_label in
        match (concrete : Arg_label.concrete) with
        | Optional x0 ->
          let acc = self#string x0 acc in
          acc
        | Labelled x0 ->
          let acc = self#string x0 acc in
          acc
        | Nolabel ->
          acc
    method closed_flag : Closed_flag.t -> 'acc -> 'acc  =
      fun closed_flag acc ->
        let concrete = Closed_flag.to_concrete closed_flag in
        match (concrete : Closed_flag.concrete) with
        | Open ->
          acc
        | Closed ->
          acc
    method override_flag : Override_flag.t -> 'acc -> 'acc  =
      fun override_flag acc ->
        let concrete = Override_flag.to_concrete override_flag in
        match (concrete : Override_flag.concrete) with
        | Fresh ->
          acc
        | Override ->
          acc
    method virtual_flag : Virtual_flag.t -> 'acc -> 'acc  =
      fun virtual_flag acc ->
        let concrete = Virtual_flag.to_concrete virtual_flag in
        match (concrete : Virtual_flag.concrete) with
        | Concrete ->
          acc
        | Virtual ->
          acc
    method mutable_flag : Mutable_flag.t -> 'acc -> 'acc  =
      fun mutable_flag acc ->
        let concrete = Mutable_flag.to_concrete mutable_flag in
        match (concrete : Mutable_flag.concrete) with
        | Mutable ->
          acc
        | Immutable ->
          acc
    method private_flag : Private_flag.t -> 'acc -> 'acc  =
      fun private_flag acc ->
        let concrete = Private_flag.to_concrete private_flag in
        match (concrete : Private_flag.concrete) with
        | Public ->
          acc
        | Private ->
          acc
    method direction_flag : Direction_flag.t -> 'acc -> 'acc  =
      fun direction_flag acc ->
        let concrete = Direction_flag.to_concrete direction_flag in
        match (concrete : Direction_flag.concrete) with
        | Downto ->
          acc
        | Upto ->
          acc
    method rec_flag : Rec_flag.t -> 'acc -> 'acc  =
      fun rec_flag acc ->
        let concrete = Rec_flag.to_concrete rec_flag in
        match (concrete : Rec_flag.concrete) with
        | Recursive ->
          acc
        | Nonrecursive ->
          acc
    method longident_loc : Longident_loc.t -> 'acc -> 'acc  =
      fun longident_loc acc ->
        let concrete = Longident_loc.to_concrete longident_loc in
        let acc = self#loc self#longident concrete acc in
        acc
    method longident : Longident.t -> 'acc -> 'acc  =
      fun longident acc ->
        let concrete = Longident.to_concrete longident in
        match (concrete : Longident.concrete) with
        | Lapply (x0, x1) ->
          let acc = self#longident x0 acc in
          let acc = self#longident x1 acc in
          acc
        | Ldot (x0, x1) ->
          let acc = self#string x0 acc in
          let acc = self#longident x1 acc in
          acc
        | Lident x0 ->
          let acc = self#string x0 acc in
          acc
  end

class virtual ['acc] fold_map =
  object (self)
    method virtual bool : bool -> 'acc -> (bool * 'acc)
    method virtual char : char -> 'acc -> (char * 'acc)
    method virtual int : int -> 'acc -> (int * 'acc)
    method virtual list : 'a . ('a -> 'acc -> ('a * 'acc)) -> 'a list -> 'acc -> ('a list * 'acc)
    method virtual option : 'a . ('a -> 'acc -> ('a * 'acc)) -> 'a option -> 'acc -> ('a option * 'acc)
    method virtual string : string -> 'acc -> (string * 'acc)
    method virtual location : Astlib.Location.t -> 'acc -> (Astlib.Location.t * 'acc)
    method virtual loc : 'a . ('a -> 'acc -> ('a * 'acc)) -> 'a Astlib.Loc.t -> 'acc -> ('a Astlib.Loc.t * 'acc)
    method directive_argument : Directive_argument.t -> 'acc -> (Directive_argument.t * 'acc)  =
      fun directive_argument acc ->
        let concrete = Directive_argument.to_concrete directive_argument in
        match (concrete : Directive_argument.concrete) with
        | Pdir_bool x0 ->
          let (x0, acc) = self#bool x0 acc in
          (Directive_argument.of_concrete (Pdir_bool x0), acc)
        | Pdir_ident x0 ->
          let (x0, acc) = self#longident x0 acc in
          (Directive_argument.of_concrete (Pdir_ident x0), acc)
        | Pdir_int (x0, x1) ->
          let (x0, acc) = self#option self#char x0 acc in
          let (x1, acc) = self#string x1 acc in
          (Directive_argument.of_concrete (Pdir_int (x0, x1)), acc)
        | Pdir_string x0 ->
          let (x0, acc) = self#string x0 acc in
          (Directive_argument.of_concrete (Pdir_string x0), acc)
        | Pdir_none ->
          (Directive_argument.of_concrete Pdir_none, acc)
    method toplevel_phrase : Toplevel_phrase.t -> 'acc -> (Toplevel_phrase.t * 'acc)  =
      fun toplevel_phrase acc ->
        let concrete = Toplevel_phrase.to_concrete toplevel_phrase in
        match (concrete : Toplevel_phrase.concrete) with
        | Ptop_dir (x0, x1) ->
          let (x0, acc) = self#directive_argument x0 acc in
          let (x1, acc) = self#string x1 acc in
          (Toplevel_phrase.of_concrete (Ptop_dir (x0, x1)), acc)
        | Ptop_def x0 ->
          let (x0, acc) = self#structure x0 acc in
          (Toplevel_phrase.of_concrete (Ptop_def x0), acc)
    method module_binding : Module_binding.t -> 'acc -> (Module_binding.t * 'acc)  =
      fun module_binding acc ->
        let concrete = Module_binding.to_concrete module_binding in
        let { pmb_loc; pmb_attributes; pmb_expr; pmb_name } : Module_binding.concrete = concrete in
        let (pmb_loc, acc) = self#location pmb_loc acc in
        let (pmb_attributes, acc) = self#attributes pmb_attributes acc in
        let (pmb_expr, acc) = self#module_expr pmb_expr acc in
        let (pmb_name, acc) = self#loc self#string pmb_name acc in
        (Module_binding.of_concrete { pmb_loc; pmb_attributes; pmb_expr; pmb_name }, acc)
    method value_binding : Value_binding.t -> 'acc -> (Value_binding.t * 'acc)  =
      fun value_binding acc ->
        let concrete = Value_binding.to_concrete value_binding in
        let { pvb_loc; pvb_attributes; pvb_expr; pvb_pat } : Value_binding.concrete = concrete in
        let (pvb_loc, acc) = self#location pvb_loc acc in
        let (pvb_attributes, acc) = self#attributes pvb_attributes acc in
        let (pvb_expr, acc) = self#expression pvb_expr acc in
        let (pvb_pat, acc) = self#pattern pvb_pat acc in
        (Value_binding.of_concrete { pvb_loc; pvb_attributes; pvb_expr; pvb_pat }, acc)
    method structure_item_desc : Structure_item_desc.t -> 'acc -> (Structure_item_desc.t * 'acc)  =
      fun structure_item_desc acc ->
        let concrete = Structure_item_desc.to_concrete structure_item_desc in
        match (concrete : Structure_item_desc.concrete) with
        | Pstr_extension (x0, x1) ->
          let (x0, acc) = self#attributes x0 acc in
          let (x1, acc) = self#extension x1 acc in
          (Structure_item_desc.of_concrete (Pstr_extension (x0, x1)), acc)
        | Pstr_attribute x0 ->
          let (x0, acc) = self#attribute x0 acc in
          (Structure_item_desc.of_concrete (Pstr_attribute x0), acc)
        | Pstr_include x0 ->
          let (x0, acc) = self#include_declaration x0 acc in
          (Structure_item_desc.of_concrete (Pstr_include x0), acc)
        | Pstr_class_type x0 ->
          let (x0, acc) = self#list self#class_type_declaration x0 acc in
          (Structure_item_desc.of_concrete (Pstr_class_type x0), acc)
        | Pstr_class x0 ->
          let (x0, acc) = self#list self#class_declaration x0 acc in
          (Structure_item_desc.of_concrete (Pstr_class x0), acc)
        | Pstr_open x0 ->
          let (x0, acc) = self#open_description x0 acc in
          (Structure_item_desc.of_concrete (Pstr_open x0), acc)
        | Pstr_modtype x0 ->
          let (x0, acc) = self#module_type_declaration x0 acc in
          (Structure_item_desc.of_concrete (Pstr_modtype x0), acc)
        | Pstr_recmodule x0 ->
          let (x0, acc) = self#list self#module_binding x0 acc in
          (Structure_item_desc.of_concrete (Pstr_recmodule x0), acc)
        | Pstr_module x0 ->
          let (x0, acc) = self#module_binding x0 acc in
          (Structure_item_desc.of_concrete (Pstr_module x0), acc)
        | Pstr_exception x0 ->
          let (x0, acc) = self#extension_constructor x0 acc in
          (Structure_item_desc.of_concrete (Pstr_exception x0), acc)
        | Pstr_typext x0 ->
          let (x0, acc) = self#type_extension x0 acc in
          (Structure_item_desc.of_concrete (Pstr_typext x0), acc)
        | Pstr_type (x0, x1) ->
          let (x0, acc) = self#list self#type_declaration x0 acc in
          let (x1, acc) = self#rec_flag x1 acc in
          (Structure_item_desc.of_concrete (Pstr_type (x0, x1)), acc)
        | Pstr_primitive x0 ->
          let (x0, acc) = self#value_description x0 acc in
          (Structure_item_desc.of_concrete (Pstr_primitive x0), acc)
        | Pstr_value (x0, x1) ->
          let (x0, acc) = self#list self#value_binding x0 acc in
          let (x1, acc) = self#rec_flag x1 acc in
          (Structure_item_desc.of_concrete (Pstr_value (x0, x1)), acc)
        | Pstr_eval (x0, x1) ->
          let (x0, acc) = self#attributes x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Structure_item_desc.of_concrete (Pstr_eval (x0, x1)), acc)
    method structure_item : Structure_item.t -> 'acc -> (Structure_item.t * 'acc)  =
      fun structure_item acc ->
        let concrete = Structure_item.to_concrete structure_item in
        let { pstr_loc; pstr_desc } : Structure_item.concrete = concrete in
        let (pstr_loc, acc) = self#location pstr_loc acc in
        let (pstr_desc, acc) = self#structure_item_desc pstr_desc acc in
        (Structure_item.of_concrete { pstr_loc; pstr_desc }, acc)
    method structure : Structure.t -> 'acc -> (Structure.t * 'acc)  =
      fun structure acc ->
        let concrete = Structure.to_concrete structure in
        let (concrete, acc) = self#list self#structure_item concrete acc in
        (Structure.of_concrete concrete, acc)
    method module_expr_desc : Module_expr_desc.t -> 'acc -> (Module_expr_desc.t * 'acc)  =
      fun module_expr_desc acc ->
        let concrete = Module_expr_desc.to_concrete module_expr_desc in
        match (concrete : Module_expr_desc.concrete) with
        | Pmod_extension x0 ->
          let (x0, acc) = self#extension x0 acc in
          (Module_expr_desc.of_concrete (Pmod_extension x0), acc)
        | Pmod_unpack x0 ->
          let (x0, acc) = self#expression x0 acc in
          (Module_expr_desc.of_concrete (Pmod_unpack x0), acc)
        | Pmod_constraint (x0, x1) ->
          let (x0, acc) = self#module_type x0 acc in
          let (x1, acc) = self#module_expr x1 acc in
          (Module_expr_desc.of_concrete (Pmod_constraint (x0, x1)), acc)
        | Pmod_apply (x0, x1) ->
          let (x0, acc) = self#module_expr x0 acc in
          let (x1, acc) = self#module_expr x1 acc in
          (Module_expr_desc.of_concrete (Pmod_apply (x0, x1)), acc)
        | Pmod_functor (x0, x1, x2) ->
          let (x0, acc) = self#module_expr x0 acc in
          let (x1, acc) = self#option self#module_type x1 acc in
          let (x2, acc) = self#loc self#string x2 acc in
          (Module_expr_desc.of_concrete (Pmod_functor (x0, x1, x2)), acc)
        | Pmod_structure x0 ->
          let (x0, acc) = self#structure x0 acc in
          (Module_expr_desc.of_concrete (Pmod_structure x0), acc)
        | Pmod_ident x0 ->
          let (x0, acc) = self#longident_loc x0 acc in
          (Module_expr_desc.of_concrete (Pmod_ident x0), acc)
    method module_expr : Module_expr.t -> 'acc -> (Module_expr.t * 'acc)  =
      fun module_expr acc ->
        let concrete = Module_expr.to_concrete module_expr in
        let { pmod_attributes; pmod_loc; pmod_desc } : Module_expr.concrete = concrete in
        let (pmod_attributes, acc) = self#attributes pmod_attributes acc in
        let (pmod_loc, acc) = self#location pmod_loc acc in
        let (pmod_desc, acc) = self#module_expr_desc pmod_desc acc in
        (Module_expr.of_concrete { pmod_attributes; pmod_loc; pmod_desc }, acc)
    method with_constraint : With_constraint.t -> 'acc -> (With_constraint.t * 'acc)  =
      fun with_constraint acc ->
        let concrete = With_constraint.to_concrete with_constraint in
        match (concrete : With_constraint.concrete) with
        | Pwith_modsubst (x0, x1) ->
          let (x0, acc) = self#longident_loc x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (With_constraint.of_concrete (Pwith_modsubst (x0, x1)), acc)
        | Pwith_typesubst (x0, x1) ->
          let (x0, acc) = self#type_declaration x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (With_constraint.of_concrete (Pwith_typesubst (x0, x1)), acc)
        | Pwith_module (x0, x1) ->
          let (x0, acc) = self#longident_loc x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (With_constraint.of_concrete (Pwith_module (x0, x1)), acc)
        | Pwith_type (x0, x1) ->
          let (x0, acc) = self#type_declaration x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (With_constraint.of_concrete (Pwith_type (x0, x1)), acc)
    method include_declaration : Include_declaration.t -> 'acc -> (Include_declaration.t * 'acc)  =
      fun include_declaration acc ->
        let concrete = Include_declaration.to_concrete include_declaration in
        let (concrete, acc) = self#include_infos_module_expr concrete acc in
        (Include_declaration.of_concrete concrete, acc)
    method include_description : Include_description.t -> 'acc -> (Include_description.t * 'acc)  =
      fun include_description acc ->
        let concrete = Include_description.to_concrete include_description in
        let (concrete, acc) = self#include_infos_module_type concrete acc in
        (Include_description.of_concrete concrete, acc)
    method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> (Module_expr.t Include_infos.t * 'acc)  =
      fun include_infos acc ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_expr.t Include_infos.concrete = concrete in
        let (pincl_attributes, acc) = self#attributes pincl_attributes acc in
        let (pincl_loc, acc) = self#location pincl_loc acc in
        let (pincl_mod, acc) = self#module_expr pincl_mod acc in
        (Include_infos.of_concrete { pincl_attributes; pincl_loc; pincl_mod }, acc)
    method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> (Module_type.t Include_infos.t * 'acc)  =
      fun include_infos acc ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_type.t Include_infos.concrete = concrete in
        let (pincl_attributes, acc) = self#attributes pincl_attributes acc in
        let (pincl_loc, acc) = self#location pincl_loc acc in
        let (pincl_mod, acc) = self#module_type pincl_mod acc in
        (Include_infos.of_concrete { pincl_attributes; pincl_loc; pincl_mod }, acc)
    method open_description : Open_description.t -> 'acc -> (Open_description.t * 'acc)  =
      fun open_description acc ->
        let concrete = Open_description.to_concrete open_description in
        let { popen_attributes; popen_loc; popen_override; popen_lid } : Open_description.concrete = concrete in
        let (popen_attributes, acc) = self#attributes popen_attributes acc in
        let (popen_loc, acc) = self#location popen_loc acc in
        let (popen_override, acc) = self#override_flag popen_override acc in
        let (popen_lid, acc) = self#longident_loc popen_lid acc in
        (Open_description.of_concrete { popen_attributes; popen_loc; popen_override; popen_lid }, acc)
    method module_type_declaration : Module_type_declaration.t -> 'acc -> (Module_type_declaration.t * 'acc)  =
      fun module_type_declaration acc ->
        let concrete = Module_type_declaration.to_concrete module_type_declaration in
        let { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name } : Module_type_declaration.concrete = concrete in
        let (pmtd_loc, acc) = self#location pmtd_loc acc in
        let (pmtd_attributes, acc) = self#attributes pmtd_attributes acc in
        let (pmtd_type, acc) = self#option self#module_type pmtd_type acc in
        let (pmtd_name, acc) = self#loc self#string pmtd_name acc in
        (Module_type_declaration.of_concrete { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name }, acc)
    method module_declaration : Module_declaration.t -> 'acc -> (Module_declaration.t * 'acc)  =
      fun module_declaration acc ->
        let concrete = Module_declaration.to_concrete module_declaration in
        let { pmd_loc; pmd_attributes; pmd_type; pmd_name } : Module_declaration.concrete = concrete in
        let (pmd_loc, acc) = self#location pmd_loc acc in
        let (pmd_attributes, acc) = self#attributes pmd_attributes acc in
        let (pmd_type, acc) = self#module_type pmd_type acc in
        let (pmd_name, acc) = self#loc self#string pmd_name acc in
        (Module_declaration.of_concrete { pmd_loc; pmd_attributes; pmd_type; pmd_name }, acc)
    method signature_item_desc : Signature_item_desc.t -> 'acc -> (Signature_item_desc.t * 'acc)  =
      fun signature_item_desc acc ->
        let concrete = Signature_item_desc.to_concrete signature_item_desc in
        match (concrete : Signature_item_desc.concrete) with
        | Psig_extension (x0, x1) ->
          let (x0, acc) = self#attributes x0 acc in
          let (x1, acc) = self#extension x1 acc in
          (Signature_item_desc.of_concrete (Psig_extension (x0, x1)), acc)
        | Psig_attribute x0 ->
          let (x0, acc) = self#attribute x0 acc in
          (Signature_item_desc.of_concrete (Psig_attribute x0), acc)
        | Psig_class_type x0 ->
          let (x0, acc) = self#list self#class_type_declaration x0 acc in
          (Signature_item_desc.of_concrete (Psig_class_type x0), acc)
        | Psig_class x0 ->
          let (x0, acc) = self#list self#class_description x0 acc in
          (Signature_item_desc.of_concrete (Psig_class x0), acc)
        | Psig_include x0 ->
          let (x0, acc) = self#include_description x0 acc in
          (Signature_item_desc.of_concrete (Psig_include x0), acc)
        | Psig_open x0 ->
          let (x0, acc) = self#open_description x0 acc in
          (Signature_item_desc.of_concrete (Psig_open x0), acc)
        | Psig_modtype x0 ->
          let (x0, acc) = self#module_type_declaration x0 acc in
          (Signature_item_desc.of_concrete (Psig_modtype x0), acc)
        | Psig_recmodule x0 ->
          let (x0, acc) = self#list self#module_declaration x0 acc in
          (Signature_item_desc.of_concrete (Psig_recmodule x0), acc)
        | Psig_module x0 ->
          let (x0, acc) = self#module_declaration x0 acc in
          (Signature_item_desc.of_concrete (Psig_module x0), acc)
        | Psig_exception x0 ->
          let (x0, acc) = self#extension_constructor x0 acc in
          (Signature_item_desc.of_concrete (Psig_exception x0), acc)
        | Psig_typext x0 ->
          let (x0, acc) = self#type_extension x0 acc in
          (Signature_item_desc.of_concrete (Psig_typext x0), acc)
        | Psig_type (x0, x1) ->
          let (x0, acc) = self#list self#type_declaration x0 acc in
          let (x1, acc) = self#rec_flag x1 acc in
          (Signature_item_desc.of_concrete (Psig_type (x0, x1)), acc)
        | Psig_value x0 ->
          let (x0, acc) = self#value_description x0 acc in
          (Signature_item_desc.of_concrete (Psig_value x0), acc)
    method signature_item : Signature_item.t -> 'acc -> (Signature_item.t * 'acc)  =
      fun signature_item acc ->
        let concrete = Signature_item.to_concrete signature_item in
        let { psig_loc; psig_desc } : Signature_item.concrete = concrete in
        let (psig_loc, acc) = self#location psig_loc acc in
        let (psig_desc, acc) = self#signature_item_desc psig_desc acc in
        (Signature_item.of_concrete { psig_loc; psig_desc }, acc)
    method signature : Signature.t -> 'acc -> (Signature.t * 'acc)  =
      fun signature acc ->
        let concrete = Signature.to_concrete signature in
        let (concrete, acc) = self#list self#signature_item concrete acc in
        (Signature.of_concrete concrete, acc)
    method module_type_desc : Module_type_desc.t -> 'acc -> (Module_type_desc.t * 'acc)  =
      fun module_type_desc acc ->
        let concrete = Module_type_desc.to_concrete module_type_desc in
        match (concrete : Module_type_desc.concrete) with
        | Pmty_alias x0 ->
          let (x0, acc) = self#longident_loc x0 acc in
          (Module_type_desc.of_concrete (Pmty_alias x0), acc)
        | Pmty_extension x0 ->
          let (x0, acc) = self#extension x0 acc in
          (Module_type_desc.of_concrete (Pmty_extension x0), acc)
        | Pmty_typeof x0 ->
          let (x0, acc) = self#module_expr x0 acc in
          (Module_type_desc.of_concrete (Pmty_typeof x0), acc)
        | Pmty_with (x0, x1) ->
          let (x0, acc) = self#list self#with_constraint x0 acc in
          let (x1, acc) = self#module_type x1 acc in
          (Module_type_desc.of_concrete (Pmty_with (x0, x1)), acc)
        | Pmty_functor (x0, x1, x2) ->
          let (x0, acc) = self#module_type x0 acc in
          let (x1, acc) = self#option self#module_type x1 acc in
          let (x2, acc) = self#loc self#string x2 acc in
          (Module_type_desc.of_concrete (Pmty_functor (x0, x1, x2)), acc)
        | Pmty_signature x0 ->
          let (x0, acc) = self#signature x0 acc in
          (Module_type_desc.of_concrete (Pmty_signature x0), acc)
        | Pmty_ident x0 ->
          let (x0, acc) = self#longident_loc x0 acc in
          (Module_type_desc.of_concrete (Pmty_ident x0), acc)
    method module_type : Module_type.t -> 'acc -> (Module_type.t * 'acc)  =
      fun module_type acc ->
        let concrete = Module_type.to_concrete module_type in
        let { pmty_attributes; pmty_loc; pmty_desc } : Module_type.concrete = concrete in
        let (pmty_attributes, acc) = self#attributes pmty_attributes acc in
        let (pmty_loc, acc) = self#location pmty_loc acc in
        let (pmty_desc, acc) = self#module_type_desc pmty_desc acc in
        (Module_type.of_concrete { pmty_attributes; pmty_loc; pmty_desc }, acc)
    method class_declaration : Class_declaration.t -> 'acc -> (Class_declaration.t * 'acc)  =
      fun class_declaration acc ->
        let concrete = Class_declaration.to_concrete class_declaration in
        let (concrete, acc) = self#class_infos_class_expr concrete acc in
        (Class_declaration.of_concrete concrete, acc)
    method class_field_kind : Class_field_kind.t -> 'acc -> (Class_field_kind.t * 'acc)  =
      fun class_field_kind acc ->
        let concrete = Class_field_kind.to_concrete class_field_kind in
        match (concrete : Class_field_kind.concrete) with
        | Cfk_concrete (x0, x1) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#override_flag x1 acc in
          (Class_field_kind.of_concrete (Cfk_concrete (x0, x1)), acc)
        | Cfk_virtual x0 ->
          let (x0, acc) = self#core_type x0 acc in
          (Class_field_kind.of_concrete (Cfk_virtual x0), acc)
    method class_field_desc : Class_field_desc.t -> 'acc -> (Class_field_desc.t * 'acc)  =
      fun class_field_desc acc ->
        let concrete = Class_field_desc.to_concrete class_field_desc in
        match (concrete : Class_field_desc.concrete) with
        | Pcf_extension x0 ->
          let (x0, acc) = self#extension x0 acc in
          (Class_field_desc.of_concrete (Pcf_extension x0), acc)
        | Pcf_attribute x0 ->
          let (x0, acc) = self#attribute x0 acc in
          (Class_field_desc.of_concrete (Pcf_attribute x0), acc)
        | Pcf_initializer x0 ->
          let (x0, acc) = self#expression x0 acc in
          (Class_field_desc.of_concrete (Pcf_initializer x0), acc)
        | Pcf_constraint x0 ->
          let (x0, acc) = (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) x0 acc in
          (Class_field_desc.of_concrete (Pcf_constraint x0), acc)
        | Pcf_method x0 ->
          let (x0, acc) = (fun (x0, x1, x2) acc -> let (x0, acc) = self#class_field_kind x0 acc in let (x1, acc) = self#private_flag x1 acc in let (x2, acc) = self#loc self#string x2 acc in ((x0, x1, x2), acc)) x0 acc in
          (Class_field_desc.of_concrete (Pcf_method x0), acc)
        | Pcf_val x0 ->
          let (x0, acc) = (fun (x0, x1, x2) acc -> let (x0, acc) = self#class_field_kind x0 acc in let (x1, acc) = self#mutable_flag x1 acc in let (x2, acc) = self#loc self#string x2 acc in ((x0, x1, x2), acc)) x0 acc in
          (Class_field_desc.of_concrete (Pcf_val x0), acc)
        | Pcf_inherit (x0, x1, x2) ->
          let (x0, acc) = self#option (self#loc self#string) x0 acc in
          let (x1, acc) = self#class_expr x1 acc in
          let (x2, acc) = self#override_flag x2 acc in
          (Class_field_desc.of_concrete (Pcf_inherit (x0, x1, x2)), acc)
    method class_field : Class_field.t -> 'acc -> (Class_field.t * 'acc)  =
      fun class_field acc ->
        let concrete = Class_field.to_concrete class_field in
        let { pcf_attributes; pcf_loc; pcf_desc } : Class_field.concrete = concrete in
        let (pcf_attributes, acc) = self#attributes pcf_attributes acc in
        let (pcf_loc, acc) = self#location pcf_loc acc in
        let (pcf_desc, acc) = self#class_field_desc pcf_desc acc in
        (Class_field.of_concrete { pcf_attributes; pcf_loc; pcf_desc }, acc)
    method class_structure : Class_structure.t -> 'acc -> (Class_structure.t * 'acc)  =
      fun class_structure acc ->
        let concrete = Class_structure.to_concrete class_structure in
        let { pcstr_fields; pcstr_self } : Class_structure.concrete = concrete in
        let (pcstr_fields, acc) = self#list self#class_field pcstr_fields acc in
        let (pcstr_self, acc) = self#pattern pcstr_self acc in
        (Class_structure.of_concrete { pcstr_fields; pcstr_self }, acc)
    method class_expr_desc : Class_expr_desc.t -> 'acc -> (Class_expr_desc.t * 'acc)  =
      fun class_expr_desc acc ->
        let concrete = Class_expr_desc.to_concrete class_expr_desc in
        match (concrete : Class_expr_desc.concrete) with
        | Pcl_open (x0, x1, x2) ->
          let (x0, acc) = self#class_expr x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          let (x2, acc) = self#override_flag x2 acc in
          (Class_expr_desc.of_concrete (Pcl_open (x0, x1, x2)), acc)
        | Pcl_extension x0 ->
          let (x0, acc) = self#extension x0 acc in
          (Class_expr_desc.of_concrete (Pcl_extension x0), acc)
        | Pcl_constraint (x0, x1) ->
          let (x0, acc) = self#class_type x0 acc in
          let (x1, acc) = self#class_expr x1 acc in
          (Class_expr_desc.of_concrete (Pcl_constraint (x0, x1)), acc)
        | Pcl_let (x0, x1, x2) ->
          let (x0, acc) = self#class_expr x0 acc in
          let (x1, acc) = self#list self#value_binding x1 acc in
          let (x2, acc) = self#rec_flag x2 acc in
          (Class_expr_desc.of_concrete (Pcl_let (x0, x1, x2)), acc)
        | Pcl_apply (x0, x1) ->
          let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#expression x0 acc in let (x1, acc) = self#arg_label x1 acc in ((x0, x1), acc)) x0 acc in
          let (x1, acc) = self#class_expr x1 acc in
          (Class_expr_desc.of_concrete (Pcl_apply (x0, x1)), acc)
        | Pcl_fun (x0, x1, x2, x3) ->
          let (x0, acc) = self#class_expr x0 acc in
          let (x1, acc) = self#pattern x1 acc in
          let (x2, acc) = self#option self#expression x2 acc in
          let (x3, acc) = self#arg_label x3 acc in
          (Class_expr_desc.of_concrete (Pcl_fun (x0, x1, x2, x3)), acc)
        | Pcl_structure x0 ->
          let (x0, acc) = self#class_structure x0 acc in
          (Class_expr_desc.of_concrete (Pcl_structure x0), acc)
        | Pcl_constr (x0, x1) ->
          let (x0, acc) = self#list self#core_type x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (Class_expr_desc.of_concrete (Pcl_constr (x0, x1)), acc)
    method class_expr : Class_expr.t -> 'acc -> (Class_expr.t * 'acc)  =
      fun class_expr acc ->
        let concrete = Class_expr.to_concrete class_expr in
        let { pcl_attributes; pcl_loc; pcl_desc } : Class_expr.concrete = concrete in
        let (pcl_attributes, acc) = self#attributes pcl_attributes acc in
        let (pcl_loc, acc) = self#location pcl_loc acc in
        let (pcl_desc, acc) = self#class_expr_desc pcl_desc acc in
        (Class_expr.of_concrete { pcl_attributes; pcl_loc; pcl_desc }, acc)
    method class_type_declaration : Class_type_declaration.t -> 'acc -> (Class_type_declaration.t * 'acc)  =
      fun class_type_declaration acc ->
        let concrete = Class_type_declaration.to_concrete class_type_declaration in
        let (concrete, acc) = self#class_infos_class_type concrete acc in
        (Class_type_declaration.of_concrete concrete, acc)
    method class_description : Class_description.t -> 'acc -> (Class_description.t * 'acc)  =
      fun class_description acc ->
        let concrete = Class_description.to_concrete class_description in
        let (concrete, acc) = self#class_infos_class_type concrete acc in
        (Class_description.of_concrete concrete, acc)
    method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> (Class_expr.t Class_infos.t * 'acc)  =
      fun class_infos acc ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_expr.t Class_infos.concrete = concrete in
        let (pci_attributes, acc) = self#attributes pci_attributes acc in
        let (pci_loc, acc) = self#location pci_loc acc in
        let (pci_expr, acc) = self#class_expr pci_expr acc in
        let (pci_name, acc) = self#loc self#string pci_name acc in
        let (pci_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#variance x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) pci_params acc in
        let (pci_virt, acc) = self#virtual_flag pci_virt acc in
        (Class_infos.of_concrete { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt }, acc)
    method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> (Class_type.t Class_infos.t * 'acc)  =
      fun class_infos acc ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_type.t Class_infos.concrete = concrete in
        let (pci_attributes, acc) = self#attributes pci_attributes acc in
        let (pci_loc, acc) = self#location pci_loc acc in
        let (pci_expr, acc) = self#class_type pci_expr acc in
        let (pci_name, acc) = self#loc self#string pci_name acc in
        let (pci_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#variance x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) pci_params acc in
        let (pci_virt, acc) = self#virtual_flag pci_virt acc in
        (Class_infos.of_concrete { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt }, acc)
    method class_type_field_desc : Class_type_field_desc.t -> 'acc -> (Class_type_field_desc.t * 'acc)  =
      fun class_type_field_desc acc ->
        let concrete = Class_type_field_desc.to_concrete class_type_field_desc in
        match (concrete : Class_type_field_desc.concrete) with
        | Pctf_extension x0 ->
          let (x0, acc) = self#extension x0 acc in
          (Class_type_field_desc.of_concrete (Pctf_extension x0), acc)
        | Pctf_attribute x0 ->
          let (x0, acc) = self#attribute x0 acc in
          (Class_type_field_desc.of_concrete (Pctf_attribute x0), acc)
        | Pctf_constraint x0 ->
          let (x0, acc) = (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) x0 acc in
          (Class_type_field_desc.of_concrete (Pctf_constraint x0), acc)
        | Pctf_method x0 ->
          let (x0, acc) = (fun (x0, x1, x2, x3) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#virtual_flag x1 acc in let (x2, acc) = self#private_flag x2 acc in let (x3, acc) = self#loc self#string x3 acc in ((x0, x1, x2, x3), acc)) x0 acc in
          (Class_type_field_desc.of_concrete (Pctf_method x0), acc)
        | Pctf_val x0 ->
          let (x0, acc) = (fun (x0, x1, x2, x3) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#virtual_flag x1 acc in let (x2, acc) = self#mutable_flag x2 acc in let (x3, acc) = self#loc self#string x3 acc in ((x0, x1, x2, x3), acc)) x0 acc in
          (Class_type_field_desc.of_concrete (Pctf_val x0), acc)
        | Pctf_inherit x0 ->
          let (x0, acc) = self#class_type x0 acc in
          (Class_type_field_desc.of_concrete (Pctf_inherit x0), acc)
    method class_type_field : Class_type_field.t -> 'acc -> (Class_type_field.t * 'acc)  =
      fun class_type_field acc ->
        let concrete = Class_type_field.to_concrete class_type_field in
        let { pctf_attributes; pctf_loc; pctf_desc } : Class_type_field.concrete = concrete in
        let (pctf_attributes, acc) = self#attributes pctf_attributes acc in
        let (pctf_loc, acc) = self#location pctf_loc acc in
        let (pctf_desc, acc) = self#class_type_field_desc pctf_desc acc in
        (Class_type_field.of_concrete { pctf_attributes; pctf_loc; pctf_desc }, acc)
    method class_signature : Class_signature.t -> 'acc -> (Class_signature.t * 'acc)  =
      fun class_signature acc ->
        let concrete = Class_signature.to_concrete class_signature in
        let { pcsig_fields; pcsig_self } : Class_signature.concrete = concrete in
        let (pcsig_fields, acc) = self#list self#class_type_field pcsig_fields acc in
        let (pcsig_self, acc) = self#core_type pcsig_self acc in
        (Class_signature.of_concrete { pcsig_fields; pcsig_self }, acc)
    method class_type_desc : Class_type_desc.t -> 'acc -> (Class_type_desc.t * 'acc)  =
      fun class_type_desc acc ->
        let concrete = Class_type_desc.to_concrete class_type_desc in
        match (concrete : Class_type_desc.concrete) with
        | Pcty_open (x0, x1, x2) ->
          let (x0, acc) = self#class_type x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          let (x2, acc) = self#override_flag x2 acc in
          (Class_type_desc.of_concrete (Pcty_open (x0, x1, x2)), acc)
        | Pcty_extension x0 ->
          let (x0, acc) = self#extension x0 acc in
          (Class_type_desc.of_concrete (Pcty_extension x0), acc)
        | Pcty_arrow (x0, x1, x2) ->
          let (x0, acc) = self#class_type x0 acc in
          let (x1, acc) = self#core_type x1 acc in
          let (x2, acc) = self#arg_label x2 acc in
          (Class_type_desc.of_concrete (Pcty_arrow (x0, x1, x2)), acc)
        | Pcty_signature x0 ->
          let (x0, acc) = self#class_signature x0 acc in
          (Class_type_desc.of_concrete (Pcty_signature x0), acc)
        | Pcty_constr (x0, x1) ->
          let (x0, acc) = self#list self#core_type x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (Class_type_desc.of_concrete (Pcty_constr (x0, x1)), acc)
    method class_type : Class_type.t -> 'acc -> (Class_type.t * 'acc)  =
      fun class_type acc ->
        let concrete = Class_type.to_concrete class_type in
        let { pcty_attributes; pcty_loc; pcty_desc } : Class_type.concrete = concrete in
        let (pcty_attributes, acc) = self#attributes pcty_attributes acc in
        let (pcty_loc, acc) = self#location pcty_loc acc in
        let (pcty_desc, acc) = self#class_type_desc pcty_desc acc in
        (Class_type.of_concrete { pcty_attributes; pcty_loc; pcty_desc }, acc)
    method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> (Extension_constructor_kind.t * 'acc)  =
      fun extension_constructor_kind acc ->
        let concrete = Extension_constructor_kind.to_concrete extension_constructor_kind in
        match (concrete : Extension_constructor_kind.concrete) with
        | Pext_rebind x0 ->
          let (x0, acc) = self#longident_loc x0 acc in
          (Extension_constructor_kind.of_concrete (Pext_rebind x0), acc)
        | Pext_decl (x0, x1) ->
          let (x0, acc) = self#option self#core_type x0 acc in
          let (x1, acc) = self#constructor_arguments x1 acc in
          (Extension_constructor_kind.of_concrete (Pext_decl (x0, x1)), acc)
    method extension_constructor : Extension_constructor.t -> 'acc -> (Extension_constructor.t * 'acc)  =
      fun extension_constructor acc ->
        let concrete = Extension_constructor.to_concrete extension_constructor in
        let { pext_attributes; pext_loc; pext_kind; pext_name } : Extension_constructor.concrete = concrete in
        let (pext_attributes, acc) = self#attributes pext_attributes acc in
        let (pext_loc, acc) = self#location pext_loc acc in
        let (pext_kind, acc) = self#extension_constructor_kind pext_kind acc in
        let (pext_name, acc) = self#loc self#string pext_name acc in
        (Extension_constructor.of_concrete { pext_attributes; pext_loc; pext_kind; pext_name }, acc)
    method type_extension : Type_extension.t -> 'acc -> (Type_extension.t * 'acc)  =
      fun type_extension acc ->
        let concrete = Type_extension.to_concrete type_extension in
        let { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path } : Type_extension.concrete = concrete in
        let (ptyext_attributes, acc) = self#attributes ptyext_attributes acc in
        let (ptyext_private, acc) = self#private_flag ptyext_private acc in
        let (ptyext_constructors, acc) = self#list self#extension_constructor ptyext_constructors acc in
        let (ptyext_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#variance x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) ptyext_params acc in
        let (ptyext_path, acc) = self#longident_loc ptyext_path acc in
        (Type_extension.of_concrete { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path }, acc)
    method constructor_arguments : Constructor_arguments.t -> 'acc -> (Constructor_arguments.t * 'acc)  =
      fun constructor_arguments acc ->
        let concrete = Constructor_arguments.to_concrete constructor_arguments in
        match (concrete : Constructor_arguments.concrete) with
        | Pcstr_record x0 ->
          let (x0, acc) = self#list self#label_declaration x0 acc in
          (Constructor_arguments.of_concrete (Pcstr_record x0), acc)
        | Pcstr_tuple x0 ->
          let (x0, acc) = self#list self#core_type x0 acc in
          (Constructor_arguments.of_concrete (Pcstr_tuple x0), acc)
    method constructor_declaration : Constructor_declaration.t -> 'acc -> (Constructor_declaration.t * 'acc)  =
      fun constructor_declaration acc ->
        let concrete = Constructor_declaration.to_concrete constructor_declaration in
        let { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name } : Constructor_declaration.concrete = concrete in
        let (pcd_attributes, acc) = self#attributes pcd_attributes acc in
        let (pcd_loc, acc) = self#location pcd_loc acc in
        let (pcd_res, acc) = self#option self#core_type pcd_res acc in
        let (pcd_args, acc) = self#constructor_arguments pcd_args acc in
        let (pcd_name, acc) = self#loc self#string pcd_name acc in
        (Constructor_declaration.of_concrete { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name }, acc)
    method label_declaration : Label_declaration.t -> 'acc -> (Label_declaration.t * 'acc)  =
      fun label_declaration acc ->
        let concrete = Label_declaration.to_concrete label_declaration in
        let { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name } : Label_declaration.concrete = concrete in
        let (pld_attributes, acc) = self#attributes pld_attributes acc in
        let (pld_loc, acc) = self#location pld_loc acc in
        let (pld_type, acc) = self#core_type pld_type acc in
        let (pld_mutable, acc) = self#mutable_flag pld_mutable acc in
        let (pld_name, acc) = self#loc self#string pld_name acc in
        (Label_declaration.of_concrete { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name }, acc)
    method type_kind : Type_kind.t -> 'acc -> (Type_kind.t * 'acc)  =
      fun type_kind acc ->
        let concrete = Type_kind.to_concrete type_kind in
        match (concrete : Type_kind.concrete) with
        | Ptype_open ->
          (Type_kind.of_concrete Ptype_open, acc)
        | Ptype_record x0 ->
          let (x0, acc) = self#list self#label_declaration x0 acc in
          (Type_kind.of_concrete (Ptype_record x0), acc)
        | Ptype_variant x0 ->
          let (x0, acc) = self#list self#constructor_declaration x0 acc in
          (Type_kind.of_concrete (Ptype_variant x0), acc)
        | Ptype_abstract ->
          (Type_kind.of_concrete Ptype_abstract, acc)
    method type_declaration : Type_declaration.t -> 'acc -> (Type_declaration.t * 'acc)  =
      fun type_declaration acc ->
        let concrete = Type_declaration.to_concrete type_declaration in
        let { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name } : Type_declaration.concrete = concrete in
        let (ptype_loc, acc) = self#location ptype_loc acc in
        let (ptype_attributes, acc) = self#attributes ptype_attributes acc in
        let (ptype_manifest, acc) = self#option self#core_type ptype_manifest acc in
        let (ptype_private, acc) = self#private_flag ptype_private acc in
        let (ptype_kind, acc) = self#type_kind ptype_kind acc in
        let (ptype_cstrs, acc) = self#list (fun (x0, x1, x2) acc -> let (x0, acc) = self#location x0 acc in let (x1, acc) = self#core_type x1 acc in let (x2, acc) = self#core_type x2 acc in ((x0, x1, x2), acc)) ptype_cstrs acc in
        let (ptype_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#variance x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) ptype_params acc in
        let (ptype_name, acc) = self#loc self#string ptype_name acc in
        (Type_declaration.of_concrete { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name }, acc)
    method value_description : Value_description.t -> 'acc -> (Value_description.t * 'acc)  =
      fun value_description acc ->
        let concrete = Value_description.to_concrete value_description in
        let { pval_loc; pval_attributes; pval_prim; pval_type; pval_name } : Value_description.concrete = concrete in
        let (pval_loc, acc) = self#location pval_loc acc in
        let (pval_attributes, acc) = self#attributes pval_attributes acc in
        let (pval_prim, acc) = self#list self#string pval_prim acc in
        let (pval_type, acc) = self#core_type pval_type acc in
        let (pval_name, acc) = self#loc self#string pval_name acc in
        (Value_description.of_concrete { pval_loc; pval_attributes; pval_prim; pval_type; pval_name }, acc)
    method case : Case.t -> 'acc -> (Case.t * 'acc)  =
      fun case acc ->
        let concrete = Case.to_concrete case in
        let { pc_rhs; pc_guard; pc_lhs } : Case.concrete = concrete in
        let (pc_rhs, acc) = self#expression pc_rhs acc in
        let (pc_guard, acc) = self#option self#expression pc_guard acc in
        let (pc_lhs, acc) = self#pattern pc_lhs acc in
        (Case.of_concrete { pc_rhs; pc_guard; pc_lhs }, acc)
    method expression_desc : Expression_desc.t -> 'acc -> (Expression_desc.t * 'acc)  =
      fun expression_desc acc ->
        let concrete = Expression_desc.to_concrete expression_desc in
        match (concrete : Expression_desc.concrete) with
        | Pexp_unreachable ->
          (Expression_desc.of_concrete Pexp_unreachable, acc)
        | Pexp_extension x0 ->
          let (x0, acc) = self#extension x0 acc in
          (Expression_desc.of_concrete (Pexp_extension x0), acc)
        | Pexp_open (x0, x1, x2) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          let (x2, acc) = self#override_flag x2 acc in
          (Expression_desc.of_concrete (Pexp_open (x0, x1, x2)), acc)
        | Pexp_pack x0 ->
          let (x0, acc) = self#module_expr x0 acc in
          (Expression_desc.of_concrete (Pexp_pack x0), acc)
        | Pexp_newtype (x0, x1) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#loc self#string x1 acc in
          (Expression_desc.of_concrete (Pexp_newtype (x0, x1)), acc)
        | Pexp_object x0 ->
          let (x0, acc) = self#class_structure x0 acc in
          (Expression_desc.of_concrete (Pexp_object x0), acc)
        | Pexp_poly (x0, x1) ->
          let (x0, acc) = self#option self#core_type x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Expression_desc.of_concrete (Pexp_poly (x0, x1)), acc)
        | Pexp_lazy x0 ->
          let (x0, acc) = self#expression x0 acc in
          (Expression_desc.of_concrete (Pexp_lazy x0), acc)
        | Pexp_assert x0 ->
          let (x0, acc) = self#expression x0 acc in
          (Expression_desc.of_concrete (Pexp_assert x0), acc)
        | Pexp_letexception (x0, x1) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#extension_constructor x1 acc in
          (Expression_desc.of_concrete (Pexp_letexception (x0, x1)), acc)
        | Pexp_letmodule (x0, x1, x2) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#module_expr x1 acc in
          let (x2, acc) = self#loc self#string x2 acc in
          (Expression_desc.of_concrete (Pexp_letmodule (x0, x1, x2)), acc)
        | Pexp_override x0 ->
          let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#expression x0 acc in let (x1, acc) = self#loc self#string x1 acc in ((x0, x1), acc)) x0 acc in
          (Expression_desc.of_concrete (Pexp_override x0), acc)
        | Pexp_setinstvar (x0, x1) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#loc self#string x1 acc in
          (Expression_desc.of_concrete (Pexp_setinstvar (x0, x1)), acc)
        | Pexp_new x0 ->
          let (x0, acc) = self#longident_loc x0 acc in
          (Expression_desc.of_concrete (Pexp_new x0), acc)
        | Pexp_send (x0, x1) ->
          let (x0, acc) = self#loc self#string x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Expression_desc.of_concrete (Pexp_send (x0, x1)), acc)
        | Pexp_coerce (x0, x1, x2) ->
          let (x0, acc) = self#core_type x0 acc in
          let (x1, acc) = self#option self#core_type x1 acc in
          let (x2, acc) = self#expression x2 acc in
          (Expression_desc.of_concrete (Pexp_coerce (x0, x1, x2)), acc)
        | Pexp_constraint (x0, x1) ->
          let (x0, acc) = self#core_type x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Expression_desc.of_concrete (Pexp_constraint (x0, x1)), acc)
        | Pexp_for (x0, x1, x2, x3, x4) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#direction_flag x1 acc in
          let (x2, acc) = self#expression x2 acc in
          let (x3, acc) = self#expression x3 acc in
          let (x4, acc) = self#pattern x4 acc in
          (Expression_desc.of_concrete (Pexp_for (x0, x1, x2, x3, x4)), acc)
        | Pexp_while (x0, x1) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Expression_desc.of_concrete (Pexp_while (x0, x1)), acc)
        | Pexp_sequence (x0, x1) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Expression_desc.of_concrete (Pexp_sequence (x0, x1)), acc)
        | Pexp_ifthenelse (x0, x1, x2) ->
          let (x0, acc) = self#option self#expression x0 acc in
          let (x1, acc) = self#expression x1 acc in
          let (x2, acc) = self#expression x2 acc in
          (Expression_desc.of_concrete (Pexp_ifthenelse (x0, x1, x2)), acc)
        | Pexp_array x0 ->
          let (x0, acc) = self#list self#expression x0 acc in
          (Expression_desc.of_concrete (Pexp_array x0), acc)
        | Pexp_setfield (x0, x1, x2) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          let (x2, acc) = self#expression x2 acc in
          (Expression_desc.of_concrete (Pexp_setfield (x0, x1, x2)), acc)
        | Pexp_field (x0, x1) ->
          let (x0, acc) = self#longident_loc x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Expression_desc.of_concrete (Pexp_field (x0, x1)), acc)
        | Pexp_record (x0, x1) ->
          let (x0, acc) = self#option self#expression x0 acc in
          let (x1, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#expression x0 acc in let (x1, acc) = self#longident_loc x1 acc in ((x0, x1), acc)) x1 acc in
          (Expression_desc.of_concrete (Pexp_record (x0, x1)), acc)
        | Pexp_variant (x0, x1) ->
          let (x0, acc) = self#option self#expression x0 acc in
          let (x1, acc) = self#string x1 acc in
          (Expression_desc.of_concrete (Pexp_variant (x0, x1)), acc)
        | Pexp_construct (x0, x1) ->
          let (x0, acc) = self#option self#expression x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (Expression_desc.of_concrete (Pexp_construct (x0, x1)), acc)
        | Pexp_tuple x0 ->
          let (x0, acc) = self#list self#expression x0 acc in
          (Expression_desc.of_concrete (Pexp_tuple x0), acc)
        | Pexp_try (x0, x1) ->
          let (x0, acc) = self#list self#case x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Expression_desc.of_concrete (Pexp_try (x0, x1)), acc)
        | Pexp_match (x0, x1) ->
          let (x0, acc) = self#list self#case x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Expression_desc.of_concrete (Pexp_match (x0, x1)), acc)
        | Pexp_apply (x0, x1) ->
          let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#expression x0 acc in let (x1, acc) = self#arg_label x1 acc in ((x0, x1), acc)) x0 acc in
          let (x1, acc) = self#expression x1 acc in
          (Expression_desc.of_concrete (Pexp_apply (x0, x1)), acc)
        | Pexp_fun (x0, x1, x2, x3) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#pattern x1 acc in
          let (x2, acc) = self#option self#expression x2 acc in
          let (x3, acc) = self#arg_label x3 acc in
          (Expression_desc.of_concrete (Pexp_fun (x0, x1, x2, x3)), acc)
        | Pexp_function x0 ->
          let (x0, acc) = self#list self#case x0 acc in
          (Expression_desc.of_concrete (Pexp_function x0), acc)
        | Pexp_let (x0, x1, x2) ->
          let (x0, acc) = self#expression x0 acc in
          let (x1, acc) = self#list self#value_binding x1 acc in
          let (x2, acc) = self#rec_flag x2 acc in
          (Expression_desc.of_concrete (Pexp_let (x0, x1, x2)), acc)
        | Pexp_constant x0 ->
          let (x0, acc) = self#constant x0 acc in
          (Expression_desc.of_concrete (Pexp_constant x0), acc)
        | Pexp_ident x0 ->
          let (x0, acc) = self#longident_loc x0 acc in
          (Expression_desc.of_concrete (Pexp_ident x0), acc)
    method expression : Expression.t -> 'acc -> (Expression.t * 'acc)  =
      fun expression acc ->
        let concrete = Expression.to_concrete expression in
        let { pexp_attributes; pexp_loc; pexp_desc } : Expression.concrete = concrete in
        let (pexp_attributes, acc) = self#attributes pexp_attributes acc in
        let (pexp_loc, acc) = self#location pexp_loc acc in
        let (pexp_desc, acc) = self#expression_desc pexp_desc acc in
        (Expression.of_concrete { pexp_attributes; pexp_loc; pexp_desc }, acc)
    method pattern_desc : Pattern_desc.t -> 'acc -> (Pattern_desc.t * 'acc)  =
      fun pattern_desc acc ->
        let concrete = Pattern_desc.to_concrete pattern_desc in
        match (concrete : Pattern_desc.concrete) with
        | Ppat_open (x0, x1) ->
          let (x0, acc) = self#pattern x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (Pattern_desc.of_concrete (Ppat_open (x0, x1)), acc)
        | Ppat_extension x0 ->
          let (x0, acc) = self#extension x0 acc in
          (Pattern_desc.of_concrete (Ppat_extension x0), acc)
        | Ppat_exception x0 ->
          let (x0, acc) = self#pattern x0 acc in
          (Pattern_desc.of_concrete (Ppat_exception x0), acc)
        | Ppat_unpack x0 ->
          let (x0, acc) = self#loc self#string x0 acc in
          (Pattern_desc.of_concrete (Ppat_unpack x0), acc)
        | Ppat_lazy x0 ->
          let (x0, acc) = self#pattern x0 acc in
          (Pattern_desc.of_concrete (Ppat_lazy x0), acc)
        | Ppat_type x0 ->
          let (x0, acc) = self#longident_loc x0 acc in
          (Pattern_desc.of_concrete (Ppat_type x0), acc)
        | Ppat_constraint (x0, x1) ->
          let (x0, acc) = self#core_type x0 acc in
          let (x1, acc) = self#pattern x1 acc in
          (Pattern_desc.of_concrete (Ppat_constraint (x0, x1)), acc)
        | Ppat_or (x0, x1) ->
          let (x0, acc) = self#pattern x0 acc in
          let (x1, acc) = self#pattern x1 acc in
          (Pattern_desc.of_concrete (Ppat_or (x0, x1)), acc)
        | Ppat_array x0 ->
          let (x0, acc) = self#list self#pattern x0 acc in
          (Pattern_desc.of_concrete (Ppat_array x0), acc)
        | Ppat_record (x0, x1) ->
          let (x0, acc) = self#closed_flag x0 acc in
          let (x1, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#pattern x0 acc in let (x1, acc) = self#longident_loc x1 acc in ((x0, x1), acc)) x1 acc in
          (Pattern_desc.of_concrete (Ppat_record (x0, x1)), acc)
        | Ppat_variant (x0, x1) ->
          let (x0, acc) = self#option self#pattern x0 acc in
          let (x1, acc) = self#string x1 acc in
          (Pattern_desc.of_concrete (Ppat_variant (x0, x1)), acc)
        | Ppat_construct (x0, x1) ->
          let (x0, acc) = self#option self#pattern x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (Pattern_desc.of_concrete (Ppat_construct (x0, x1)), acc)
        | Ppat_tuple x0 ->
          let (x0, acc) = self#list self#pattern x0 acc in
          (Pattern_desc.of_concrete (Ppat_tuple x0), acc)
        | Ppat_interval (x0, x1) ->
          let (x0, acc) = self#constant x0 acc in
          let (x1, acc) = self#constant x1 acc in
          (Pattern_desc.of_concrete (Ppat_interval (x0, x1)), acc)
        | Ppat_constant x0 ->
          let (x0, acc) = self#constant x0 acc in
          (Pattern_desc.of_concrete (Ppat_constant x0), acc)
        | Ppat_alias (x0, x1) ->
          let (x0, acc) = self#loc self#string x0 acc in
          let (x1, acc) = self#pattern x1 acc in
          (Pattern_desc.of_concrete (Ppat_alias (x0, x1)), acc)
        | Ppat_var x0 ->
          let (x0, acc) = self#loc self#string x0 acc in
          (Pattern_desc.of_concrete (Ppat_var x0), acc)
        | Ppat_any ->
          (Pattern_desc.of_concrete Ppat_any, acc)
    method pattern : Pattern.t -> 'acc -> (Pattern.t * 'acc)  =
      fun pattern acc ->
        let concrete = Pattern.to_concrete pattern in
        let { ppat_attributes; ppat_loc; ppat_desc } : Pattern.concrete = concrete in
        let (ppat_attributes, acc) = self#attributes ppat_attributes acc in
        let (ppat_loc, acc) = self#location ppat_loc acc in
        let (ppat_desc, acc) = self#pattern_desc ppat_desc acc in
        (Pattern.of_concrete { ppat_attributes; ppat_loc; ppat_desc }, acc)
    method object_field : Object_field.t -> 'acc -> (Object_field.t * 'acc)  =
      fun object_field acc ->
        let concrete = Object_field.to_concrete object_field in
        match (concrete : Object_field.concrete) with
        | Oinherit x0 ->
          let (x0, acc) = self#core_type x0 acc in
          (Object_field.of_concrete (Oinherit x0), acc)
        | Otag (x0, x1, x2) ->
          let (x0, acc) = self#core_type x0 acc in
          let (x1, acc) = self#attributes x1 acc in
          let (x2, acc) = self#loc self#string x2 acc in
          (Object_field.of_concrete (Otag (x0, x1, x2)), acc)
    method row_field : Row_field.t -> 'acc -> (Row_field.t * 'acc)  =
      fun row_field acc ->
        let concrete = Row_field.to_concrete row_field in
        match (concrete : Row_field.concrete) with
        | Rinherit x0 ->
          let (x0, acc) = self#core_type x0 acc in
          (Row_field.of_concrete (Rinherit x0), acc)
        | Rtag (x0, x1, x2, x3) ->
          let (x0, acc) = self#list self#core_type x0 acc in
          let (x1, acc) = self#bool x1 acc in
          let (x2, acc) = self#attributes x2 acc in
          let (x3, acc) = self#loc self#string x3 acc in
          (Row_field.of_concrete (Rtag (x0, x1, x2, x3)), acc)
    method package_type : Package_type.t -> 'acc -> (Package_type.t * 'acc)  =
      fun package_type acc ->
        let concrete = Package_type.to_concrete package_type in
        let (x0, x1) = concrete in
        let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#longident_loc x1 acc in ((x0, x1), acc)) x0 acc in
        let (x1, acc) = self#longident_loc x1 acc in
        (Package_type.of_concrete (x0, x1), acc)
    method core_type_desc : Core_type_desc.t -> 'acc -> (Core_type_desc.t * 'acc)  =
      fun core_type_desc acc ->
        let concrete = Core_type_desc.to_concrete core_type_desc in
        match (concrete : Core_type_desc.concrete) with
        | Ptyp_extension x0 ->
          let (x0, acc) = self#extension x0 acc in
          (Core_type_desc.of_concrete (Ptyp_extension x0), acc)
        | Ptyp_package x0 ->
          let (x0, acc) = self#package_type x0 acc in
          (Core_type_desc.of_concrete (Ptyp_package x0), acc)
        | Ptyp_poly (x0, x1) ->
          let (x0, acc) = self#core_type x0 acc in
          let (x1, acc) = self#list (self#loc self#string) x1 acc in
          (Core_type_desc.of_concrete (Ptyp_poly (x0, x1)), acc)
        | Ptyp_variant (x0, x1, x2) ->
          let (x0, acc) = self#option (self#list self#string) x0 acc in
          let (x1, acc) = self#closed_flag x1 acc in
          let (x2, acc) = self#list self#row_field x2 acc in
          (Core_type_desc.of_concrete (Ptyp_variant (x0, x1, x2)), acc)
        | Ptyp_alias (x0, x1) ->
          let (x0, acc) = self#string x0 acc in
          let (x1, acc) = self#core_type x1 acc in
          (Core_type_desc.of_concrete (Ptyp_alias (x0, x1)), acc)
        | Ptyp_class (x0, x1) ->
          let (x0, acc) = self#list self#core_type x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (Core_type_desc.of_concrete (Ptyp_class (x0, x1)), acc)
        | Ptyp_object (x0, x1) ->
          let (x0, acc) = self#closed_flag x0 acc in
          let (x1, acc) = self#list self#object_field x1 acc in
          (Core_type_desc.of_concrete (Ptyp_object (x0, x1)), acc)
        | Ptyp_constr (x0, x1) ->
          let (x0, acc) = self#list self#core_type x0 acc in
          let (x1, acc) = self#longident_loc x1 acc in
          (Core_type_desc.of_concrete (Ptyp_constr (x0, x1)), acc)
        | Ptyp_tuple x0 ->
          let (x0, acc) = self#list self#core_type x0 acc in
          (Core_type_desc.of_concrete (Ptyp_tuple x0), acc)
        | Ptyp_arrow (x0, x1, x2) ->
          let (x0, acc) = self#core_type x0 acc in
          let (x1, acc) = self#core_type x1 acc in
          let (x2, acc) = self#arg_label x2 acc in
          (Core_type_desc.of_concrete (Ptyp_arrow (x0, x1, x2)), acc)
        | Ptyp_var x0 ->
          let (x0, acc) = self#string x0 acc in
          (Core_type_desc.of_concrete (Ptyp_var x0), acc)
        | Ptyp_any ->
          (Core_type_desc.of_concrete Ptyp_any, acc)
    method core_type : Core_type.t -> 'acc -> (Core_type.t * 'acc)  =
      fun core_type acc ->
        let concrete = Core_type.to_concrete core_type in
        let { ptyp_attributes; ptyp_loc; ptyp_desc } : Core_type.concrete = concrete in
        let (ptyp_attributes, acc) = self#attributes ptyp_attributes acc in
        let (ptyp_loc, acc) = self#location ptyp_loc acc in
        let (ptyp_desc, acc) = self#core_type_desc ptyp_desc acc in
        (Core_type.of_concrete { ptyp_attributes; ptyp_loc; ptyp_desc }, acc)
    method payload : Payload.t -> 'acc -> (Payload.t * 'acc)  =
      fun payload acc ->
        let concrete = Payload.to_concrete payload in
        match (concrete : Payload.concrete) with
        | PPat (x0, x1) ->
          let (x0, acc) = self#option self#expression x0 acc in
          let (x1, acc) = self#pattern x1 acc in
          (Payload.of_concrete (PPat (x0, x1)), acc)
        | PTyp x0 ->
          let (x0, acc) = self#core_type x0 acc in
          (Payload.of_concrete (PTyp x0), acc)
        | PSig x0 ->
          let (x0, acc) = self#signature x0 acc in
          (Payload.of_concrete (PSig x0), acc)
        | PStr x0 ->
          let (x0, acc) = self#structure x0 acc in
          (Payload.of_concrete (PStr x0), acc)
    method attributes : Attributes.t -> 'acc -> (Attributes.t * 'acc)  =
      fun attributes acc ->
        let concrete = Attributes.to_concrete attributes in
        let (concrete, acc) = self#list self#attribute concrete acc in
        (Attributes.of_concrete concrete, acc)
    method extension : Extension.t -> 'acc -> (Extension.t * 'acc)  =
      fun extension acc ->
        let concrete = Extension.to_concrete extension in
        let (x0, x1) = concrete in
        let (x0, acc) = self#payload x0 acc in
        let (x1, acc) = self#loc self#string x1 acc in
        (Extension.of_concrete (x0, x1), acc)
    method attribute : Attribute.t -> 'acc -> (Attribute.t * 'acc)  =
      fun attribute acc ->
        let concrete = Attribute.to_concrete attribute in
        let (x0, x1) = concrete in
        let (x0, acc) = self#payload x0 acc in
        let (x1, acc) = self#loc self#string x1 acc in
        (Attribute.of_concrete (x0, x1), acc)
    method constant : Constant.t -> 'acc -> (Constant.t * 'acc)  =
      fun constant acc ->
        let concrete = Constant.to_concrete constant in
        match (concrete : Constant.concrete) with
        | Pconst_float (x0, x1) ->
          let (x0, acc) = self#option self#char x0 acc in
          let (x1, acc) = self#string x1 acc in
          (Constant.of_concrete (Pconst_float (x0, x1)), acc)
        | Pconst_string (x0, x1) ->
          let (x0, acc) = self#option self#string x0 acc in
          let (x1, acc) = self#string x1 acc in
          (Constant.of_concrete (Pconst_string (x0, x1)), acc)
        | Pconst_char x0 ->
          let (x0, acc) = self#char x0 acc in
          (Constant.of_concrete (Pconst_char x0), acc)
        | Pconst_integer (x0, x1) ->
          let (x0, acc) = self#option self#char x0 acc in
          let (x1, acc) = self#string x1 acc in
          (Constant.of_concrete (Pconst_integer (x0, x1)), acc)
    method variance : Variance.t -> 'acc -> (Variance.t * 'acc)  =
      fun variance acc ->
        let concrete = Variance.to_concrete variance in
        match (concrete : Variance.concrete) with
        | Invariant ->
          (Variance.of_concrete Invariant, acc)
        | Contravariant ->
          (Variance.of_concrete Contravariant, acc)
        | Covariant ->
          (Variance.of_concrete Covariant, acc)
    method arg_label : Arg_label.t -> 'acc -> (Arg_label.t * 'acc)  =
      fun arg_label acc ->
        let concrete = Arg_label.to_concrete arg_label in
        match (concrete : Arg_label.concrete) with
        | Optional x0 ->
          let (x0, acc) = self#string x0 acc in
          (Arg_label.of_concrete (Optional x0), acc)
        | Labelled x0 ->
          let (x0, acc) = self#string x0 acc in
          (Arg_label.of_concrete (Labelled x0), acc)
        | Nolabel ->
          (Arg_label.of_concrete Nolabel, acc)
    method closed_flag : Closed_flag.t -> 'acc -> (Closed_flag.t * 'acc)  =
      fun closed_flag acc ->
        let concrete = Closed_flag.to_concrete closed_flag in
        match (concrete : Closed_flag.concrete) with
        | Open ->
          (Closed_flag.of_concrete Open, acc)
        | Closed ->
          (Closed_flag.of_concrete Closed, acc)
    method override_flag : Override_flag.t -> 'acc -> (Override_flag.t * 'acc)  =
      fun override_flag acc ->
        let concrete = Override_flag.to_concrete override_flag in
        match (concrete : Override_flag.concrete) with
        | Fresh ->
          (Override_flag.of_concrete Fresh, acc)
        | Override ->
          (Override_flag.of_concrete Override, acc)
    method virtual_flag : Virtual_flag.t -> 'acc -> (Virtual_flag.t * 'acc)  =
      fun virtual_flag acc ->
        let concrete = Virtual_flag.to_concrete virtual_flag in
        match (concrete : Virtual_flag.concrete) with
        | Concrete ->
          (Virtual_flag.of_concrete Concrete, acc)
        | Virtual ->
          (Virtual_flag.of_concrete Virtual, acc)
    method mutable_flag : Mutable_flag.t -> 'acc -> (Mutable_flag.t * 'acc)  =
      fun mutable_flag acc ->
        let concrete = Mutable_flag.to_concrete mutable_flag in
        match (concrete : Mutable_flag.concrete) with
        | Mutable ->
          (Mutable_flag.of_concrete Mutable, acc)
        | Immutable ->
          (Mutable_flag.of_concrete Immutable, acc)
    method private_flag : Private_flag.t -> 'acc -> (Private_flag.t * 'acc)  =
      fun private_flag acc ->
        let concrete = Private_flag.to_concrete private_flag in
        match (concrete : Private_flag.concrete) with
        | Public ->
          (Private_flag.of_concrete Public, acc)
        | Private ->
          (Private_flag.of_concrete Private, acc)
    method direction_flag : Direction_flag.t -> 'acc -> (Direction_flag.t * 'acc)  =
      fun direction_flag acc ->
        let concrete = Direction_flag.to_concrete direction_flag in
        match (concrete : Direction_flag.concrete) with
        | Downto ->
          (Direction_flag.of_concrete Downto, acc)
        | Upto ->
          (Direction_flag.of_concrete Upto, acc)
    method rec_flag : Rec_flag.t -> 'acc -> (Rec_flag.t * 'acc)  =
      fun rec_flag acc ->
        let concrete = Rec_flag.to_concrete rec_flag in
        match (concrete : Rec_flag.concrete) with
        | Recursive ->
          (Rec_flag.of_concrete Recursive, acc)
        | Nonrecursive ->
          (Rec_flag.of_concrete Nonrecursive, acc)
    method longident_loc : Longident_loc.t -> 'acc -> (Longident_loc.t * 'acc)  =
      fun longident_loc acc ->
        let concrete = Longident_loc.to_concrete longident_loc in
        let (concrete, acc) = self#loc self#longident concrete acc in
        (Longident_loc.of_concrete concrete, acc)
    method longident : Longident.t -> 'acc -> (Longident.t * 'acc)  =
      fun longident acc ->
        let concrete = Longident.to_concrete longident in
        match (concrete : Longident.concrete) with
        | Lapply (x0, x1) ->
          let (x0, acc) = self#longident x0 acc in
          let (x1, acc) = self#longident x1 acc in
          (Longident.of_concrete (Lapply (x0, x1)), acc)
        | Ldot (x0, x1) ->
          let (x0, acc) = self#string x0 acc in
          let (x1, acc) = self#longident x1 acc in
          (Longident.of_concrete (Ldot (x0, x1)), acc)
        | Lident x0 ->
          let (x0, acc) = self#string x0 acc in
          (Longident.of_concrete (Lident x0), acc)
  end

class virtual ['ctx] map_with_context =
  object (self)
    method virtual bool : 'ctx -> bool -> bool
    method virtual char : 'ctx -> char -> char
    method virtual int : 'ctx -> int -> int
    method virtual list : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list
    method virtual option : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
    method virtual string : 'ctx -> string -> string
    method virtual location : 'ctx -> Astlib.Location.t -> Astlib.Location.t
    method virtual loc : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a Astlib.Loc.t -> 'a Astlib.Loc.t
    method directive_argument : 'ctx -> Directive_argument.t -> Directive_argument.t  =
      fun _ctx directive_argument ->
        let concrete = Directive_argument.to_concrete directive_argument in
        match (concrete : Directive_argument.concrete) with
        | Pdir_bool x0 ->
          let x0 = self#bool _ctx x0 in
          Directive_argument.of_concrete (Pdir_bool x0)
        | Pdir_ident x0 ->
          let x0 = self#longident _ctx x0 in
          Directive_argument.of_concrete (Pdir_ident x0)
        | Pdir_int (x0, x1) ->
          let x0 = self#option self#char _ctx x0 in
          let x1 = self#string _ctx x1 in
          Directive_argument.of_concrete (Pdir_int (x0, x1))
        | Pdir_string x0 ->
          let x0 = self#string _ctx x0 in
          Directive_argument.of_concrete (Pdir_string x0)
        | Pdir_none ->
          Directive_argument.of_concrete Pdir_none
    method toplevel_phrase : 'ctx -> Toplevel_phrase.t -> Toplevel_phrase.t  =
      fun _ctx toplevel_phrase ->
        let concrete = Toplevel_phrase.to_concrete toplevel_phrase in
        match (concrete : Toplevel_phrase.concrete) with
        | Ptop_dir (x0, x1) ->
          let x0 = self#directive_argument _ctx x0 in
          let x1 = self#string _ctx x1 in
          Toplevel_phrase.of_concrete (Ptop_dir (x0, x1))
        | Ptop_def x0 ->
          let x0 = self#structure _ctx x0 in
          Toplevel_phrase.of_concrete (Ptop_def x0)
    method module_binding : 'ctx -> Module_binding.t -> Module_binding.t  =
      fun _ctx module_binding ->
        let concrete = Module_binding.to_concrete module_binding in
        let { pmb_loc; pmb_attributes; pmb_expr; pmb_name } : Module_binding.concrete = concrete in
        let pmb_loc = self#location _ctx pmb_loc in
        let pmb_attributes = self#attributes _ctx pmb_attributes in
        let pmb_expr = self#module_expr _ctx pmb_expr in
        let pmb_name = self#loc self#string _ctx pmb_name in
        Module_binding.of_concrete { pmb_loc; pmb_attributes; pmb_expr; pmb_name }
    method value_binding : 'ctx -> Value_binding.t -> Value_binding.t  =
      fun _ctx value_binding ->
        let concrete = Value_binding.to_concrete value_binding in
        let { pvb_loc; pvb_attributes; pvb_expr; pvb_pat } : Value_binding.concrete = concrete in
        let pvb_loc = self#location _ctx pvb_loc in
        let pvb_attributes = self#attributes _ctx pvb_attributes in
        let pvb_expr = self#expression _ctx pvb_expr in
        let pvb_pat = self#pattern _ctx pvb_pat in
        Value_binding.of_concrete { pvb_loc; pvb_attributes; pvb_expr; pvb_pat }
    method structure_item_desc : 'ctx -> Structure_item_desc.t -> Structure_item_desc.t  =
      fun _ctx structure_item_desc ->
        let concrete = Structure_item_desc.to_concrete structure_item_desc in
        match (concrete : Structure_item_desc.concrete) with
        | Pstr_extension (x0, x1) ->
          let x0 = self#attributes _ctx x0 in
          let x1 = self#extension _ctx x1 in
          Structure_item_desc.of_concrete (Pstr_extension (x0, x1))
        | Pstr_attribute x0 ->
          let x0 = self#attribute _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_attribute x0)
        | Pstr_include x0 ->
          let x0 = self#include_declaration _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_include x0)
        | Pstr_class_type x0 ->
          let x0 = self#list self#class_type_declaration _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_class_type x0)
        | Pstr_class x0 ->
          let x0 = self#list self#class_declaration _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_class x0)
        | Pstr_open x0 ->
          let x0 = self#open_description _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_open x0)
        | Pstr_modtype x0 ->
          let x0 = self#module_type_declaration _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_modtype x0)
        | Pstr_recmodule x0 ->
          let x0 = self#list self#module_binding _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_recmodule x0)
        | Pstr_module x0 ->
          let x0 = self#module_binding _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_module x0)
        | Pstr_exception x0 ->
          let x0 = self#extension_constructor _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_exception x0)
        | Pstr_typext x0 ->
          let x0 = self#type_extension _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_typext x0)
        | Pstr_type (x0, x1) ->
          let x0 = self#list self#type_declaration _ctx x0 in
          let x1 = self#rec_flag _ctx x1 in
          Structure_item_desc.of_concrete (Pstr_type (x0, x1))
        | Pstr_primitive x0 ->
          let x0 = self#value_description _ctx x0 in
          Structure_item_desc.of_concrete (Pstr_primitive x0)
        | Pstr_value (x0, x1) ->
          let x0 = self#list self#value_binding _ctx x0 in
          let x1 = self#rec_flag _ctx x1 in
          Structure_item_desc.of_concrete (Pstr_value (x0, x1))
        | Pstr_eval (x0, x1) ->
          let x0 = self#attributes _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Structure_item_desc.of_concrete (Pstr_eval (x0, x1))
    method structure_item : 'ctx -> Structure_item.t -> Structure_item.t  =
      fun _ctx structure_item ->
        let concrete = Structure_item.to_concrete structure_item in
        let { pstr_loc; pstr_desc } : Structure_item.concrete = concrete in
        let pstr_loc = self#location _ctx pstr_loc in
        let pstr_desc = self#structure_item_desc _ctx pstr_desc in
        Structure_item.of_concrete { pstr_loc; pstr_desc }
    method structure : 'ctx -> Structure.t -> Structure.t  =
      fun _ctx structure ->
        let concrete = Structure.to_concrete structure in
        let concrete = self#list self#structure_item _ctx concrete in
        Structure.of_concrete concrete
    method module_expr_desc : 'ctx -> Module_expr_desc.t -> Module_expr_desc.t  =
      fun _ctx module_expr_desc ->
        let concrete = Module_expr_desc.to_concrete module_expr_desc in
        match (concrete : Module_expr_desc.concrete) with
        | Pmod_extension x0 ->
          let x0 = self#extension _ctx x0 in
          Module_expr_desc.of_concrete (Pmod_extension x0)
        | Pmod_unpack x0 ->
          let x0 = self#expression _ctx x0 in
          Module_expr_desc.of_concrete (Pmod_unpack x0)
        | Pmod_constraint (x0, x1) ->
          let x0 = self#module_type _ctx x0 in
          let x1 = self#module_expr _ctx x1 in
          Module_expr_desc.of_concrete (Pmod_constraint (x0, x1))
        | Pmod_apply (x0, x1) ->
          let x0 = self#module_expr _ctx x0 in
          let x1 = self#module_expr _ctx x1 in
          Module_expr_desc.of_concrete (Pmod_apply (x0, x1))
        | Pmod_functor (x0, x1, x2) ->
          let x0 = self#module_expr _ctx x0 in
          let x1 = self#option self#module_type _ctx x1 in
          let x2 = self#loc self#string _ctx x2 in
          Module_expr_desc.of_concrete (Pmod_functor (x0, x1, x2))
        | Pmod_structure x0 ->
          let x0 = self#structure _ctx x0 in
          Module_expr_desc.of_concrete (Pmod_structure x0)
        | Pmod_ident x0 ->
          let x0 = self#longident_loc _ctx x0 in
          Module_expr_desc.of_concrete (Pmod_ident x0)
    method module_expr : 'ctx -> Module_expr.t -> Module_expr.t  =
      fun _ctx module_expr ->
        let concrete = Module_expr.to_concrete module_expr in
        let { pmod_attributes; pmod_loc; pmod_desc } : Module_expr.concrete = concrete in
        let pmod_attributes = self#attributes _ctx pmod_attributes in
        let pmod_loc = self#location _ctx pmod_loc in
        let pmod_desc = self#module_expr_desc _ctx pmod_desc in
        Module_expr.of_concrete { pmod_attributes; pmod_loc; pmod_desc }
    method with_constraint : 'ctx -> With_constraint.t -> With_constraint.t  =
      fun _ctx with_constraint ->
        let concrete = With_constraint.to_concrete with_constraint in
        match (concrete : With_constraint.concrete) with
        | Pwith_modsubst (x0, x1) ->
          let x0 = self#longident_loc _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          With_constraint.of_concrete (Pwith_modsubst (x0, x1))
        | Pwith_typesubst (x0, x1) ->
          let x0 = self#type_declaration _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          With_constraint.of_concrete (Pwith_typesubst (x0, x1))
        | Pwith_module (x0, x1) ->
          let x0 = self#longident_loc _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          With_constraint.of_concrete (Pwith_module (x0, x1))
        | Pwith_type (x0, x1) ->
          let x0 = self#type_declaration _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          With_constraint.of_concrete (Pwith_type (x0, x1))
    method include_declaration : 'ctx -> Include_declaration.t -> Include_declaration.t  =
      fun _ctx include_declaration ->
        let concrete = Include_declaration.to_concrete include_declaration in
        let concrete = self#include_infos_module_expr _ctx concrete in
        Include_declaration.of_concrete concrete
    method include_description : 'ctx -> Include_description.t -> Include_description.t  =
      fun _ctx include_description ->
        let concrete = Include_description.to_concrete include_description in
        let concrete = self#include_infos_module_type _ctx concrete in
        Include_description.of_concrete concrete
    method include_infos_module_expr : 'ctx -> Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t  =
      fun _ctx include_infos ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_expr.t Include_infos.concrete = concrete in
        let pincl_attributes = self#attributes _ctx pincl_attributes in
        let pincl_loc = self#location _ctx pincl_loc in
        let pincl_mod = self#module_expr _ctx pincl_mod in
        Include_infos.of_concrete { pincl_attributes; pincl_loc; pincl_mod }
    method include_infos_module_type : 'ctx -> Module_type.t Include_infos.t -> Module_type.t Include_infos.t  =
      fun _ctx include_infos ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_type.t Include_infos.concrete = concrete in
        let pincl_attributes = self#attributes _ctx pincl_attributes in
        let pincl_loc = self#location _ctx pincl_loc in
        let pincl_mod = self#module_type _ctx pincl_mod in
        Include_infos.of_concrete { pincl_attributes; pincl_loc; pincl_mod }
    method open_description : 'ctx -> Open_description.t -> Open_description.t  =
      fun _ctx open_description ->
        let concrete = Open_description.to_concrete open_description in
        let { popen_attributes; popen_loc; popen_override; popen_lid } : Open_description.concrete = concrete in
        let popen_attributes = self#attributes _ctx popen_attributes in
        let popen_loc = self#location _ctx popen_loc in
        let popen_override = self#override_flag _ctx popen_override in
        let popen_lid = self#longident_loc _ctx popen_lid in
        Open_description.of_concrete { popen_attributes; popen_loc; popen_override; popen_lid }
    method module_type_declaration : 'ctx -> Module_type_declaration.t -> Module_type_declaration.t  =
      fun _ctx module_type_declaration ->
        let concrete = Module_type_declaration.to_concrete module_type_declaration in
        let { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name } : Module_type_declaration.concrete = concrete in
        let pmtd_loc = self#location _ctx pmtd_loc in
        let pmtd_attributes = self#attributes _ctx pmtd_attributes in
        let pmtd_type = self#option self#module_type _ctx pmtd_type in
        let pmtd_name = self#loc self#string _ctx pmtd_name in
        Module_type_declaration.of_concrete { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name }
    method module_declaration : 'ctx -> Module_declaration.t -> Module_declaration.t  =
      fun _ctx module_declaration ->
        let concrete = Module_declaration.to_concrete module_declaration in
        let { pmd_loc; pmd_attributes; pmd_type; pmd_name } : Module_declaration.concrete = concrete in
        let pmd_loc = self#location _ctx pmd_loc in
        let pmd_attributes = self#attributes _ctx pmd_attributes in
        let pmd_type = self#module_type _ctx pmd_type in
        let pmd_name = self#loc self#string _ctx pmd_name in
        Module_declaration.of_concrete { pmd_loc; pmd_attributes; pmd_type; pmd_name }
    method signature_item_desc : 'ctx -> Signature_item_desc.t -> Signature_item_desc.t  =
      fun _ctx signature_item_desc ->
        let concrete = Signature_item_desc.to_concrete signature_item_desc in
        match (concrete : Signature_item_desc.concrete) with
        | Psig_extension (x0, x1) ->
          let x0 = self#attributes _ctx x0 in
          let x1 = self#extension _ctx x1 in
          Signature_item_desc.of_concrete (Psig_extension (x0, x1))
        | Psig_attribute x0 ->
          let x0 = self#attribute _ctx x0 in
          Signature_item_desc.of_concrete (Psig_attribute x0)
        | Psig_class_type x0 ->
          let x0 = self#list self#class_type_declaration _ctx x0 in
          Signature_item_desc.of_concrete (Psig_class_type x0)
        | Psig_class x0 ->
          let x0 = self#list self#class_description _ctx x0 in
          Signature_item_desc.of_concrete (Psig_class x0)
        | Psig_include x0 ->
          let x0 = self#include_description _ctx x0 in
          Signature_item_desc.of_concrete (Psig_include x0)
        | Psig_open x0 ->
          let x0 = self#open_description _ctx x0 in
          Signature_item_desc.of_concrete (Psig_open x0)
        | Psig_modtype x0 ->
          let x0 = self#module_type_declaration _ctx x0 in
          Signature_item_desc.of_concrete (Psig_modtype x0)
        | Psig_recmodule x0 ->
          let x0 = self#list self#module_declaration _ctx x0 in
          Signature_item_desc.of_concrete (Psig_recmodule x0)
        | Psig_module x0 ->
          let x0 = self#module_declaration _ctx x0 in
          Signature_item_desc.of_concrete (Psig_module x0)
        | Psig_exception x0 ->
          let x0 = self#extension_constructor _ctx x0 in
          Signature_item_desc.of_concrete (Psig_exception x0)
        | Psig_typext x0 ->
          let x0 = self#type_extension _ctx x0 in
          Signature_item_desc.of_concrete (Psig_typext x0)
        | Psig_type (x0, x1) ->
          let x0 = self#list self#type_declaration _ctx x0 in
          let x1 = self#rec_flag _ctx x1 in
          Signature_item_desc.of_concrete (Psig_type (x0, x1))
        | Psig_value x0 ->
          let x0 = self#value_description _ctx x0 in
          Signature_item_desc.of_concrete (Psig_value x0)
    method signature_item : 'ctx -> Signature_item.t -> Signature_item.t  =
      fun _ctx signature_item ->
        let concrete = Signature_item.to_concrete signature_item in
        let { psig_loc; psig_desc } : Signature_item.concrete = concrete in
        let psig_loc = self#location _ctx psig_loc in
        let psig_desc = self#signature_item_desc _ctx psig_desc in
        Signature_item.of_concrete { psig_loc; psig_desc }
    method signature : 'ctx -> Signature.t -> Signature.t  =
      fun _ctx signature ->
        let concrete = Signature.to_concrete signature in
        let concrete = self#list self#signature_item _ctx concrete in
        Signature.of_concrete concrete
    method module_type_desc : 'ctx -> Module_type_desc.t -> Module_type_desc.t  =
      fun _ctx module_type_desc ->
        let concrete = Module_type_desc.to_concrete module_type_desc in
        match (concrete : Module_type_desc.concrete) with
        | Pmty_alias x0 ->
          let x0 = self#longident_loc _ctx x0 in
          Module_type_desc.of_concrete (Pmty_alias x0)
        | Pmty_extension x0 ->
          let x0 = self#extension _ctx x0 in
          Module_type_desc.of_concrete (Pmty_extension x0)
        | Pmty_typeof x0 ->
          let x0 = self#module_expr _ctx x0 in
          Module_type_desc.of_concrete (Pmty_typeof x0)
        | Pmty_with (x0, x1) ->
          let x0 = self#list self#with_constraint _ctx x0 in
          let x1 = self#module_type _ctx x1 in
          Module_type_desc.of_concrete (Pmty_with (x0, x1))
        | Pmty_functor (x0, x1, x2) ->
          let x0 = self#module_type _ctx x0 in
          let x1 = self#option self#module_type _ctx x1 in
          let x2 = self#loc self#string _ctx x2 in
          Module_type_desc.of_concrete (Pmty_functor (x0, x1, x2))
        | Pmty_signature x0 ->
          let x0 = self#signature _ctx x0 in
          Module_type_desc.of_concrete (Pmty_signature x0)
        | Pmty_ident x0 ->
          let x0 = self#longident_loc _ctx x0 in
          Module_type_desc.of_concrete (Pmty_ident x0)
    method module_type : 'ctx -> Module_type.t -> Module_type.t  =
      fun _ctx module_type ->
        let concrete = Module_type.to_concrete module_type in
        let { pmty_attributes; pmty_loc; pmty_desc } : Module_type.concrete = concrete in
        let pmty_attributes = self#attributes _ctx pmty_attributes in
        let pmty_loc = self#location _ctx pmty_loc in
        let pmty_desc = self#module_type_desc _ctx pmty_desc in
        Module_type.of_concrete { pmty_attributes; pmty_loc; pmty_desc }
    method class_declaration : 'ctx -> Class_declaration.t -> Class_declaration.t  =
      fun _ctx class_declaration ->
        let concrete = Class_declaration.to_concrete class_declaration in
        let concrete = self#class_infos_class_expr _ctx concrete in
        Class_declaration.of_concrete concrete
    method class_field_kind : 'ctx -> Class_field_kind.t -> Class_field_kind.t  =
      fun _ctx class_field_kind ->
        let concrete = Class_field_kind.to_concrete class_field_kind in
        match (concrete : Class_field_kind.concrete) with
        | Cfk_concrete (x0, x1) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#override_flag _ctx x1 in
          Class_field_kind.of_concrete (Cfk_concrete (x0, x1))
        | Cfk_virtual x0 ->
          let x0 = self#core_type _ctx x0 in
          Class_field_kind.of_concrete (Cfk_virtual x0)
    method class_field_desc : 'ctx -> Class_field_desc.t -> Class_field_desc.t  =
      fun _ctx class_field_desc ->
        let concrete = Class_field_desc.to_concrete class_field_desc in
        match (concrete : Class_field_desc.concrete) with
        | Pcf_extension x0 ->
          let x0 = self#extension _ctx x0 in
          Class_field_desc.of_concrete (Pcf_extension x0)
        | Pcf_attribute x0 ->
          let x0 = self#attribute _ctx x0 in
          Class_field_desc.of_concrete (Pcf_attribute x0)
        | Pcf_initializer x0 ->
          let x0 = self#expression _ctx x0 in
          Class_field_desc.of_concrete (Pcf_initializer x0)
        | Pcf_constraint x0 ->
          let x0 = (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx x0 in
          Class_field_desc.of_concrete (Pcf_constraint x0)
        | Pcf_method x0 ->
          let x0 = (fun _ctx (x0, x1, x2) -> let x0 = self#class_field_kind _ctx x0 in let x1 = self#private_flag _ctx x1 in let x2 = self#loc self#string _ctx x2 in (x0, x1, x2)) _ctx x0 in
          Class_field_desc.of_concrete (Pcf_method x0)
        | Pcf_val x0 ->
          let x0 = (fun _ctx (x0, x1, x2) -> let x0 = self#class_field_kind _ctx x0 in let x1 = self#mutable_flag _ctx x1 in let x2 = self#loc self#string _ctx x2 in (x0, x1, x2)) _ctx x0 in
          Class_field_desc.of_concrete (Pcf_val x0)
        | Pcf_inherit (x0, x1, x2) ->
          let x0 = self#option (self#loc self#string) _ctx x0 in
          let x1 = self#class_expr _ctx x1 in
          let x2 = self#override_flag _ctx x2 in
          Class_field_desc.of_concrete (Pcf_inherit (x0, x1, x2))
    method class_field : 'ctx -> Class_field.t -> Class_field.t  =
      fun _ctx class_field ->
        let concrete = Class_field.to_concrete class_field in
        let { pcf_attributes; pcf_loc; pcf_desc } : Class_field.concrete = concrete in
        let pcf_attributes = self#attributes _ctx pcf_attributes in
        let pcf_loc = self#location _ctx pcf_loc in
        let pcf_desc = self#class_field_desc _ctx pcf_desc in
        Class_field.of_concrete { pcf_attributes; pcf_loc; pcf_desc }
    method class_structure : 'ctx -> Class_structure.t -> Class_structure.t  =
      fun _ctx class_structure ->
        let concrete = Class_structure.to_concrete class_structure in
        let { pcstr_fields; pcstr_self } : Class_structure.concrete = concrete in
        let pcstr_fields = self#list self#class_field _ctx pcstr_fields in
        let pcstr_self = self#pattern _ctx pcstr_self in
        Class_structure.of_concrete { pcstr_fields; pcstr_self }
    method class_expr_desc : 'ctx -> Class_expr_desc.t -> Class_expr_desc.t  =
      fun _ctx class_expr_desc ->
        let concrete = Class_expr_desc.to_concrete class_expr_desc in
        match (concrete : Class_expr_desc.concrete) with
        | Pcl_open (x0, x1, x2) ->
          let x0 = self#class_expr _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          let x2 = self#override_flag _ctx x2 in
          Class_expr_desc.of_concrete (Pcl_open (x0, x1, x2))
        | Pcl_extension x0 ->
          let x0 = self#extension _ctx x0 in
          Class_expr_desc.of_concrete (Pcl_extension x0)
        | Pcl_constraint (x0, x1) ->
          let x0 = self#class_type _ctx x0 in
          let x1 = self#class_expr _ctx x1 in
          Class_expr_desc.of_concrete (Pcl_constraint (x0, x1))
        | Pcl_let (x0, x1, x2) ->
          let x0 = self#class_expr _ctx x0 in
          let x1 = self#list self#value_binding _ctx x1 in
          let x2 = self#rec_flag _ctx x2 in
          Class_expr_desc.of_concrete (Pcl_let (x0, x1, x2))
        | Pcl_apply (x0, x1) ->
          let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#expression _ctx x0 in let x1 = self#arg_label _ctx x1 in (x0, x1)) _ctx x0 in
          let x1 = self#class_expr _ctx x1 in
          Class_expr_desc.of_concrete (Pcl_apply (x0, x1))
        | Pcl_fun (x0, x1, x2, x3) ->
          let x0 = self#class_expr _ctx x0 in
          let x1 = self#pattern _ctx x1 in
          let x2 = self#option self#expression _ctx x2 in
          let x3 = self#arg_label _ctx x3 in
          Class_expr_desc.of_concrete (Pcl_fun (x0, x1, x2, x3))
        | Pcl_structure x0 ->
          let x0 = self#class_structure _ctx x0 in
          Class_expr_desc.of_concrete (Pcl_structure x0)
        | Pcl_constr (x0, x1) ->
          let x0 = self#list self#core_type _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          Class_expr_desc.of_concrete (Pcl_constr (x0, x1))
    method class_expr : 'ctx -> Class_expr.t -> Class_expr.t  =
      fun _ctx class_expr ->
        let concrete = Class_expr.to_concrete class_expr in
        let { pcl_attributes; pcl_loc; pcl_desc } : Class_expr.concrete = concrete in
        let pcl_attributes = self#attributes _ctx pcl_attributes in
        let pcl_loc = self#location _ctx pcl_loc in
        let pcl_desc = self#class_expr_desc _ctx pcl_desc in
        Class_expr.of_concrete { pcl_attributes; pcl_loc; pcl_desc }
    method class_type_declaration : 'ctx -> Class_type_declaration.t -> Class_type_declaration.t  =
      fun _ctx class_type_declaration ->
        let concrete = Class_type_declaration.to_concrete class_type_declaration in
        let concrete = self#class_infos_class_type _ctx concrete in
        Class_type_declaration.of_concrete concrete
    method class_description : 'ctx -> Class_description.t -> Class_description.t  =
      fun _ctx class_description ->
        let concrete = Class_description.to_concrete class_description in
        let concrete = self#class_infos_class_type _ctx concrete in
        Class_description.of_concrete concrete
    method class_infos_class_expr : 'ctx -> Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t  =
      fun _ctx class_infos ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_expr.t Class_infos.concrete = concrete in
        let pci_attributes = self#attributes _ctx pci_attributes in
        let pci_loc = self#location _ctx pci_loc in
        let pci_expr = self#class_expr _ctx pci_expr in
        let pci_name = self#loc self#string _ctx pci_name in
        let pci_params = self#list (fun _ctx (x0, x1) -> let x0 = self#variance _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx pci_params in
        let pci_virt = self#virtual_flag _ctx pci_virt in
        Class_infos.of_concrete { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt }
    method class_infos_class_type : 'ctx -> Class_type.t Class_infos.t -> Class_type.t Class_infos.t  =
      fun _ctx class_infos ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_type.t Class_infos.concrete = concrete in
        let pci_attributes = self#attributes _ctx pci_attributes in
        let pci_loc = self#location _ctx pci_loc in
        let pci_expr = self#class_type _ctx pci_expr in
        let pci_name = self#loc self#string _ctx pci_name in
        let pci_params = self#list (fun _ctx (x0, x1) -> let x0 = self#variance _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx pci_params in
        let pci_virt = self#virtual_flag _ctx pci_virt in
        Class_infos.of_concrete { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt }
    method class_type_field_desc : 'ctx -> Class_type_field_desc.t -> Class_type_field_desc.t  =
      fun _ctx class_type_field_desc ->
        let concrete = Class_type_field_desc.to_concrete class_type_field_desc in
        match (concrete : Class_type_field_desc.concrete) with
        | Pctf_extension x0 ->
          let x0 = self#extension _ctx x0 in
          Class_type_field_desc.of_concrete (Pctf_extension x0)
        | Pctf_attribute x0 ->
          let x0 = self#attribute _ctx x0 in
          Class_type_field_desc.of_concrete (Pctf_attribute x0)
        | Pctf_constraint x0 ->
          let x0 = (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx x0 in
          Class_type_field_desc.of_concrete (Pctf_constraint x0)
        | Pctf_method x0 ->
          let x0 = (fun _ctx (x0, x1, x2, x3) -> let x0 = self#core_type _ctx x0 in let x1 = self#virtual_flag _ctx x1 in let x2 = self#private_flag _ctx x2 in let x3 = self#loc self#string _ctx x3 in (x0, x1, x2, x3)) _ctx x0 in
          Class_type_field_desc.of_concrete (Pctf_method x0)
        | Pctf_val x0 ->
          let x0 = (fun _ctx (x0, x1, x2, x3) -> let x0 = self#core_type _ctx x0 in let x1 = self#virtual_flag _ctx x1 in let x2 = self#mutable_flag _ctx x2 in let x3 = self#loc self#string _ctx x3 in (x0, x1, x2, x3)) _ctx x0 in
          Class_type_field_desc.of_concrete (Pctf_val x0)
        | Pctf_inherit x0 ->
          let x0 = self#class_type _ctx x0 in
          Class_type_field_desc.of_concrete (Pctf_inherit x0)
    method class_type_field : 'ctx -> Class_type_field.t -> Class_type_field.t  =
      fun _ctx class_type_field ->
        let concrete = Class_type_field.to_concrete class_type_field in
        let { pctf_attributes; pctf_loc; pctf_desc } : Class_type_field.concrete = concrete in
        let pctf_attributes = self#attributes _ctx pctf_attributes in
        let pctf_loc = self#location _ctx pctf_loc in
        let pctf_desc = self#class_type_field_desc _ctx pctf_desc in
        Class_type_field.of_concrete { pctf_attributes; pctf_loc; pctf_desc }
    method class_signature : 'ctx -> Class_signature.t -> Class_signature.t  =
      fun _ctx class_signature ->
        let concrete = Class_signature.to_concrete class_signature in
        let { pcsig_fields; pcsig_self } : Class_signature.concrete = concrete in
        let pcsig_fields = self#list self#class_type_field _ctx pcsig_fields in
        let pcsig_self = self#core_type _ctx pcsig_self in
        Class_signature.of_concrete { pcsig_fields; pcsig_self }
    method class_type_desc : 'ctx -> Class_type_desc.t -> Class_type_desc.t  =
      fun _ctx class_type_desc ->
        let concrete = Class_type_desc.to_concrete class_type_desc in
        match (concrete : Class_type_desc.concrete) with
        | Pcty_open (x0, x1, x2) ->
          let x0 = self#class_type _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          let x2 = self#override_flag _ctx x2 in
          Class_type_desc.of_concrete (Pcty_open (x0, x1, x2))
        | Pcty_extension x0 ->
          let x0 = self#extension _ctx x0 in
          Class_type_desc.of_concrete (Pcty_extension x0)
        | Pcty_arrow (x0, x1, x2) ->
          let x0 = self#class_type _ctx x0 in
          let x1 = self#core_type _ctx x1 in
          let x2 = self#arg_label _ctx x2 in
          Class_type_desc.of_concrete (Pcty_arrow (x0, x1, x2))
        | Pcty_signature x0 ->
          let x0 = self#class_signature _ctx x0 in
          Class_type_desc.of_concrete (Pcty_signature x0)
        | Pcty_constr (x0, x1) ->
          let x0 = self#list self#core_type _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          Class_type_desc.of_concrete (Pcty_constr (x0, x1))
    method class_type : 'ctx -> Class_type.t -> Class_type.t  =
      fun _ctx class_type ->
        let concrete = Class_type.to_concrete class_type in
        let { pcty_attributes; pcty_loc; pcty_desc } : Class_type.concrete = concrete in
        let pcty_attributes = self#attributes _ctx pcty_attributes in
        let pcty_loc = self#location _ctx pcty_loc in
        let pcty_desc = self#class_type_desc _ctx pcty_desc in
        Class_type.of_concrete { pcty_attributes; pcty_loc; pcty_desc }
    method extension_constructor_kind : 'ctx -> Extension_constructor_kind.t -> Extension_constructor_kind.t  =
      fun _ctx extension_constructor_kind ->
        let concrete = Extension_constructor_kind.to_concrete extension_constructor_kind in
        match (concrete : Extension_constructor_kind.concrete) with
        | Pext_rebind x0 ->
          let x0 = self#longident_loc _ctx x0 in
          Extension_constructor_kind.of_concrete (Pext_rebind x0)
        | Pext_decl (x0, x1) ->
          let x0 = self#option self#core_type _ctx x0 in
          let x1 = self#constructor_arguments _ctx x1 in
          Extension_constructor_kind.of_concrete (Pext_decl (x0, x1))
    method extension_constructor : 'ctx -> Extension_constructor.t -> Extension_constructor.t  =
      fun _ctx extension_constructor ->
        let concrete = Extension_constructor.to_concrete extension_constructor in
        let { pext_attributes; pext_loc; pext_kind; pext_name } : Extension_constructor.concrete = concrete in
        let pext_attributes = self#attributes _ctx pext_attributes in
        let pext_loc = self#location _ctx pext_loc in
        let pext_kind = self#extension_constructor_kind _ctx pext_kind in
        let pext_name = self#loc self#string _ctx pext_name in
        Extension_constructor.of_concrete { pext_attributes; pext_loc; pext_kind; pext_name }
    method type_extension : 'ctx -> Type_extension.t -> Type_extension.t  =
      fun _ctx type_extension ->
        let concrete = Type_extension.to_concrete type_extension in
        let { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path } : Type_extension.concrete = concrete in
        let ptyext_attributes = self#attributes _ctx ptyext_attributes in
        let ptyext_private = self#private_flag _ctx ptyext_private in
        let ptyext_constructors = self#list self#extension_constructor _ctx ptyext_constructors in
        let ptyext_params = self#list (fun _ctx (x0, x1) -> let x0 = self#variance _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx ptyext_params in
        let ptyext_path = self#longident_loc _ctx ptyext_path in
        Type_extension.of_concrete { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path }
    method constructor_arguments : 'ctx -> Constructor_arguments.t -> Constructor_arguments.t  =
      fun _ctx constructor_arguments ->
        let concrete = Constructor_arguments.to_concrete constructor_arguments in
        match (concrete : Constructor_arguments.concrete) with
        | Pcstr_record x0 ->
          let x0 = self#list self#label_declaration _ctx x0 in
          Constructor_arguments.of_concrete (Pcstr_record x0)
        | Pcstr_tuple x0 ->
          let x0 = self#list self#core_type _ctx x0 in
          Constructor_arguments.of_concrete (Pcstr_tuple x0)
    method constructor_declaration : 'ctx -> Constructor_declaration.t -> Constructor_declaration.t  =
      fun _ctx constructor_declaration ->
        let concrete = Constructor_declaration.to_concrete constructor_declaration in
        let { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name } : Constructor_declaration.concrete = concrete in
        let pcd_attributes = self#attributes _ctx pcd_attributes in
        let pcd_loc = self#location _ctx pcd_loc in
        let pcd_res = self#option self#core_type _ctx pcd_res in
        let pcd_args = self#constructor_arguments _ctx pcd_args in
        let pcd_name = self#loc self#string _ctx pcd_name in
        Constructor_declaration.of_concrete { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name }
    method label_declaration : 'ctx -> Label_declaration.t -> Label_declaration.t  =
      fun _ctx label_declaration ->
        let concrete = Label_declaration.to_concrete label_declaration in
        let { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name } : Label_declaration.concrete = concrete in
        let pld_attributes = self#attributes _ctx pld_attributes in
        let pld_loc = self#location _ctx pld_loc in
        let pld_type = self#core_type _ctx pld_type in
        let pld_mutable = self#mutable_flag _ctx pld_mutable in
        let pld_name = self#loc self#string _ctx pld_name in
        Label_declaration.of_concrete { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name }
    method type_kind : 'ctx -> Type_kind.t -> Type_kind.t  =
      fun _ctx type_kind ->
        let concrete = Type_kind.to_concrete type_kind in
        match (concrete : Type_kind.concrete) with
        | Ptype_open ->
          Type_kind.of_concrete Ptype_open
        | Ptype_record x0 ->
          let x0 = self#list self#label_declaration _ctx x0 in
          Type_kind.of_concrete (Ptype_record x0)
        | Ptype_variant x0 ->
          let x0 = self#list self#constructor_declaration _ctx x0 in
          Type_kind.of_concrete (Ptype_variant x0)
        | Ptype_abstract ->
          Type_kind.of_concrete Ptype_abstract
    method type_declaration : 'ctx -> Type_declaration.t -> Type_declaration.t  =
      fun _ctx type_declaration ->
        let concrete = Type_declaration.to_concrete type_declaration in
        let { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name } : Type_declaration.concrete = concrete in
        let ptype_loc = self#location _ctx ptype_loc in
        let ptype_attributes = self#attributes _ctx ptype_attributes in
        let ptype_manifest = self#option self#core_type _ctx ptype_manifest in
        let ptype_private = self#private_flag _ctx ptype_private in
        let ptype_kind = self#type_kind _ctx ptype_kind in
        let ptype_cstrs = self#list (fun _ctx (x0, x1, x2) -> let x0 = self#location _ctx x0 in let x1 = self#core_type _ctx x1 in let x2 = self#core_type _ctx x2 in (x0, x1, x2)) _ctx ptype_cstrs in
        let ptype_params = self#list (fun _ctx (x0, x1) -> let x0 = self#variance _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx ptype_params in
        let ptype_name = self#loc self#string _ctx ptype_name in
        Type_declaration.of_concrete { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name }
    method value_description : 'ctx -> Value_description.t -> Value_description.t  =
      fun _ctx value_description ->
        let concrete = Value_description.to_concrete value_description in
        let { pval_loc; pval_attributes; pval_prim; pval_type; pval_name } : Value_description.concrete = concrete in
        let pval_loc = self#location _ctx pval_loc in
        let pval_attributes = self#attributes _ctx pval_attributes in
        let pval_prim = self#list self#string _ctx pval_prim in
        let pval_type = self#core_type _ctx pval_type in
        let pval_name = self#loc self#string _ctx pval_name in
        Value_description.of_concrete { pval_loc; pval_attributes; pval_prim; pval_type; pval_name }
    method case : 'ctx -> Case.t -> Case.t  =
      fun _ctx case ->
        let concrete = Case.to_concrete case in
        let { pc_rhs; pc_guard; pc_lhs } : Case.concrete = concrete in
        let pc_rhs = self#expression _ctx pc_rhs in
        let pc_guard = self#option self#expression _ctx pc_guard in
        let pc_lhs = self#pattern _ctx pc_lhs in
        Case.of_concrete { pc_rhs; pc_guard; pc_lhs }
    method expression_desc : 'ctx -> Expression_desc.t -> Expression_desc.t  =
      fun _ctx expression_desc ->
        let concrete = Expression_desc.to_concrete expression_desc in
        match (concrete : Expression_desc.concrete) with
        | Pexp_unreachable ->
          Expression_desc.of_concrete Pexp_unreachable
        | Pexp_extension x0 ->
          let x0 = self#extension _ctx x0 in
          Expression_desc.of_concrete (Pexp_extension x0)
        | Pexp_open (x0, x1, x2) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          let x2 = self#override_flag _ctx x2 in
          Expression_desc.of_concrete (Pexp_open (x0, x1, x2))
        | Pexp_pack x0 ->
          let x0 = self#module_expr _ctx x0 in
          Expression_desc.of_concrete (Pexp_pack x0)
        | Pexp_newtype (x0, x1) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#loc self#string _ctx x1 in
          Expression_desc.of_concrete (Pexp_newtype (x0, x1))
        | Pexp_object x0 ->
          let x0 = self#class_structure _ctx x0 in
          Expression_desc.of_concrete (Pexp_object x0)
        | Pexp_poly (x0, x1) ->
          let x0 = self#option self#core_type _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Expression_desc.of_concrete (Pexp_poly (x0, x1))
        | Pexp_lazy x0 ->
          let x0 = self#expression _ctx x0 in
          Expression_desc.of_concrete (Pexp_lazy x0)
        | Pexp_assert x0 ->
          let x0 = self#expression _ctx x0 in
          Expression_desc.of_concrete (Pexp_assert x0)
        | Pexp_letexception (x0, x1) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#extension_constructor _ctx x1 in
          Expression_desc.of_concrete (Pexp_letexception (x0, x1))
        | Pexp_letmodule (x0, x1, x2) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#module_expr _ctx x1 in
          let x2 = self#loc self#string _ctx x2 in
          Expression_desc.of_concrete (Pexp_letmodule (x0, x1, x2))
        | Pexp_override x0 ->
          let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#expression _ctx x0 in let x1 = self#loc self#string _ctx x1 in (x0, x1)) _ctx x0 in
          Expression_desc.of_concrete (Pexp_override x0)
        | Pexp_setinstvar (x0, x1) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#loc self#string _ctx x1 in
          Expression_desc.of_concrete (Pexp_setinstvar (x0, x1))
        | Pexp_new x0 ->
          let x0 = self#longident_loc _ctx x0 in
          Expression_desc.of_concrete (Pexp_new x0)
        | Pexp_send (x0, x1) ->
          let x0 = self#loc self#string _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Expression_desc.of_concrete (Pexp_send (x0, x1))
        | Pexp_coerce (x0, x1, x2) ->
          let x0 = self#core_type _ctx x0 in
          let x1 = self#option self#core_type _ctx x1 in
          let x2 = self#expression _ctx x2 in
          Expression_desc.of_concrete (Pexp_coerce (x0, x1, x2))
        | Pexp_constraint (x0, x1) ->
          let x0 = self#core_type _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Expression_desc.of_concrete (Pexp_constraint (x0, x1))
        | Pexp_for (x0, x1, x2, x3, x4) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#direction_flag _ctx x1 in
          let x2 = self#expression _ctx x2 in
          let x3 = self#expression _ctx x3 in
          let x4 = self#pattern _ctx x4 in
          Expression_desc.of_concrete (Pexp_for (x0, x1, x2, x3, x4))
        | Pexp_while (x0, x1) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Expression_desc.of_concrete (Pexp_while (x0, x1))
        | Pexp_sequence (x0, x1) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Expression_desc.of_concrete (Pexp_sequence (x0, x1))
        | Pexp_ifthenelse (x0, x1, x2) ->
          let x0 = self#option self#expression _ctx x0 in
          let x1 = self#expression _ctx x1 in
          let x2 = self#expression _ctx x2 in
          Expression_desc.of_concrete (Pexp_ifthenelse (x0, x1, x2))
        | Pexp_array x0 ->
          let x0 = self#list self#expression _ctx x0 in
          Expression_desc.of_concrete (Pexp_array x0)
        | Pexp_setfield (x0, x1, x2) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          let x2 = self#expression _ctx x2 in
          Expression_desc.of_concrete (Pexp_setfield (x0, x1, x2))
        | Pexp_field (x0, x1) ->
          let x0 = self#longident_loc _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Expression_desc.of_concrete (Pexp_field (x0, x1))
        | Pexp_record (x0, x1) ->
          let x0 = self#option self#expression _ctx x0 in
          let x1 = self#list (fun _ctx (x0, x1) -> let x0 = self#expression _ctx x0 in let x1 = self#longident_loc _ctx x1 in (x0, x1)) _ctx x1 in
          Expression_desc.of_concrete (Pexp_record (x0, x1))
        | Pexp_variant (x0, x1) ->
          let x0 = self#option self#expression _ctx x0 in
          let x1 = self#string _ctx x1 in
          Expression_desc.of_concrete (Pexp_variant (x0, x1))
        | Pexp_construct (x0, x1) ->
          let x0 = self#option self#expression _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          Expression_desc.of_concrete (Pexp_construct (x0, x1))
        | Pexp_tuple x0 ->
          let x0 = self#list self#expression _ctx x0 in
          Expression_desc.of_concrete (Pexp_tuple x0)
        | Pexp_try (x0, x1) ->
          let x0 = self#list self#case _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Expression_desc.of_concrete (Pexp_try (x0, x1))
        | Pexp_match (x0, x1) ->
          let x0 = self#list self#case _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Expression_desc.of_concrete (Pexp_match (x0, x1))
        | Pexp_apply (x0, x1) ->
          let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#expression _ctx x0 in let x1 = self#arg_label _ctx x1 in (x0, x1)) _ctx x0 in
          let x1 = self#expression _ctx x1 in
          Expression_desc.of_concrete (Pexp_apply (x0, x1))
        | Pexp_fun (x0, x1, x2, x3) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#pattern _ctx x1 in
          let x2 = self#option self#expression _ctx x2 in
          let x3 = self#arg_label _ctx x3 in
          Expression_desc.of_concrete (Pexp_fun (x0, x1, x2, x3))
        | Pexp_function x0 ->
          let x0 = self#list self#case _ctx x0 in
          Expression_desc.of_concrete (Pexp_function x0)
        | Pexp_let (x0, x1, x2) ->
          let x0 = self#expression _ctx x0 in
          let x1 = self#list self#value_binding _ctx x1 in
          let x2 = self#rec_flag _ctx x2 in
          Expression_desc.of_concrete (Pexp_let (x0, x1, x2))
        | Pexp_constant x0 ->
          let x0 = self#constant _ctx x0 in
          Expression_desc.of_concrete (Pexp_constant x0)
        | Pexp_ident x0 ->
          let x0 = self#longident_loc _ctx x0 in
          Expression_desc.of_concrete (Pexp_ident x0)
    method expression : 'ctx -> Expression.t -> Expression.t  =
      fun _ctx expression ->
        let concrete = Expression.to_concrete expression in
        let { pexp_attributes; pexp_loc; pexp_desc } : Expression.concrete = concrete in
        let pexp_attributes = self#attributes _ctx pexp_attributes in
        let pexp_loc = self#location _ctx pexp_loc in
        let pexp_desc = self#expression_desc _ctx pexp_desc in
        Expression.of_concrete { pexp_attributes; pexp_loc; pexp_desc }
    method pattern_desc : 'ctx -> Pattern_desc.t -> Pattern_desc.t  =
      fun _ctx pattern_desc ->
        let concrete = Pattern_desc.to_concrete pattern_desc in
        match (concrete : Pattern_desc.concrete) with
        | Ppat_open (x0, x1) ->
          let x0 = self#pattern _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          Pattern_desc.of_concrete (Ppat_open (x0, x1))
        | Ppat_extension x0 ->
          let x0 = self#extension _ctx x0 in
          Pattern_desc.of_concrete (Ppat_extension x0)
        | Ppat_exception x0 ->
          let x0 = self#pattern _ctx x0 in
          Pattern_desc.of_concrete (Ppat_exception x0)
        | Ppat_unpack x0 ->
          let x0 = self#loc self#string _ctx x0 in
          Pattern_desc.of_concrete (Ppat_unpack x0)
        | Ppat_lazy x0 ->
          let x0 = self#pattern _ctx x0 in
          Pattern_desc.of_concrete (Ppat_lazy x0)
        | Ppat_type x0 ->
          let x0 = self#longident_loc _ctx x0 in
          Pattern_desc.of_concrete (Ppat_type x0)
        | Ppat_constraint (x0, x1) ->
          let x0 = self#core_type _ctx x0 in
          let x1 = self#pattern _ctx x1 in
          Pattern_desc.of_concrete (Ppat_constraint (x0, x1))
        | Ppat_or (x0, x1) ->
          let x0 = self#pattern _ctx x0 in
          let x1 = self#pattern _ctx x1 in
          Pattern_desc.of_concrete (Ppat_or (x0, x1))
        | Ppat_array x0 ->
          let x0 = self#list self#pattern _ctx x0 in
          Pattern_desc.of_concrete (Ppat_array x0)
        | Ppat_record (x0, x1) ->
          let x0 = self#closed_flag _ctx x0 in
          let x1 = self#list (fun _ctx (x0, x1) -> let x0 = self#pattern _ctx x0 in let x1 = self#longident_loc _ctx x1 in (x0, x1)) _ctx x1 in
          Pattern_desc.of_concrete (Ppat_record (x0, x1))
        | Ppat_variant (x0, x1) ->
          let x0 = self#option self#pattern _ctx x0 in
          let x1 = self#string _ctx x1 in
          Pattern_desc.of_concrete (Ppat_variant (x0, x1))
        | Ppat_construct (x0, x1) ->
          let x0 = self#option self#pattern _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          Pattern_desc.of_concrete (Ppat_construct (x0, x1))
        | Ppat_tuple x0 ->
          let x0 = self#list self#pattern _ctx x0 in
          Pattern_desc.of_concrete (Ppat_tuple x0)
        | Ppat_interval (x0, x1) ->
          let x0 = self#constant _ctx x0 in
          let x1 = self#constant _ctx x1 in
          Pattern_desc.of_concrete (Ppat_interval (x0, x1))
        | Ppat_constant x0 ->
          let x0 = self#constant _ctx x0 in
          Pattern_desc.of_concrete (Ppat_constant x0)
        | Ppat_alias (x0, x1) ->
          let x0 = self#loc self#string _ctx x0 in
          let x1 = self#pattern _ctx x1 in
          Pattern_desc.of_concrete (Ppat_alias (x0, x1))
        | Ppat_var x0 ->
          let x0 = self#loc self#string _ctx x0 in
          Pattern_desc.of_concrete (Ppat_var x0)
        | Ppat_any ->
          Pattern_desc.of_concrete Ppat_any
    method pattern : 'ctx -> Pattern.t -> Pattern.t  =
      fun _ctx pattern ->
        let concrete = Pattern.to_concrete pattern in
        let { ppat_attributes; ppat_loc; ppat_desc } : Pattern.concrete = concrete in
        let ppat_attributes = self#attributes _ctx ppat_attributes in
        let ppat_loc = self#location _ctx ppat_loc in
        let ppat_desc = self#pattern_desc _ctx ppat_desc in
        Pattern.of_concrete { ppat_attributes; ppat_loc; ppat_desc }
    method object_field : 'ctx -> Object_field.t -> Object_field.t  =
      fun _ctx object_field ->
        let concrete = Object_field.to_concrete object_field in
        match (concrete : Object_field.concrete) with
        | Oinherit x0 ->
          let x0 = self#core_type _ctx x0 in
          Object_field.of_concrete (Oinherit x0)
        | Otag (x0, x1, x2) ->
          let x0 = self#core_type _ctx x0 in
          let x1 = self#attributes _ctx x1 in
          let x2 = self#loc self#string _ctx x2 in
          Object_field.of_concrete (Otag (x0, x1, x2))
    method row_field : 'ctx -> Row_field.t -> Row_field.t  =
      fun _ctx row_field ->
        let concrete = Row_field.to_concrete row_field in
        match (concrete : Row_field.concrete) with
        | Rinherit x0 ->
          let x0 = self#core_type _ctx x0 in
          Row_field.of_concrete (Rinherit x0)
        | Rtag (x0, x1, x2, x3) ->
          let x0 = self#list self#core_type _ctx x0 in
          let x1 = self#bool _ctx x1 in
          let x2 = self#attributes _ctx x2 in
          let x3 = self#loc self#string _ctx x3 in
          Row_field.of_concrete (Rtag (x0, x1, x2, x3))
    method package_type : 'ctx -> Package_type.t -> Package_type.t  =
      fun _ctx package_type ->
        let concrete = Package_type.to_concrete package_type in
        let (x0, x1) = concrete in
        let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#longident_loc _ctx x1 in (x0, x1)) _ctx x0 in
        let x1 = self#longident_loc _ctx x1 in
        Package_type.of_concrete (x0, x1)
    method core_type_desc : 'ctx -> Core_type_desc.t -> Core_type_desc.t  =
      fun _ctx core_type_desc ->
        let concrete = Core_type_desc.to_concrete core_type_desc in
        match (concrete : Core_type_desc.concrete) with
        | Ptyp_extension x0 ->
          let x0 = self#extension _ctx x0 in
          Core_type_desc.of_concrete (Ptyp_extension x0)
        | Ptyp_package x0 ->
          let x0 = self#package_type _ctx x0 in
          Core_type_desc.of_concrete (Ptyp_package x0)
        | Ptyp_poly (x0, x1) ->
          let x0 = self#core_type _ctx x0 in
          let x1 = self#list (self#loc self#string) _ctx x1 in
          Core_type_desc.of_concrete (Ptyp_poly (x0, x1))
        | Ptyp_variant (x0, x1, x2) ->
          let x0 = self#option (self#list self#string) _ctx x0 in
          let x1 = self#closed_flag _ctx x1 in
          let x2 = self#list self#row_field _ctx x2 in
          Core_type_desc.of_concrete (Ptyp_variant (x0, x1, x2))
        | Ptyp_alias (x0, x1) ->
          let x0 = self#string _ctx x0 in
          let x1 = self#core_type _ctx x1 in
          Core_type_desc.of_concrete (Ptyp_alias (x0, x1))
        | Ptyp_class (x0, x1) ->
          let x0 = self#list self#core_type _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          Core_type_desc.of_concrete (Ptyp_class (x0, x1))
        | Ptyp_object (x0, x1) ->
          let x0 = self#closed_flag _ctx x0 in
          let x1 = self#list self#object_field _ctx x1 in
          Core_type_desc.of_concrete (Ptyp_object (x0, x1))
        | Ptyp_constr (x0, x1) ->
          let x0 = self#list self#core_type _ctx x0 in
          let x1 = self#longident_loc _ctx x1 in
          Core_type_desc.of_concrete (Ptyp_constr (x0, x1))
        | Ptyp_tuple x0 ->
          let x0 = self#list self#core_type _ctx x0 in
          Core_type_desc.of_concrete (Ptyp_tuple x0)
        | Ptyp_arrow (x0, x1, x2) ->
          let x0 = self#core_type _ctx x0 in
          let x1 = self#core_type _ctx x1 in
          let x2 = self#arg_label _ctx x2 in
          Core_type_desc.of_concrete (Ptyp_arrow (x0, x1, x2))
        | Ptyp_var x0 ->
          let x0 = self#string _ctx x0 in
          Core_type_desc.of_concrete (Ptyp_var x0)
        | Ptyp_any ->
          Core_type_desc.of_concrete Ptyp_any
    method core_type : 'ctx -> Core_type.t -> Core_type.t  =
      fun _ctx core_type ->
        let concrete = Core_type.to_concrete core_type in
        let { ptyp_attributes; ptyp_loc; ptyp_desc } : Core_type.concrete = concrete in
        let ptyp_attributes = self#attributes _ctx ptyp_attributes in
        let ptyp_loc = self#location _ctx ptyp_loc in
        let ptyp_desc = self#core_type_desc _ctx ptyp_desc in
        Core_type.of_concrete { ptyp_attributes; ptyp_loc; ptyp_desc }
    method payload : 'ctx -> Payload.t -> Payload.t  =
      fun _ctx payload ->
        let concrete = Payload.to_concrete payload in
        match (concrete : Payload.concrete) with
        | PPat (x0, x1) ->
          let x0 = self#option self#expression _ctx x0 in
          let x1 = self#pattern _ctx x1 in
          Payload.of_concrete (PPat (x0, x1))
        | PTyp x0 ->
          let x0 = self#core_type _ctx x0 in
          Payload.of_concrete (PTyp x0)
        | PSig x0 ->
          let x0 = self#signature _ctx x0 in
          Payload.of_concrete (PSig x0)
        | PStr x0 ->
          let x0 = self#structure _ctx x0 in
          Payload.of_concrete (PStr x0)
    method attributes : 'ctx -> Attributes.t -> Attributes.t  =
      fun _ctx attributes ->
        let concrete = Attributes.to_concrete attributes in
        let concrete = self#list self#attribute _ctx concrete in
        Attributes.of_concrete concrete
    method extension : 'ctx -> Extension.t -> Extension.t  =
      fun _ctx extension ->
        let concrete = Extension.to_concrete extension in
        let (x0, x1) = concrete in
        let x0 = self#payload _ctx x0 in
        let x1 = self#loc self#string _ctx x1 in
        Extension.of_concrete (x0, x1)
    method attribute : 'ctx -> Attribute.t -> Attribute.t  =
      fun _ctx attribute ->
        let concrete = Attribute.to_concrete attribute in
        let (x0, x1) = concrete in
        let x0 = self#payload _ctx x0 in
        let x1 = self#loc self#string _ctx x1 in
        Attribute.of_concrete (x0, x1)
    method constant : 'ctx -> Constant.t -> Constant.t  =
      fun _ctx constant ->
        let concrete = Constant.to_concrete constant in
        match (concrete : Constant.concrete) with
        | Pconst_float (x0, x1) ->
          let x0 = self#option self#char _ctx x0 in
          let x1 = self#string _ctx x1 in
          Constant.of_concrete (Pconst_float (x0, x1))
        | Pconst_string (x0, x1) ->
          let x0 = self#option self#string _ctx x0 in
          let x1 = self#string _ctx x1 in
          Constant.of_concrete (Pconst_string (x0, x1))
        | Pconst_char x0 ->
          let x0 = self#char _ctx x0 in
          Constant.of_concrete (Pconst_char x0)
        | Pconst_integer (x0, x1) ->
          let x0 = self#option self#char _ctx x0 in
          let x1 = self#string _ctx x1 in
          Constant.of_concrete (Pconst_integer (x0, x1))
    method variance : 'ctx -> Variance.t -> Variance.t  =
      fun _ctx variance ->
        let concrete = Variance.to_concrete variance in
        match (concrete : Variance.concrete) with
        | Invariant ->
          Variance.of_concrete Invariant
        | Contravariant ->
          Variance.of_concrete Contravariant
        | Covariant ->
          Variance.of_concrete Covariant
    method arg_label : 'ctx -> Arg_label.t -> Arg_label.t  =
      fun _ctx arg_label ->
        let concrete = Arg_label.to_concrete arg_label in
        match (concrete : Arg_label.concrete) with
        | Optional x0 ->
          let x0 = self#string _ctx x0 in
          Arg_label.of_concrete (Optional x0)
        | Labelled x0 ->
          let x0 = self#string _ctx x0 in
          Arg_label.of_concrete (Labelled x0)
        | Nolabel ->
          Arg_label.of_concrete Nolabel
    method closed_flag : 'ctx -> Closed_flag.t -> Closed_flag.t  =
      fun _ctx closed_flag ->
        let concrete = Closed_flag.to_concrete closed_flag in
        match (concrete : Closed_flag.concrete) with
        | Open ->
          Closed_flag.of_concrete Open
        | Closed ->
          Closed_flag.of_concrete Closed
    method override_flag : 'ctx -> Override_flag.t -> Override_flag.t  =
      fun _ctx override_flag ->
        let concrete = Override_flag.to_concrete override_flag in
        match (concrete : Override_flag.concrete) with
        | Fresh ->
          Override_flag.of_concrete Fresh
        | Override ->
          Override_flag.of_concrete Override
    method virtual_flag : 'ctx -> Virtual_flag.t -> Virtual_flag.t  =
      fun _ctx virtual_flag ->
        let concrete = Virtual_flag.to_concrete virtual_flag in
        match (concrete : Virtual_flag.concrete) with
        | Concrete ->
          Virtual_flag.of_concrete Concrete
        | Virtual ->
          Virtual_flag.of_concrete Virtual
    method mutable_flag : 'ctx -> Mutable_flag.t -> Mutable_flag.t  =
      fun _ctx mutable_flag ->
        let concrete = Mutable_flag.to_concrete mutable_flag in
        match (concrete : Mutable_flag.concrete) with
        | Mutable ->
          Mutable_flag.of_concrete Mutable
        | Immutable ->
          Mutable_flag.of_concrete Immutable
    method private_flag : 'ctx -> Private_flag.t -> Private_flag.t  =
      fun _ctx private_flag ->
        let concrete = Private_flag.to_concrete private_flag in
        match (concrete : Private_flag.concrete) with
        | Public ->
          Private_flag.of_concrete Public
        | Private ->
          Private_flag.of_concrete Private
    method direction_flag : 'ctx -> Direction_flag.t -> Direction_flag.t  =
      fun _ctx direction_flag ->
        let concrete = Direction_flag.to_concrete direction_flag in
        match (concrete : Direction_flag.concrete) with
        | Downto ->
          Direction_flag.of_concrete Downto
        | Upto ->
          Direction_flag.of_concrete Upto
    method rec_flag : 'ctx -> Rec_flag.t -> Rec_flag.t  =
      fun _ctx rec_flag ->
        let concrete = Rec_flag.to_concrete rec_flag in
        match (concrete : Rec_flag.concrete) with
        | Recursive ->
          Rec_flag.of_concrete Recursive
        | Nonrecursive ->
          Rec_flag.of_concrete Nonrecursive
    method longident_loc : 'ctx -> Longident_loc.t -> Longident_loc.t  =
      fun _ctx longident_loc ->
        let concrete = Longident_loc.to_concrete longident_loc in
        let concrete = self#loc self#longident _ctx concrete in
        Longident_loc.of_concrete concrete
    method longident : 'ctx -> Longident.t -> Longident.t  =
      fun _ctx longident ->
        let concrete = Longident.to_concrete longident in
        match (concrete : Longident.concrete) with
        | Lapply (x0, x1) ->
          let x0 = self#longident _ctx x0 in
          let x1 = self#longident _ctx x1 in
          Longident.of_concrete (Lapply (x0, x1))
        | Ldot (x0, x1) ->
          let x0 = self#string _ctx x0 in
          let x1 = self#longident _ctx x1 in
          Longident.of_concrete (Ldot (x0, x1))
        | Lident x0 ->
          let x0 = self#string _ctx x0 in
          Longident.of_concrete (Lident x0)
  end

class virtual ['res] lift =
  object (self)
    method virtual node : (string * int) option -> 'res -> 'res
    method virtual record : (string * int) option -> (string * 'res) list -> 'res
    method virtual constr : (string * int) option -> string -> 'res list -> 'res
    method virtual tuple : 'res list -> 'res
    method virtual bool : bool -> 'res
    method virtual char : char -> 'res
    method virtual int : int -> 'res
    method virtual list : 'a . ('a -> 'res) -> 'a list -> 'res
    method virtual option : 'a . ('a -> 'res) -> 'a option -> 'res
    method virtual string : string -> 'res
    method virtual location : Astlib.Location.t -> 'res
    method virtual loc : 'a . ('a -> 'res) -> 'a Astlib.Loc.t -> 'res
    method directive_argument : Directive_argument.t -> 'res  =
      fun directive_argument ->
        let concrete = Directive_argument.to_concrete directive_argument in
        match (concrete : Directive_argument.concrete) with
        | Pdir_bool x0 ->
          let x0 = self#bool x0 in
          self#constr (Some ("directive_argument", 0)) "Pdir_bool" [x0]
        | Pdir_ident x0 ->
          let x0 = self#longident x0 in
          self#constr (Some ("directive_argument", 0)) "Pdir_ident" [x0]
        | Pdir_int (x0, x1) ->
          let x0 = self#option self#char x0 in
          let x1 = self#string x1 in
          self#constr (Some ("directive_argument", 0)) "Pdir_int" [x0; x1]
        | Pdir_string x0 ->
          let x0 = self#string x0 in
          self#constr (Some ("directive_argument", 0)) "Pdir_string" [x0]
        | Pdir_none ->
          self#constr (Some ("directive_argument", 0)) "Pdir_none" []
    method toplevel_phrase : Toplevel_phrase.t -> 'res  =
      fun toplevel_phrase ->
        let concrete = Toplevel_phrase.to_concrete toplevel_phrase in
        match (concrete : Toplevel_phrase.concrete) with
        | Ptop_dir (x0, x1) ->
          let x0 = self#directive_argument x0 in
          let x1 = self#string x1 in
          self#constr (Some ("toplevel_phrase", 0)) "Ptop_dir" [x0; x1]
        | Ptop_def x0 ->
          let x0 = self#structure x0 in
          self#constr (Some ("toplevel_phrase", 0)) "Ptop_def" [x0]
    method module_binding : Module_binding.t -> 'res  =
      fun module_binding ->
        let concrete = Module_binding.to_concrete module_binding in
        let { pmb_loc; pmb_attributes; pmb_expr; pmb_name } : Module_binding.concrete = concrete in
        let pmb_loc = self#location pmb_loc in
        let pmb_attributes = self#attributes pmb_attributes in
        let pmb_expr = self#module_expr pmb_expr in
        let pmb_name = self#loc self#string pmb_name in
        self#record (Some ("module_binding", 0)) [("pmb_loc", pmb_loc); ("pmb_attributes", pmb_attributes); ("pmb_expr", pmb_expr); ("pmb_name", pmb_name)]
    method value_binding : Value_binding.t -> 'res  =
      fun value_binding ->
        let concrete = Value_binding.to_concrete value_binding in
        let { pvb_loc; pvb_attributes; pvb_expr; pvb_pat } : Value_binding.concrete = concrete in
        let pvb_loc = self#location pvb_loc in
        let pvb_attributes = self#attributes pvb_attributes in
        let pvb_expr = self#expression pvb_expr in
        let pvb_pat = self#pattern pvb_pat in
        self#record (Some ("value_binding", 0)) [("pvb_loc", pvb_loc); ("pvb_attributes", pvb_attributes); ("pvb_expr", pvb_expr); ("pvb_pat", pvb_pat)]
    method structure_item_desc : Structure_item_desc.t -> 'res  =
      fun structure_item_desc ->
        let concrete = Structure_item_desc.to_concrete structure_item_desc in
        match (concrete : Structure_item_desc.concrete) with
        | Pstr_extension (x0, x1) ->
          let x0 = self#attributes x0 in
          let x1 = self#extension x1 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_extension" [x0; x1]
        | Pstr_attribute x0 ->
          let x0 = self#attribute x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_attribute" [x0]
        | Pstr_include x0 ->
          let x0 = self#include_declaration x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_include" [x0]
        | Pstr_class_type x0 ->
          let x0 = self#list self#class_type_declaration x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_class_type" [x0]
        | Pstr_class x0 ->
          let x0 = self#list self#class_declaration x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_class" [x0]
        | Pstr_open x0 ->
          let x0 = self#open_description x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_open" [x0]
        | Pstr_modtype x0 ->
          let x0 = self#module_type_declaration x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_modtype" [x0]
        | Pstr_recmodule x0 ->
          let x0 = self#list self#module_binding x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_recmodule" [x0]
        | Pstr_module x0 ->
          let x0 = self#module_binding x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_module" [x0]
        | Pstr_exception x0 ->
          let x0 = self#extension_constructor x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_exception" [x0]
        | Pstr_typext x0 ->
          let x0 = self#type_extension x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_typext" [x0]
        | Pstr_type (x0, x1) ->
          let x0 = self#list self#type_declaration x0 in
          let x1 = self#rec_flag x1 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_type" [x0; x1]
        | Pstr_primitive x0 ->
          let x0 = self#value_description x0 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_primitive" [x0]
        | Pstr_value (x0, x1) ->
          let x0 = self#list self#value_binding x0 in
          let x1 = self#rec_flag x1 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_value" [x0; x1]
        | Pstr_eval (x0, x1) ->
          let x0 = self#attributes x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("structure_item_desc", 0)) "Pstr_eval" [x0; x1]
    method structure_item : Structure_item.t -> 'res  =
      fun structure_item ->
        let concrete = Structure_item.to_concrete structure_item in
        let { pstr_loc; pstr_desc } : Structure_item.concrete = concrete in
        let pstr_loc = self#location pstr_loc in
        let pstr_desc = self#structure_item_desc pstr_desc in
        self#record (Some ("structure_item", 0)) [("pstr_loc", pstr_loc); ("pstr_desc", pstr_desc)]
    method structure : Structure.t -> 'res  =
      fun structure ->
        let concrete = Structure.to_concrete structure in
        let concrete = self#list self#structure_item concrete in
        self#node (Some ("structure", 0)) concrete
    method module_expr_desc : Module_expr_desc.t -> 'res  =
      fun module_expr_desc ->
        let concrete = Module_expr_desc.to_concrete module_expr_desc in
        match (concrete : Module_expr_desc.concrete) with
        | Pmod_extension x0 ->
          let x0 = self#extension x0 in
          self#constr (Some ("module_expr_desc", 0)) "Pmod_extension" [x0]
        | Pmod_unpack x0 ->
          let x0 = self#expression x0 in
          self#constr (Some ("module_expr_desc", 0)) "Pmod_unpack" [x0]
        | Pmod_constraint (x0, x1) ->
          let x0 = self#module_type x0 in
          let x1 = self#module_expr x1 in
          self#constr (Some ("module_expr_desc", 0)) "Pmod_constraint" [x0; x1]
        | Pmod_apply (x0, x1) ->
          let x0 = self#module_expr x0 in
          let x1 = self#module_expr x1 in
          self#constr (Some ("module_expr_desc", 0)) "Pmod_apply" [x0; x1]
        | Pmod_functor (x0, x1, x2) ->
          let x0 = self#module_expr x0 in
          let x1 = self#option self#module_type x1 in
          let x2 = self#loc self#string x2 in
          self#constr (Some ("module_expr_desc", 0)) "Pmod_functor" [x0; x1; x2]
        | Pmod_structure x0 ->
          let x0 = self#structure x0 in
          self#constr (Some ("module_expr_desc", 0)) "Pmod_structure" [x0]
        | Pmod_ident x0 ->
          let x0 = self#longident_loc x0 in
          self#constr (Some ("module_expr_desc", 0)) "Pmod_ident" [x0]
    method module_expr : Module_expr.t -> 'res  =
      fun module_expr ->
        let concrete = Module_expr.to_concrete module_expr in
        let { pmod_attributes; pmod_loc; pmod_desc } : Module_expr.concrete = concrete in
        let pmod_attributes = self#attributes pmod_attributes in
        let pmod_loc = self#location pmod_loc in
        let pmod_desc = self#module_expr_desc pmod_desc in
        self#record (Some ("module_expr", 0)) [("pmod_attributes", pmod_attributes); ("pmod_loc", pmod_loc); ("pmod_desc", pmod_desc)]
    method with_constraint : With_constraint.t -> 'res  =
      fun with_constraint ->
        let concrete = With_constraint.to_concrete with_constraint in
        match (concrete : With_constraint.concrete) with
        | Pwith_modsubst (x0, x1) ->
          let x0 = self#longident_loc x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("with_constraint", 0)) "Pwith_modsubst" [x0; x1]
        | Pwith_typesubst (x0, x1) ->
          let x0 = self#type_declaration x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("with_constraint", 0)) "Pwith_typesubst" [x0; x1]
        | Pwith_module (x0, x1) ->
          let x0 = self#longident_loc x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("with_constraint", 0)) "Pwith_module" [x0; x1]
        | Pwith_type (x0, x1) ->
          let x0 = self#type_declaration x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("with_constraint", 0)) "Pwith_type" [x0; x1]
    method include_declaration : Include_declaration.t -> 'res  =
      fun include_declaration ->
        let concrete = Include_declaration.to_concrete include_declaration in
        let concrete = self#include_infos_module_expr concrete in
        self#node (Some ("include_declaration", 0)) concrete
    method include_description : Include_description.t -> 'res  =
      fun include_description ->
        let concrete = Include_description.to_concrete include_description in
        let concrete = self#include_infos_module_type concrete in
        self#node (Some ("include_description", 0)) concrete
    method include_infos_module_expr : Module_expr.t Include_infos.t -> 'res  =
      fun include_infos ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_expr.t Include_infos.concrete = concrete in
        let pincl_attributes = self#attributes pincl_attributes in
        let pincl_loc = self#location pincl_loc in
        let pincl_mod = self#module_expr pincl_mod in
        self#record (Some ("include_infos", 1)) [("pincl_attributes", pincl_attributes); ("pincl_loc", pincl_loc); ("pincl_mod", pincl_mod)]
    method include_infos_module_type : Module_type.t Include_infos.t -> 'res  =
      fun include_infos ->
        let concrete = Include_infos.to_concrete include_infos in
        let { pincl_attributes; pincl_loc; pincl_mod } : Module_type.t Include_infos.concrete = concrete in
        let pincl_attributes = self#attributes pincl_attributes in
        let pincl_loc = self#location pincl_loc in
        let pincl_mod = self#module_type pincl_mod in
        self#record (Some ("include_infos", 1)) [("pincl_attributes", pincl_attributes); ("pincl_loc", pincl_loc); ("pincl_mod", pincl_mod)]
    method open_description : Open_description.t -> 'res  =
      fun open_description ->
        let concrete = Open_description.to_concrete open_description in
        let { popen_attributes; popen_loc; popen_override; popen_lid } : Open_description.concrete = concrete in
        let popen_attributes = self#attributes popen_attributes in
        let popen_loc = self#location popen_loc in
        let popen_override = self#override_flag popen_override in
        let popen_lid = self#longident_loc popen_lid in
        self#record (Some ("open_description", 0)) [("popen_attributes", popen_attributes); ("popen_loc", popen_loc); ("popen_override", popen_override); ("popen_lid", popen_lid)]
    method module_type_declaration : Module_type_declaration.t -> 'res  =
      fun module_type_declaration ->
        let concrete = Module_type_declaration.to_concrete module_type_declaration in
        let { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name } : Module_type_declaration.concrete = concrete in
        let pmtd_loc = self#location pmtd_loc in
        let pmtd_attributes = self#attributes pmtd_attributes in
        let pmtd_type = self#option self#module_type pmtd_type in
        let pmtd_name = self#loc self#string pmtd_name in
        self#record (Some ("module_type_declaration", 0)) [("pmtd_loc", pmtd_loc); ("pmtd_attributes", pmtd_attributes); ("pmtd_type", pmtd_type); ("pmtd_name", pmtd_name)]
    method module_declaration : Module_declaration.t -> 'res  =
      fun module_declaration ->
        let concrete = Module_declaration.to_concrete module_declaration in
        let { pmd_loc; pmd_attributes; pmd_type; pmd_name } : Module_declaration.concrete = concrete in
        let pmd_loc = self#location pmd_loc in
        let pmd_attributes = self#attributes pmd_attributes in
        let pmd_type = self#module_type pmd_type in
        let pmd_name = self#loc self#string pmd_name in
        self#record (Some ("module_declaration", 0)) [("pmd_loc", pmd_loc); ("pmd_attributes", pmd_attributes); ("pmd_type", pmd_type); ("pmd_name", pmd_name)]
    method signature_item_desc : Signature_item_desc.t -> 'res  =
      fun signature_item_desc ->
        let concrete = Signature_item_desc.to_concrete signature_item_desc in
        match (concrete : Signature_item_desc.concrete) with
        | Psig_extension (x0, x1) ->
          let x0 = self#attributes x0 in
          let x1 = self#extension x1 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_extension" [x0; x1]
        | Psig_attribute x0 ->
          let x0 = self#attribute x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_attribute" [x0]
        | Psig_class_type x0 ->
          let x0 = self#list self#class_type_declaration x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_class_type" [x0]
        | Psig_class x0 ->
          let x0 = self#list self#class_description x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_class" [x0]
        | Psig_include x0 ->
          let x0 = self#include_description x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_include" [x0]
        | Psig_open x0 ->
          let x0 = self#open_description x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_open" [x0]
        | Psig_modtype x0 ->
          let x0 = self#module_type_declaration x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_modtype" [x0]
        | Psig_recmodule x0 ->
          let x0 = self#list self#module_declaration x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_recmodule" [x0]
        | Psig_module x0 ->
          let x0 = self#module_declaration x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_module" [x0]
        | Psig_exception x0 ->
          let x0 = self#extension_constructor x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_exception" [x0]
        | Psig_typext x0 ->
          let x0 = self#type_extension x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_typext" [x0]
        | Psig_type (x0, x1) ->
          let x0 = self#list self#type_declaration x0 in
          let x1 = self#rec_flag x1 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_type" [x0; x1]
        | Psig_value x0 ->
          let x0 = self#value_description x0 in
          self#constr (Some ("signature_item_desc", 0)) "Psig_value" [x0]
    method signature_item : Signature_item.t -> 'res  =
      fun signature_item ->
        let concrete = Signature_item.to_concrete signature_item in
        let { psig_loc; psig_desc } : Signature_item.concrete = concrete in
        let psig_loc = self#location psig_loc in
        let psig_desc = self#signature_item_desc psig_desc in
        self#record (Some ("signature_item", 0)) [("psig_loc", psig_loc); ("psig_desc", psig_desc)]
    method signature : Signature.t -> 'res  =
      fun signature ->
        let concrete = Signature.to_concrete signature in
        let concrete = self#list self#signature_item concrete in
        self#node (Some ("signature", 0)) concrete
    method module_type_desc : Module_type_desc.t -> 'res  =
      fun module_type_desc ->
        let concrete = Module_type_desc.to_concrete module_type_desc in
        match (concrete : Module_type_desc.concrete) with
        | Pmty_alias x0 ->
          let x0 = self#longident_loc x0 in
          self#constr (Some ("module_type_desc", 0)) "Pmty_alias" [x0]
        | Pmty_extension x0 ->
          let x0 = self#extension x0 in
          self#constr (Some ("module_type_desc", 0)) "Pmty_extension" [x0]
        | Pmty_typeof x0 ->
          let x0 = self#module_expr x0 in
          self#constr (Some ("module_type_desc", 0)) "Pmty_typeof" [x0]
        | Pmty_with (x0, x1) ->
          let x0 = self#list self#with_constraint x0 in
          let x1 = self#module_type x1 in
          self#constr (Some ("module_type_desc", 0)) "Pmty_with" [x0; x1]
        | Pmty_functor (x0, x1, x2) ->
          let x0 = self#module_type x0 in
          let x1 = self#option self#module_type x1 in
          let x2 = self#loc self#string x2 in
          self#constr (Some ("module_type_desc", 0)) "Pmty_functor" [x0; x1; x2]
        | Pmty_signature x0 ->
          let x0 = self#signature x0 in
          self#constr (Some ("module_type_desc", 0)) "Pmty_signature" [x0]
        | Pmty_ident x0 ->
          let x0 = self#longident_loc x0 in
          self#constr (Some ("module_type_desc", 0)) "Pmty_ident" [x0]
    method module_type : Module_type.t -> 'res  =
      fun module_type ->
        let concrete = Module_type.to_concrete module_type in
        let { pmty_attributes; pmty_loc; pmty_desc } : Module_type.concrete = concrete in
        let pmty_attributes = self#attributes pmty_attributes in
        let pmty_loc = self#location pmty_loc in
        let pmty_desc = self#module_type_desc pmty_desc in
        self#record (Some ("module_type", 0)) [("pmty_attributes", pmty_attributes); ("pmty_loc", pmty_loc); ("pmty_desc", pmty_desc)]
    method class_declaration : Class_declaration.t -> 'res  =
      fun class_declaration ->
        let concrete = Class_declaration.to_concrete class_declaration in
        let concrete = self#class_infos_class_expr concrete in
        self#node (Some ("class_declaration", 0)) concrete
    method class_field_kind : Class_field_kind.t -> 'res  =
      fun class_field_kind ->
        let concrete = Class_field_kind.to_concrete class_field_kind in
        match (concrete : Class_field_kind.concrete) with
        | Cfk_concrete (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#override_flag x1 in
          self#constr (Some ("class_field_kind", 0)) "Cfk_concrete" [x0; x1]
        | Cfk_virtual x0 ->
          let x0 = self#core_type x0 in
          self#constr (Some ("class_field_kind", 0)) "Cfk_virtual" [x0]
    method class_field_desc : Class_field_desc.t -> 'res  =
      fun class_field_desc ->
        let concrete = Class_field_desc.to_concrete class_field_desc in
        match (concrete : Class_field_desc.concrete) with
        | Pcf_extension x0 ->
          let x0 = self#extension x0 in
          self#constr (Some ("class_field_desc", 0)) "Pcf_extension" [x0]
        | Pcf_attribute x0 ->
          let x0 = self#attribute x0 in
          self#constr (Some ("class_field_desc", 0)) "Pcf_attribute" [x0]
        | Pcf_initializer x0 ->
          let x0 = self#expression x0 in
          self#constr (Some ("class_field_desc", 0)) "Pcf_initializer" [x0]
        | Pcf_constraint x0 ->
          let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in self#node None (self#tuple [x0; x1])) x0 in
          self#constr (Some ("class_field_desc", 0)) "Pcf_constraint" [x0]
        | Pcf_method x0 ->
          let x0 = (fun (x0, x1, x2) -> let x0 = self#class_field_kind x0 in let x1 = self#private_flag x1 in let x2 = self#loc self#string x2 in self#node None (self#tuple [x0; x1; x2])) x0 in
          self#constr (Some ("class_field_desc", 0)) "Pcf_method" [x0]
        | Pcf_val x0 ->
          let x0 = (fun (x0, x1, x2) -> let x0 = self#class_field_kind x0 in let x1 = self#mutable_flag x1 in let x2 = self#loc self#string x2 in self#node None (self#tuple [x0; x1; x2])) x0 in
          self#constr (Some ("class_field_desc", 0)) "Pcf_val" [x0]
        | Pcf_inherit (x0, x1, x2) ->
          let x0 = self#option (self#loc self#string) x0 in
          let x1 = self#class_expr x1 in
          let x2 = self#override_flag x2 in
          self#constr (Some ("class_field_desc", 0)) "Pcf_inherit" [x0; x1; x2]
    method class_field : Class_field.t -> 'res  =
      fun class_field ->
        let concrete = Class_field.to_concrete class_field in
        let { pcf_attributes; pcf_loc; pcf_desc } : Class_field.concrete = concrete in
        let pcf_attributes = self#attributes pcf_attributes in
        let pcf_loc = self#location pcf_loc in
        let pcf_desc = self#class_field_desc pcf_desc in
        self#record (Some ("class_field", 0)) [("pcf_attributes", pcf_attributes); ("pcf_loc", pcf_loc); ("pcf_desc", pcf_desc)]
    method class_structure : Class_structure.t -> 'res  =
      fun class_structure ->
        let concrete = Class_structure.to_concrete class_structure in
        let { pcstr_fields; pcstr_self } : Class_structure.concrete = concrete in
        let pcstr_fields = self#list self#class_field pcstr_fields in
        let pcstr_self = self#pattern pcstr_self in
        self#record (Some ("class_structure", 0)) [("pcstr_fields", pcstr_fields); ("pcstr_self", pcstr_self)]
    method class_expr_desc : Class_expr_desc.t -> 'res  =
      fun class_expr_desc ->
        let concrete = Class_expr_desc.to_concrete class_expr_desc in
        match (concrete : Class_expr_desc.concrete) with
        | Pcl_open (x0, x1, x2) ->
          let x0 = self#class_expr x0 in
          let x1 = self#longident_loc x1 in
          let x2 = self#override_flag x2 in
          self#constr (Some ("class_expr_desc", 0)) "Pcl_open" [x0; x1; x2]
        | Pcl_extension x0 ->
          let x0 = self#extension x0 in
          self#constr (Some ("class_expr_desc", 0)) "Pcl_extension" [x0]
        | Pcl_constraint (x0, x1) ->
          let x0 = self#class_type x0 in
          let x1 = self#class_expr x1 in
          self#constr (Some ("class_expr_desc", 0)) "Pcl_constraint" [x0; x1]
        | Pcl_let (x0, x1, x2) ->
          let x0 = self#class_expr x0 in
          let x1 = self#list self#value_binding x1 in
          let x2 = self#rec_flag x2 in
          self#constr (Some ("class_expr_desc", 0)) "Pcl_let" [x0; x1; x2]
        | Pcl_apply (x0, x1) ->
          let x0 = self#list (fun (x0, x1) -> let x0 = self#expression x0 in let x1 = self#arg_label x1 in self#node None (self#tuple [x0; x1])) x0 in
          let x1 = self#class_expr x1 in
          self#constr (Some ("class_expr_desc", 0)) "Pcl_apply" [x0; x1]
        | Pcl_fun (x0, x1, x2, x3) ->
          let x0 = self#class_expr x0 in
          let x1 = self#pattern x1 in
          let x2 = self#option self#expression x2 in
          let x3 = self#arg_label x3 in
          self#constr (Some ("class_expr_desc", 0)) "Pcl_fun" [x0; x1; x2; x3]
        | Pcl_structure x0 ->
          let x0 = self#class_structure x0 in
          self#constr (Some ("class_expr_desc", 0)) "Pcl_structure" [x0]
        | Pcl_constr (x0, x1) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("class_expr_desc", 0)) "Pcl_constr" [x0; x1]
    method class_expr : Class_expr.t -> 'res  =
      fun class_expr ->
        let concrete = Class_expr.to_concrete class_expr in
        let { pcl_attributes; pcl_loc; pcl_desc } : Class_expr.concrete = concrete in
        let pcl_attributes = self#attributes pcl_attributes in
        let pcl_loc = self#location pcl_loc in
        let pcl_desc = self#class_expr_desc pcl_desc in
        self#record (Some ("class_expr", 0)) [("pcl_attributes", pcl_attributes); ("pcl_loc", pcl_loc); ("pcl_desc", pcl_desc)]
    method class_type_declaration : Class_type_declaration.t -> 'res  =
      fun class_type_declaration ->
        let concrete = Class_type_declaration.to_concrete class_type_declaration in
        let concrete = self#class_infos_class_type concrete in
        self#node (Some ("class_type_declaration", 0)) concrete
    method class_description : Class_description.t -> 'res  =
      fun class_description ->
        let concrete = Class_description.to_concrete class_description in
        let concrete = self#class_infos_class_type concrete in
        self#node (Some ("class_description", 0)) concrete
    method class_infos_class_expr : Class_expr.t Class_infos.t -> 'res  =
      fun class_infos ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_expr.t Class_infos.concrete = concrete in
        let pci_attributes = self#attributes pci_attributes in
        let pci_loc = self#location pci_loc in
        let pci_expr = self#class_expr pci_expr in
        let pci_name = self#loc self#string pci_name in
        let pci_params = self#list (fun (x0, x1) -> let x0 = self#variance x0 in let x1 = self#core_type x1 in self#node None (self#tuple [x0; x1])) pci_params in
        let pci_virt = self#virtual_flag pci_virt in
        self#record (Some ("class_infos", 1)) [("pci_attributes", pci_attributes); ("pci_loc", pci_loc); ("pci_expr", pci_expr); ("pci_name", pci_name); ("pci_params", pci_params); ("pci_virt", pci_virt)]
    method class_infos_class_type : Class_type.t Class_infos.t -> 'res  =
      fun class_infos ->
        let concrete = Class_infos.to_concrete class_infos in
        let { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } : Class_type.t Class_infos.concrete = concrete in
        let pci_attributes = self#attributes pci_attributes in
        let pci_loc = self#location pci_loc in
        let pci_expr = self#class_type pci_expr in
        let pci_name = self#loc self#string pci_name in
        let pci_params = self#list (fun (x0, x1) -> let x0 = self#variance x0 in let x1 = self#core_type x1 in self#node None (self#tuple [x0; x1])) pci_params in
        let pci_virt = self#virtual_flag pci_virt in
        self#record (Some ("class_infos", 1)) [("pci_attributes", pci_attributes); ("pci_loc", pci_loc); ("pci_expr", pci_expr); ("pci_name", pci_name); ("pci_params", pci_params); ("pci_virt", pci_virt)]
    method class_type_field_desc : Class_type_field_desc.t -> 'res  =
      fun class_type_field_desc ->
        let concrete = Class_type_field_desc.to_concrete class_type_field_desc in
        match (concrete : Class_type_field_desc.concrete) with
        | Pctf_extension x0 ->
          let x0 = self#extension x0 in
          self#constr (Some ("class_type_field_desc", 0)) "Pctf_extension" [x0]
        | Pctf_attribute x0 ->
          let x0 = self#attribute x0 in
          self#constr (Some ("class_type_field_desc", 0)) "Pctf_attribute" [x0]
        | Pctf_constraint x0 ->
          let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in self#node None (self#tuple [x0; x1])) x0 in
          self#constr (Some ("class_type_field_desc", 0)) "Pctf_constraint" [x0]
        | Pctf_method x0 ->
          let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#core_type x0 in let x1 = self#virtual_flag x1 in let x2 = self#private_flag x2 in let x3 = self#loc self#string x3 in self#node None (self#tuple [x0; x1; x2; x3])) x0 in
          self#constr (Some ("class_type_field_desc", 0)) "Pctf_method" [x0]
        | Pctf_val x0 ->
          let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#core_type x0 in let x1 = self#virtual_flag x1 in let x2 = self#mutable_flag x2 in let x3 = self#loc self#string x3 in self#node None (self#tuple [x0; x1; x2; x3])) x0 in
          self#constr (Some ("class_type_field_desc", 0)) "Pctf_val" [x0]
        | Pctf_inherit x0 ->
          let x0 = self#class_type x0 in
          self#constr (Some ("class_type_field_desc", 0)) "Pctf_inherit" [x0]
    method class_type_field : Class_type_field.t -> 'res  =
      fun class_type_field ->
        let concrete = Class_type_field.to_concrete class_type_field in
        let { pctf_attributes; pctf_loc; pctf_desc } : Class_type_field.concrete = concrete in
        let pctf_attributes = self#attributes pctf_attributes in
        let pctf_loc = self#location pctf_loc in
        let pctf_desc = self#class_type_field_desc pctf_desc in
        self#record (Some ("class_type_field", 0)) [("pctf_attributes", pctf_attributes); ("pctf_loc", pctf_loc); ("pctf_desc", pctf_desc)]
    method class_signature : Class_signature.t -> 'res  =
      fun class_signature ->
        let concrete = Class_signature.to_concrete class_signature in
        let { pcsig_fields; pcsig_self } : Class_signature.concrete = concrete in
        let pcsig_fields = self#list self#class_type_field pcsig_fields in
        let pcsig_self = self#core_type pcsig_self in
        self#record (Some ("class_signature", 0)) [("pcsig_fields", pcsig_fields); ("pcsig_self", pcsig_self)]
    method class_type_desc : Class_type_desc.t -> 'res  =
      fun class_type_desc ->
        let concrete = Class_type_desc.to_concrete class_type_desc in
        match (concrete : Class_type_desc.concrete) with
        | Pcty_open (x0, x1, x2) ->
          let x0 = self#class_type x0 in
          let x1 = self#longident_loc x1 in
          let x2 = self#override_flag x2 in
          self#constr (Some ("class_type_desc", 0)) "Pcty_open" [x0; x1; x2]
        | Pcty_extension x0 ->
          let x0 = self#extension x0 in
          self#constr (Some ("class_type_desc", 0)) "Pcty_extension" [x0]
        | Pcty_arrow (x0, x1, x2) ->
          let x0 = self#class_type x0 in
          let x1 = self#core_type x1 in
          let x2 = self#arg_label x2 in
          self#constr (Some ("class_type_desc", 0)) "Pcty_arrow" [x0; x1; x2]
        | Pcty_signature x0 ->
          let x0 = self#class_signature x0 in
          self#constr (Some ("class_type_desc", 0)) "Pcty_signature" [x0]
        | Pcty_constr (x0, x1) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("class_type_desc", 0)) "Pcty_constr" [x0; x1]
    method class_type : Class_type.t -> 'res  =
      fun class_type ->
        let concrete = Class_type.to_concrete class_type in
        let { pcty_attributes; pcty_loc; pcty_desc } : Class_type.concrete = concrete in
        let pcty_attributes = self#attributes pcty_attributes in
        let pcty_loc = self#location pcty_loc in
        let pcty_desc = self#class_type_desc pcty_desc in
        self#record (Some ("class_type", 0)) [("pcty_attributes", pcty_attributes); ("pcty_loc", pcty_loc); ("pcty_desc", pcty_desc)]
    method extension_constructor_kind : Extension_constructor_kind.t -> 'res  =
      fun extension_constructor_kind ->
        let concrete = Extension_constructor_kind.to_concrete extension_constructor_kind in
        match (concrete : Extension_constructor_kind.concrete) with
        | Pext_rebind x0 ->
          let x0 = self#longident_loc x0 in
          self#constr (Some ("extension_constructor_kind", 0)) "Pext_rebind" [x0]
        | Pext_decl (x0, x1) ->
          let x0 = self#option self#core_type x0 in
          let x1 = self#constructor_arguments x1 in
          self#constr (Some ("extension_constructor_kind", 0)) "Pext_decl" [x0; x1]
    method extension_constructor : Extension_constructor.t -> 'res  =
      fun extension_constructor ->
        let concrete = Extension_constructor.to_concrete extension_constructor in
        let { pext_attributes; pext_loc; pext_kind; pext_name } : Extension_constructor.concrete = concrete in
        let pext_attributes = self#attributes pext_attributes in
        let pext_loc = self#location pext_loc in
        let pext_kind = self#extension_constructor_kind pext_kind in
        let pext_name = self#loc self#string pext_name in
        self#record (Some ("extension_constructor", 0)) [("pext_attributes", pext_attributes); ("pext_loc", pext_loc); ("pext_kind", pext_kind); ("pext_name", pext_name)]
    method type_extension : Type_extension.t -> 'res  =
      fun type_extension ->
        let concrete = Type_extension.to_concrete type_extension in
        let { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path } : Type_extension.concrete = concrete in
        let ptyext_attributes = self#attributes ptyext_attributes in
        let ptyext_private = self#private_flag ptyext_private in
        let ptyext_constructors = self#list self#extension_constructor ptyext_constructors in
        let ptyext_params = self#list (fun (x0, x1) -> let x0 = self#variance x0 in let x1 = self#core_type x1 in self#node None (self#tuple [x0; x1])) ptyext_params in
        let ptyext_path = self#longident_loc ptyext_path in
        self#record (Some ("type_extension", 0)) [("ptyext_attributes", ptyext_attributes); ("ptyext_private", ptyext_private); ("ptyext_constructors", ptyext_constructors); ("ptyext_params", ptyext_params); ("ptyext_path", ptyext_path)]
    method constructor_arguments : Constructor_arguments.t -> 'res  =
      fun constructor_arguments ->
        let concrete = Constructor_arguments.to_concrete constructor_arguments in
        match (concrete : Constructor_arguments.concrete) with
        | Pcstr_record x0 ->
          let x0 = self#list self#label_declaration x0 in
          self#constr (Some ("constructor_arguments", 0)) "Pcstr_record" [x0]
        | Pcstr_tuple x0 ->
          let x0 = self#list self#core_type x0 in
          self#constr (Some ("constructor_arguments", 0)) "Pcstr_tuple" [x0]
    method constructor_declaration : Constructor_declaration.t -> 'res  =
      fun constructor_declaration ->
        let concrete = Constructor_declaration.to_concrete constructor_declaration in
        let { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name } : Constructor_declaration.concrete = concrete in
        let pcd_attributes = self#attributes pcd_attributes in
        let pcd_loc = self#location pcd_loc in
        let pcd_res = self#option self#core_type pcd_res in
        let pcd_args = self#constructor_arguments pcd_args in
        let pcd_name = self#loc self#string pcd_name in
        self#record (Some ("constructor_declaration", 0)) [("pcd_attributes", pcd_attributes); ("pcd_loc", pcd_loc); ("pcd_res", pcd_res); ("pcd_args", pcd_args); ("pcd_name", pcd_name)]
    method label_declaration : Label_declaration.t -> 'res  =
      fun label_declaration ->
        let concrete = Label_declaration.to_concrete label_declaration in
        let { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name } : Label_declaration.concrete = concrete in
        let pld_attributes = self#attributes pld_attributes in
        let pld_loc = self#location pld_loc in
        let pld_type = self#core_type pld_type in
        let pld_mutable = self#mutable_flag pld_mutable in
        let pld_name = self#loc self#string pld_name in
        self#record (Some ("label_declaration", 0)) [("pld_attributes", pld_attributes); ("pld_loc", pld_loc); ("pld_type", pld_type); ("pld_mutable", pld_mutable); ("pld_name", pld_name)]
    method type_kind : Type_kind.t -> 'res  =
      fun type_kind ->
        let concrete = Type_kind.to_concrete type_kind in
        match (concrete : Type_kind.concrete) with
        | Ptype_open ->
          self#constr (Some ("type_kind", 0)) "Ptype_open" []
        | Ptype_record x0 ->
          let x0 = self#list self#label_declaration x0 in
          self#constr (Some ("type_kind", 0)) "Ptype_record" [x0]
        | Ptype_variant x0 ->
          let x0 = self#list self#constructor_declaration x0 in
          self#constr (Some ("type_kind", 0)) "Ptype_variant" [x0]
        | Ptype_abstract ->
          self#constr (Some ("type_kind", 0)) "Ptype_abstract" []
    method type_declaration : Type_declaration.t -> 'res  =
      fun type_declaration ->
        let concrete = Type_declaration.to_concrete type_declaration in
        let { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name } : Type_declaration.concrete = concrete in
        let ptype_loc = self#location ptype_loc in
        let ptype_attributes = self#attributes ptype_attributes in
        let ptype_manifest = self#option self#core_type ptype_manifest in
        let ptype_private = self#private_flag ptype_private in
        let ptype_kind = self#type_kind ptype_kind in
        let ptype_cstrs = self#list (fun (x0, x1, x2) -> let x0 = self#location x0 in let x1 = self#core_type x1 in let x2 = self#core_type x2 in self#node None (self#tuple [x0; x1; x2])) ptype_cstrs in
        let ptype_params = self#list (fun (x0, x1) -> let x0 = self#variance x0 in let x1 = self#core_type x1 in self#node None (self#tuple [x0; x1])) ptype_params in
        let ptype_name = self#loc self#string ptype_name in
        self#record (Some ("type_declaration", 0)) [("ptype_loc", ptype_loc); ("ptype_attributes", ptype_attributes); ("ptype_manifest", ptype_manifest); ("ptype_private", ptype_private); ("ptype_kind", ptype_kind); ("ptype_cstrs", ptype_cstrs); ("ptype_params", ptype_params); ("ptype_name", ptype_name)]
    method value_description : Value_description.t -> 'res  =
      fun value_description ->
        let concrete = Value_description.to_concrete value_description in
        let { pval_loc; pval_attributes; pval_prim; pval_type; pval_name } : Value_description.concrete = concrete in
        let pval_loc = self#location pval_loc in
        let pval_attributes = self#attributes pval_attributes in
        let pval_prim = self#list self#string pval_prim in
        let pval_type = self#core_type pval_type in
        let pval_name = self#loc self#string pval_name in
        self#record (Some ("value_description", 0)) [("pval_loc", pval_loc); ("pval_attributes", pval_attributes); ("pval_prim", pval_prim); ("pval_type", pval_type); ("pval_name", pval_name)]
    method case : Case.t -> 'res  =
      fun case ->
        let concrete = Case.to_concrete case in
        let { pc_rhs; pc_guard; pc_lhs } : Case.concrete = concrete in
        let pc_rhs = self#expression pc_rhs in
        let pc_guard = self#option self#expression pc_guard in
        let pc_lhs = self#pattern pc_lhs in
        self#record (Some ("case", 0)) [("pc_rhs", pc_rhs); ("pc_guard", pc_guard); ("pc_lhs", pc_lhs)]
    method expression_desc : Expression_desc.t -> 'res  =
      fun expression_desc ->
        let concrete = Expression_desc.to_concrete expression_desc in
        match (concrete : Expression_desc.concrete) with
        | Pexp_unreachable ->
          self#constr (Some ("expression_desc", 0)) "Pexp_unreachable" []
        | Pexp_extension x0 ->
          let x0 = self#extension x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_extension" [x0]
        | Pexp_open (x0, x1, x2) ->
          let x0 = self#expression x0 in
          let x1 = self#longident_loc x1 in
          let x2 = self#override_flag x2 in
          self#constr (Some ("expression_desc", 0)) "Pexp_open" [x0; x1; x2]
        | Pexp_pack x0 ->
          let x0 = self#module_expr x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_pack" [x0]
        | Pexp_newtype (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#loc self#string x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_newtype" [x0; x1]
        | Pexp_object x0 ->
          let x0 = self#class_structure x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_object" [x0]
        | Pexp_poly (x0, x1) ->
          let x0 = self#option self#core_type x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_poly" [x0; x1]
        | Pexp_lazy x0 ->
          let x0 = self#expression x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_lazy" [x0]
        | Pexp_assert x0 ->
          let x0 = self#expression x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_assert" [x0]
        | Pexp_letexception (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#extension_constructor x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_letexception" [x0; x1]
        | Pexp_letmodule (x0, x1, x2) ->
          let x0 = self#expression x0 in
          let x1 = self#module_expr x1 in
          let x2 = self#loc self#string x2 in
          self#constr (Some ("expression_desc", 0)) "Pexp_letmodule" [x0; x1; x2]
        | Pexp_override x0 ->
          let x0 = self#list (fun (x0, x1) -> let x0 = self#expression x0 in let x1 = self#loc self#string x1 in self#node None (self#tuple [x0; x1])) x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_override" [x0]
        | Pexp_setinstvar (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#loc self#string x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_setinstvar" [x0; x1]
        | Pexp_new x0 ->
          let x0 = self#longident_loc x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_new" [x0]
        | Pexp_send (x0, x1) ->
          let x0 = self#loc self#string x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_send" [x0; x1]
        | Pexp_coerce (x0, x1, x2) ->
          let x0 = self#core_type x0 in
          let x1 = self#option self#core_type x1 in
          let x2 = self#expression x2 in
          self#constr (Some ("expression_desc", 0)) "Pexp_coerce" [x0; x1; x2]
        | Pexp_constraint (x0, x1) ->
          let x0 = self#core_type x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_constraint" [x0; x1]
        | Pexp_for (x0, x1, x2, x3, x4) ->
          let x0 = self#expression x0 in
          let x1 = self#direction_flag x1 in
          let x2 = self#expression x2 in
          let x3 = self#expression x3 in
          let x4 = self#pattern x4 in
          self#constr (Some ("expression_desc", 0)) "Pexp_for" [x0; x1; x2; x3; x4]
        | Pexp_while (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_while" [x0; x1]
        | Pexp_sequence (x0, x1) ->
          let x0 = self#expression x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_sequence" [x0; x1]
        | Pexp_ifthenelse (x0, x1, x2) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#expression x1 in
          let x2 = self#expression x2 in
          self#constr (Some ("expression_desc", 0)) "Pexp_ifthenelse" [x0; x1; x2]
        | Pexp_array x0 ->
          let x0 = self#list self#expression x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_array" [x0]
        | Pexp_setfield (x0, x1, x2) ->
          let x0 = self#expression x0 in
          let x1 = self#longident_loc x1 in
          let x2 = self#expression x2 in
          self#constr (Some ("expression_desc", 0)) "Pexp_setfield" [x0; x1; x2]
        | Pexp_field (x0, x1) ->
          let x0 = self#longident_loc x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_field" [x0; x1]
        | Pexp_record (x0, x1) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#list (fun (x0, x1) -> let x0 = self#expression x0 in let x1 = self#longident_loc x1 in self#node None (self#tuple [x0; x1])) x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_record" [x0; x1]
        | Pexp_variant (x0, x1) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#string x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_variant" [x0; x1]
        | Pexp_construct (x0, x1) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_construct" [x0; x1]
        | Pexp_tuple x0 ->
          let x0 = self#list self#expression x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_tuple" [x0]
        | Pexp_try (x0, x1) ->
          let x0 = self#list self#case x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_try" [x0; x1]
        | Pexp_match (x0, x1) ->
          let x0 = self#list self#case x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_match" [x0; x1]
        | Pexp_apply (x0, x1) ->
          let x0 = self#list (fun (x0, x1) -> let x0 = self#expression x0 in let x1 = self#arg_label x1 in self#node None (self#tuple [x0; x1])) x0 in
          let x1 = self#expression x1 in
          self#constr (Some ("expression_desc", 0)) "Pexp_apply" [x0; x1]
        | Pexp_fun (x0, x1, x2, x3) ->
          let x0 = self#expression x0 in
          let x1 = self#pattern x1 in
          let x2 = self#option self#expression x2 in
          let x3 = self#arg_label x3 in
          self#constr (Some ("expression_desc", 0)) "Pexp_fun" [x0; x1; x2; x3]
        | Pexp_function x0 ->
          let x0 = self#list self#case x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_function" [x0]
        | Pexp_let (x0, x1, x2) ->
          let x0 = self#expression x0 in
          let x1 = self#list self#value_binding x1 in
          let x2 = self#rec_flag x2 in
          self#constr (Some ("expression_desc", 0)) "Pexp_let" [x0; x1; x2]
        | Pexp_constant x0 ->
          let x0 = self#constant x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_constant" [x0]
        | Pexp_ident x0 ->
          let x0 = self#longident_loc x0 in
          self#constr (Some ("expression_desc", 0)) "Pexp_ident" [x0]
    method expression : Expression.t -> 'res  =
      fun expression ->
        let concrete = Expression.to_concrete expression in
        let { pexp_attributes; pexp_loc; pexp_desc } : Expression.concrete = concrete in
        let pexp_attributes = self#attributes pexp_attributes in
        let pexp_loc = self#location pexp_loc in
        let pexp_desc = self#expression_desc pexp_desc in
        self#record (Some ("expression", 0)) [("pexp_attributes", pexp_attributes); ("pexp_loc", pexp_loc); ("pexp_desc", pexp_desc)]
    method pattern_desc : Pattern_desc.t -> 'res  =
      fun pattern_desc ->
        let concrete = Pattern_desc.to_concrete pattern_desc in
        match (concrete : Pattern_desc.concrete) with
        | Ppat_open (x0, x1) ->
          let x0 = self#pattern x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_open" [x0; x1]
        | Ppat_extension x0 ->
          let x0 = self#extension x0 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_extension" [x0]
        | Ppat_exception x0 ->
          let x0 = self#pattern x0 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_exception" [x0]
        | Ppat_unpack x0 ->
          let x0 = self#loc self#string x0 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_unpack" [x0]
        | Ppat_lazy x0 ->
          let x0 = self#pattern x0 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_lazy" [x0]
        | Ppat_type x0 ->
          let x0 = self#longident_loc x0 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_type" [x0]
        | Ppat_constraint (x0, x1) ->
          let x0 = self#core_type x0 in
          let x1 = self#pattern x1 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_constraint" [x0; x1]
        | Ppat_or (x0, x1) ->
          let x0 = self#pattern x0 in
          let x1 = self#pattern x1 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_or" [x0; x1]
        | Ppat_array x0 ->
          let x0 = self#list self#pattern x0 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_array" [x0]
        | Ppat_record (x0, x1) ->
          let x0 = self#closed_flag x0 in
          let x1 = self#list (fun (x0, x1) -> let x0 = self#pattern x0 in let x1 = self#longident_loc x1 in self#node None (self#tuple [x0; x1])) x1 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_record" [x0; x1]
        | Ppat_variant (x0, x1) ->
          let x0 = self#option self#pattern x0 in
          let x1 = self#string x1 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_variant" [x0; x1]
        | Ppat_construct (x0, x1) ->
          let x0 = self#option self#pattern x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_construct" [x0; x1]
        | Ppat_tuple x0 ->
          let x0 = self#list self#pattern x0 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_tuple" [x0]
        | Ppat_interval (x0, x1) ->
          let x0 = self#constant x0 in
          let x1 = self#constant x1 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_interval" [x0; x1]
        | Ppat_constant x0 ->
          let x0 = self#constant x0 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_constant" [x0]
        | Ppat_alias (x0, x1) ->
          let x0 = self#loc self#string x0 in
          let x1 = self#pattern x1 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_alias" [x0; x1]
        | Ppat_var x0 ->
          let x0 = self#loc self#string x0 in
          self#constr (Some ("pattern_desc", 0)) "Ppat_var" [x0]
        | Ppat_any ->
          self#constr (Some ("pattern_desc", 0)) "Ppat_any" []
    method pattern : Pattern.t -> 'res  =
      fun pattern ->
        let concrete = Pattern.to_concrete pattern in
        let { ppat_attributes; ppat_loc; ppat_desc } : Pattern.concrete = concrete in
        let ppat_attributes = self#attributes ppat_attributes in
        let ppat_loc = self#location ppat_loc in
        let ppat_desc = self#pattern_desc ppat_desc in
        self#record (Some ("pattern", 0)) [("ppat_attributes", ppat_attributes); ("ppat_loc", ppat_loc); ("ppat_desc", ppat_desc)]
    method object_field : Object_field.t -> 'res  =
      fun object_field ->
        let concrete = Object_field.to_concrete object_field in
        match (concrete : Object_field.concrete) with
        | Oinherit x0 ->
          let x0 = self#core_type x0 in
          self#constr (Some ("object_field", 0)) "Oinherit" [x0]
        | Otag (x0, x1, x2) ->
          let x0 = self#core_type x0 in
          let x1 = self#attributes x1 in
          let x2 = self#loc self#string x2 in
          self#constr (Some ("object_field", 0)) "Otag" [x0; x1; x2]
    method row_field : Row_field.t -> 'res  =
      fun row_field ->
        let concrete = Row_field.to_concrete row_field in
        match (concrete : Row_field.concrete) with
        | Rinherit x0 ->
          let x0 = self#core_type x0 in
          self#constr (Some ("row_field", 0)) "Rinherit" [x0]
        | Rtag (x0, x1, x2, x3) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#bool x1 in
          let x2 = self#attributes x2 in
          let x3 = self#loc self#string x3 in
          self#constr (Some ("row_field", 0)) "Rtag" [x0; x1; x2; x3]
    method package_type : Package_type.t -> 'res  =
      fun package_type ->
        let concrete = Package_type.to_concrete package_type in
        let (x0, x1) = concrete in
        let x0 = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#longident_loc x1 in self#node None (self#tuple [x0; x1])) x0 in
        let x1 = self#longident_loc x1 in
        self#node (Some ("package_type", 0)) (self#tuple [x0; x1])
    method core_type_desc : Core_type_desc.t -> 'res  =
      fun core_type_desc ->
        let concrete = Core_type_desc.to_concrete core_type_desc in
        match (concrete : Core_type_desc.concrete) with
        | Ptyp_extension x0 ->
          let x0 = self#extension x0 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_extension" [x0]
        | Ptyp_package x0 ->
          let x0 = self#package_type x0 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_package" [x0]
        | Ptyp_poly (x0, x1) ->
          let x0 = self#core_type x0 in
          let x1 = self#list (self#loc self#string) x1 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_poly" [x0; x1]
        | Ptyp_variant (x0, x1, x2) ->
          let x0 = self#option (self#list self#string) x0 in
          let x1 = self#closed_flag x1 in
          let x2 = self#list self#row_field x2 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_variant" [x0; x1; x2]
        | Ptyp_alias (x0, x1) ->
          let x0 = self#string x0 in
          let x1 = self#core_type x1 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_alias" [x0; x1]
        | Ptyp_class (x0, x1) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_class" [x0; x1]
        | Ptyp_object (x0, x1) ->
          let x0 = self#closed_flag x0 in
          let x1 = self#list self#object_field x1 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_object" [x0; x1]
        | Ptyp_constr (x0, x1) ->
          let x0 = self#list self#core_type x0 in
          let x1 = self#longident_loc x1 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_constr" [x0; x1]
        | Ptyp_tuple x0 ->
          let x0 = self#list self#core_type x0 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_tuple" [x0]
        | Ptyp_arrow (x0, x1, x2) ->
          let x0 = self#core_type x0 in
          let x1 = self#core_type x1 in
          let x2 = self#arg_label x2 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_arrow" [x0; x1; x2]
        | Ptyp_var x0 ->
          let x0 = self#string x0 in
          self#constr (Some ("core_type_desc", 0)) "Ptyp_var" [x0]
        | Ptyp_any ->
          self#constr (Some ("core_type_desc", 0)) "Ptyp_any" []
    method core_type : Core_type.t -> 'res  =
      fun core_type ->
        let concrete = Core_type.to_concrete core_type in
        let { ptyp_attributes; ptyp_loc; ptyp_desc } : Core_type.concrete = concrete in
        let ptyp_attributes = self#attributes ptyp_attributes in
        let ptyp_loc = self#location ptyp_loc in
        let ptyp_desc = self#core_type_desc ptyp_desc in
        self#record (Some ("core_type", 0)) [("ptyp_attributes", ptyp_attributes); ("ptyp_loc", ptyp_loc); ("ptyp_desc", ptyp_desc)]
    method payload : Payload.t -> 'res  =
      fun payload ->
        let concrete = Payload.to_concrete payload in
        match (concrete : Payload.concrete) with
        | PPat (x0, x1) ->
          let x0 = self#option self#expression x0 in
          let x1 = self#pattern x1 in
          self#constr (Some ("payload", 0)) "PPat" [x0; x1]
        | PTyp x0 ->
          let x0 = self#core_type x0 in
          self#constr (Some ("payload", 0)) "PTyp" [x0]
        | PSig x0 ->
          let x0 = self#signature x0 in
          self#constr (Some ("payload", 0)) "PSig" [x0]
        | PStr x0 ->
          let x0 = self#structure x0 in
          self#constr (Some ("payload", 0)) "PStr" [x0]
    method attributes : Attributes.t -> 'res  =
      fun attributes ->
        let concrete = Attributes.to_concrete attributes in
        let concrete = self#list self#attribute concrete in
        self#node (Some ("attributes", 0)) concrete
    method extension : Extension.t -> 'res  =
      fun extension ->
        let concrete = Extension.to_concrete extension in
        let (x0, x1) = concrete in
        let x0 = self#payload x0 in
        let x1 = self#loc self#string x1 in
        self#node (Some ("extension", 0)) (self#tuple [x0; x1])
    method attribute : Attribute.t -> 'res  =
      fun attribute ->
        let concrete = Attribute.to_concrete attribute in
        let (x0, x1) = concrete in
        let x0 = self#payload x0 in
        let x1 = self#loc self#string x1 in
        self#node (Some ("attribute", 0)) (self#tuple [x0; x1])
    method constant : Constant.t -> 'res  =
      fun constant ->
        let concrete = Constant.to_concrete constant in
        match (concrete : Constant.concrete) with
        | Pconst_float (x0, x1) ->
          let x0 = self#option self#char x0 in
          let x1 = self#string x1 in
          self#constr (Some ("constant", 0)) "Pconst_float" [x0; x1]
        | Pconst_string (x0, x1) ->
          let x0 = self#option self#string x0 in
          let x1 = self#string x1 in
          self#constr (Some ("constant", 0)) "Pconst_string" [x0; x1]
        | Pconst_char x0 ->
          let x0 = self#char x0 in
          self#constr (Some ("constant", 0)) "Pconst_char" [x0]
        | Pconst_integer (x0, x1) ->
          let x0 = self#option self#char x0 in
          let x1 = self#string x1 in
          self#constr (Some ("constant", 0)) "Pconst_integer" [x0; x1]
    method variance : Variance.t -> 'res  =
      fun variance ->
        let concrete = Variance.to_concrete variance in
        match (concrete : Variance.concrete) with
        | Invariant ->
          self#constr (Some ("variance", 0)) "Invariant" []
        | Contravariant ->
          self#constr (Some ("variance", 0)) "Contravariant" []
        | Covariant ->
          self#constr (Some ("variance", 0)) "Covariant" []
    method arg_label : Arg_label.t -> 'res  =
      fun arg_label ->
        let concrete = Arg_label.to_concrete arg_label in
        match (concrete : Arg_label.concrete) with
        | Optional x0 ->
          let x0 = self#string x0 in
          self#constr (Some ("arg_label", 0)) "Optional" [x0]
        | Labelled x0 ->
          let x0 = self#string x0 in
          self#constr (Some ("arg_label", 0)) "Labelled" [x0]
        | Nolabel ->
          self#constr (Some ("arg_label", 0)) "Nolabel" []
    method closed_flag : Closed_flag.t -> 'res  =
      fun closed_flag ->
        let concrete = Closed_flag.to_concrete closed_flag in
        match (concrete : Closed_flag.concrete) with
        | Open ->
          self#constr (Some ("closed_flag", 0)) "Open" []
        | Closed ->
          self#constr (Some ("closed_flag", 0)) "Closed" []
    method override_flag : Override_flag.t -> 'res  =
      fun override_flag ->
        let concrete = Override_flag.to_concrete override_flag in
        match (concrete : Override_flag.concrete) with
        | Fresh ->
          self#constr (Some ("override_flag", 0)) "Fresh" []
        | Override ->
          self#constr (Some ("override_flag", 0)) "Override" []
    method virtual_flag : Virtual_flag.t -> 'res  =
      fun virtual_flag ->
        let concrete = Virtual_flag.to_concrete virtual_flag in
        match (concrete : Virtual_flag.concrete) with
        | Concrete ->
          self#constr (Some ("virtual_flag", 0)) "Concrete" []
        | Virtual ->
          self#constr (Some ("virtual_flag", 0)) "Virtual" []
    method mutable_flag : Mutable_flag.t -> 'res  =
      fun mutable_flag ->
        let concrete = Mutable_flag.to_concrete mutable_flag in
        match (concrete : Mutable_flag.concrete) with
        | Mutable ->
          self#constr (Some ("mutable_flag", 0)) "Mutable" []
        | Immutable ->
          self#constr (Some ("mutable_flag", 0)) "Immutable" []
    method private_flag : Private_flag.t -> 'res  =
      fun private_flag ->
        let concrete = Private_flag.to_concrete private_flag in
        match (concrete : Private_flag.concrete) with
        | Public ->
          self#constr (Some ("private_flag", 0)) "Public" []
        | Private ->
          self#constr (Some ("private_flag", 0)) "Private" []
    method direction_flag : Direction_flag.t -> 'res  =
      fun direction_flag ->
        let concrete = Direction_flag.to_concrete direction_flag in
        match (concrete : Direction_flag.concrete) with
        | Downto ->
          self#constr (Some ("direction_flag", 0)) "Downto" []
        | Upto ->
          self#constr (Some ("direction_flag", 0)) "Upto" []
    method rec_flag : Rec_flag.t -> 'res  =
      fun rec_flag ->
        let concrete = Rec_flag.to_concrete rec_flag in
        match (concrete : Rec_flag.concrete) with
        | Recursive ->
          self#constr (Some ("rec_flag", 0)) "Recursive" []
        | Nonrecursive ->
          self#constr (Some ("rec_flag", 0)) "Nonrecursive" []
    method longident_loc : Longident_loc.t -> 'res  =
      fun longident_loc ->
        let concrete = Longident_loc.to_concrete longident_loc in
        let concrete = self#loc self#longident concrete in
        self#node (Some ("longident_loc", 0)) concrete
    method longident : Longident.t -> 'res  =
      fun longident ->
        let concrete = Longident.to_concrete longident in
        match (concrete : Longident.concrete) with
        | Lapply (x0, x1) ->
          let x0 = self#longident x0 in
          let x1 = self#longident x1 in
          self#constr (Some ("longident", 0)) "Lapply" [x0; x1]
        | Ldot (x0, x1) ->
          let x0 = self#string x0 in
          let x1 = self#longident x1 in
          self#constr (Some ("longident", 0)) "Ldot" [x0; x1]
        | Lident x0 ->
          let x0 = self#string x0 in
          self#constr (Some ("longident", 0)) "Lident" [x0]
  end
(*$*)
