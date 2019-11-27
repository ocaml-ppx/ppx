(*$ Ppx_ast_cinaps.print_virtual_traverse_ml () *)
module V4_07 = struct
  open Versions.V4_07

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
      method longident : Longident.t -> Longident.t  =
        fun longident ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let x0 = self#string x0 in
            Longident.of_concrete (Lident x0)
          | Ldot (x0, x1) ->
            let x0 = self#longident x0 in
            let x1 = self#string x1 in
            Longident.of_concrete (Ldot (x0, x1))
          | Lapply (x0, x1) ->
            let x0 = self#longident x0 in
            let x1 = self#longident x1 in
            Longident.of_concrete (Lapply (x0, x1))
      method longident_loc : Longident_loc.t -> Longident_loc.t  =
        fun longident_loc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let concrete = self#loc self#longident concrete in
          Longident_loc.of_concrete concrete
      method rec_flag : Rec_flag.t -> Rec_flag.t  =
        fun rec_flag ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            Rec_flag.of_concrete Nonrecursive
          | Recursive ->
            Rec_flag.of_concrete Recursive
      method direction_flag : Direction_flag.t -> Direction_flag.t  =
        fun direction_flag ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            Direction_flag.of_concrete Upto
          | Downto ->
            Direction_flag.of_concrete Downto
      method private_flag : Private_flag.t -> Private_flag.t  =
        fun private_flag ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            Private_flag.of_concrete Private
          | Public ->
            Private_flag.of_concrete Public
      method mutable_flag : Mutable_flag.t -> Mutable_flag.t  =
        fun mutable_flag ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            Mutable_flag.of_concrete Immutable
          | Mutable ->
            Mutable_flag.of_concrete Mutable
      method virtual_flag : Virtual_flag.t -> Virtual_flag.t  =
        fun virtual_flag ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            Virtual_flag.of_concrete Virtual
          | Concrete ->
            Virtual_flag.of_concrete Concrete
      method override_flag : Override_flag.t -> Override_flag.t  =
        fun override_flag ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            Override_flag.of_concrete Override
          | Fresh ->
            Override_flag.of_concrete Fresh
      method closed_flag : Closed_flag.t -> Closed_flag.t  =
        fun closed_flag ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            Closed_flag.of_concrete Closed
          | Open ->
            Closed_flag.of_concrete Open
      method label : Label.t -> Label.t  =
        fun label ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let concrete = self#string concrete in
          Label.of_concrete concrete
      method arg_label : Arg_label.t -> Arg_label.t  =
        fun arg_label ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            Arg_label.of_concrete Nolabel
          | Labelled x0 ->
            let x0 = self#string x0 in
            Arg_label.of_concrete (Labelled x0)
          | Optional x0 ->
            let x0 = self#string x0 in
            Arg_label.of_concrete (Optional x0)
      method variance : Variance.t -> Variance.t  =
        fun variance ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            Variance.of_concrete Covariant
          | Contravariant ->
            Variance.of_concrete Contravariant
          | Invariant ->
            Variance.of_concrete Invariant
      method constant : Constant.t -> Constant.t  =
        fun constant ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            Constant.of_concrete (Pconst_integer (x0, x1))
          | Pconst_char x0 ->
            let x0 = self#char x0 in
            Constant.of_concrete (Pconst_char x0)
          | Pconst_string (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#string x1 in
            Constant.of_concrete (Pconst_string (x0, x1))
          | Pconst_float (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            Constant.of_concrete (Pconst_float (x0, x1))
      method attribute : Attribute.t -> Attribute.t  =
        fun attribute ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string x0 in
          let x1 = self#payload x1 in
          Attribute.of_concrete (x0, x1)
      method extension : Extension.t -> Extension.t  =
        fun extension ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string x0 in
          let x1 = self#payload x1 in
          Extension.of_concrete (x0, x1)
      method attributes : Attributes.t -> Attributes.t  =
        fun attributes ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let concrete = self#list self#attribute concrete in
          Attributes.of_concrete concrete
      method payload : Payload.t -> Payload.t  =
        fun payload ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let x0 = self#structure x0 in
            Payload.of_concrete (PStr x0)
          | PSig x0 ->
            let x0 = self#signature x0 in
            Payload.of_concrete (PSig x0)
          | PTyp x0 ->
            let x0 = self#core_type x0 in
            Payload.of_concrete (PTyp x0)
          | PPat (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#option self#expression x1 in
            Payload.of_concrete (PPat (x0, x1))
      method core_type : Core_type.t -> Core_type.t  =
        fun core_type ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let ptyp_desc = self#core_type_desc ptyp_desc in
          let ptyp_loc = self#location ptyp_loc in
          let ptyp_attributes = self#attributes ptyp_attributes in
          Core_type.of_concrete { ptyp_desc; ptyp_loc; ptyp_attributes }
      method core_type_desc : Core_type_desc.t -> Core_type_desc.t  =
        fun core_type_desc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            Core_type_desc.of_concrete Ptyp_any
          | Ptyp_var x0 ->
            let x0 = self#string x0 in
            Core_type_desc.of_concrete (Ptyp_var x0)
          | Ptyp_arrow (x0, x1, x2) ->
            let x0 = self#arg_label x0 in
            let x1 = self#core_type x1 in
            let x2 = self#core_type x2 in
            Core_type_desc.of_concrete (Ptyp_arrow (x0, x1, x2))
          | Ptyp_tuple x0 ->
            let x0 = self#list self#core_type x0 in
            Core_type_desc.of_concrete (Ptyp_tuple x0)
          | Ptyp_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            Core_type_desc.of_concrete (Ptyp_constr (x0, x1))
          | Ptyp_object (x0, x1) ->
            let x0 = self#list self#object_field x0 in
            let x1 = self#closed_flag x1 in
            Core_type_desc.of_concrete (Ptyp_object (x0, x1))
          | Ptyp_class (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            Core_type_desc.of_concrete (Ptyp_class (x0, x1))
          | Ptyp_alias (x0, x1) ->
            let x0 = self#core_type x0 in
            let x1 = self#string x1 in
            Core_type_desc.of_concrete (Ptyp_alias (x0, x1))
          | Ptyp_variant (x0, x1, x2) ->
            let x0 = self#list self#row_field x0 in
            let x1 = self#closed_flag x1 in
            let x2 = self#option (self#list self#label) x2 in
            Core_type_desc.of_concrete (Ptyp_variant (x0, x1, x2))
          | Ptyp_poly (x0, x1) ->
            let x0 = self#list (self#loc self#string) x0 in
            let x1 = self#core_type x1 in
            Core_type_desc.of_concrete (Ptyp_poly (x0, x1))
          | Ptyp_package x0 ->
            let x0 = self#package_type x0 in
            Core_type_desc.of_concrete (Ptyp_package x0)
          | Ptyp_extension x0 ->
            let x0 = self#extension x0 in
            Core_type_desc.of_concrete (Ptyp_extension x0)
      method package_type : Package_type.t -> Package_type.t  =
        fun package_type ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#longident_loc x0 in
          let x1 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#core_type x1 in (x0, x1)) x1 in
          Package_type.of_concrete (x0, x1)
      method row_field : Row_field.t -> Row_field.t  =
        fun row_field ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#attributes x1 in
            let x2 = self#bool x2 in
            let x3 = self#list self#core_type x3 in
            Row_field.of_concrete (Rtag (x0, x1, x2, x3))
          | Rinherit x0 ->
            let x0 = self#core_type x0 in
            Row_field.of_concrete (Rinherit x0)
      method object_field : Object_field.t -> Object_field.t  =
        fun object_field ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#attributes x1 in
            let x2 = self#core_type x2 in
            Object_field.of_concrete (Otag (x0, x1, x2))
          | Oinherit x0 ->
            let x0 = self#core_type x0 in
            Object_field.of_concrete (Oinherit x0)
      method pattern : Pattern.t -> Pattern.t  =
        fun pattern ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let ppat_desc = self#pattern_desc ppat_desc in
          let ppat_loc = self#location ppat_loc in
          let ppat_attributes = self#attributes ppat_attributes in
          Pattern.of_concrete { ppat_desc; ppat_loc; ppat_attributes }
      method pattern_desc : Pattern_desc.t -> Pattern_desc.t  =
        fun pattern_desc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            Pattern_desc.of_concrete Ppat_any
          | Ppat_var x0 ->
            let x0 = self#loc self#string x0 in
            Pattern_desc.of_concrete (Ppat_var x0)
          | Ppat_alias (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#loc self#string x1 in
            Pattern_desc.of_concrete (Ppat_alias (x0, x1))
          | Ppat_constant x0 ->
            let x0 = self#constant x0 in
            Pattern_desc.of_concrete (Ppat_constant x0)
          | Ppat_interval (x0, x1) ->
            let x0 = self#constant x0 in
            let x1 = self#constant x1 in
            Pattern_desc.of_concrete (Ppat_interval (x0, x1))
          | Ppat_tuple x0 ->
            let x0 = self#list self#pattern x0 in
            Pattern_desc.of_concrete (Ppat_tuple x0)
          | Ppat_construct (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#option self#pattern x1 in
            Pattern_desc.of_concrete (Ppat_construct (x0, x1))
          | Ppat_variant (x0, x1) ->
            let x0 = self#label x0 in
            let x1 = self#option self#pattern x1 in
            Pattern_desc.of_concrete (Ppat_variant (x0, x1))
          | Ppat_record (x0, x1) ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#pattern x1 in (x0, x1)) x0 in
            let x1 = self#closed_flag x1 in
            Pattern_desc.of_concrete (Ppat_record (x0, x1))
          | Ppat_array x0 ->
            let x0 = self#list self#pattern x0 in
            Pattern_desc.of_concrete (Ppat_array x0)
          | Ppat_or (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#pattern x1 in
            Pattern_desc.of_concrete (Ppat_or (x0, x1))
          | Ppat_constraint (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#core_type x1 in
            Pattern_desc.of_concrete (Ppat_constraint (x0, x1))
          | Ppat_type x0 ->
            let x0 = self#longident_loc x0 in
            Pattern_desc.of_concrete (Ppat_type x0)
          | Ppat_lazy x0 ->
            let x0 = self#pattern x0 in
            Pattern_desc.of_concrete (Ppat_lazy x0)
          | Ppat_unpack x0 ->
            let x0 = self#loc self#string x0 in
            Pattern_desc.of_concrete (Ppat_unpack x0)
          | Ppat_exception x0 ->
            let x0 = self#pattern x0 in
            Pattern_desc.of_concrete (Ppat_exception x0)
          | Ppat_extension x0 ->
            let x0 = self#extension x0 in
            Pattern_desc.of_concrete (Ppat_extension x0)
          | Ppat_open (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#pattern x1 in
            Pattern_desc.of_concrete (Ppat_open (x0, x1))
      method expression : Expression.t -> Expression.t  =
        fun expression ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let pexp_desc = self#expression_desc pexp_desc in
          let pexp_loc = self#location pexp_loc in
          let pexp_attributes = self#attributes pexp_attributes in
          Expression.of_concrete { pexp_desc; pexp_loc; pexp_attributes }
      method expression_desc : Expression_desc.t -> Expression_desc.t  =
        fun expression_desc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let x0 = self#longident_loc x0 in
            Expression_desc.of_concrete (Pexp_ident x0)
          | Pexp_constant x0 ->
            let x0 = self#constant x0 in
            Expression_desc.of_concrete (Pexp_constant x0)
          | Pexp_let (x0, x1, x2) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            let x2 = self#expression x2 in
            Expression_desc.of_concrete (Pexp_let (x0, x1, x2))
          | Pexp_function x0 ->
            let x0 = self#list self#case x0 in
            Expression_desc.of_concrete (Pexp_function x0)
          | Pexp_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label x0 in
            let x1 = self#option self#expression x1 in
            let x2 = self#pattern x2 in
            let x3 = self#expression x3 in
            Expression_desc.of_concrete (Pexp_fun (x0, x1, x2, x3))
          | Pexp_apply (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list (fun (x0, x1) -> let x0 = self#arg_label x0 in let x1 = self#expression x1 in (x0, x1)) x1 in
            Expression_desc.of_concrete (Pexp_apply (x0, x1))
          | Pexp_match (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list self#case x1 in
            Expression_desc.of_concrete (Pexp_match (x0, x1))
          | Pexp_try (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list self#case x1 in
            Expression_desc.of_concrete (Pexp_try (x0, x1))
          | Pexp_tuple x0 ->
            let x0 = self#list self#expression x0 in
            Expression_desc.of_concrete (Pexp_tuple x0)
          | Pexp_construct (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#option self#expression x1 in
            Expression_desc.of_concrete (Pexp_construct (x0, x1))
          | Pexp_variant (x0, x1) ->
            let x0 = self#label x0 in
            let x1 = self#option self#expression x1 in
            Expression_desc.of_concrete (Pexp_variant (x0, x1))
          | Pexp_record (x0, x1) ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#expression x1 in (x0, x1)) x0 in
            let x1 = self#option self#expression x1 in
            Expression_desc.of_concrete (Pexp_record (x0, x1))
          | Pexp_field (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#longident_loc x1 in
            Expression_desc.of_concrete (Pexp_field (x0, x1))
          | Pexp_setfield (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#expression x2 in
            Expression_desc.of_concrete (Pexp_setfield (x0, x1, x2))
          | Pexp_array x0 ->
            let x0 = self#list self#expression x0 in
            Expression_desc.of_concrete (Pexp_array x0)
          | Pexp_ifthenelse (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            let x2 = self#option self#expression x2 in
            Expression_desc.of_concrete (Pexp_ifthenelse (x0, x1, x2))
          | Pexp_sequence (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_sequence (x0, x1))
          | Pexp_while (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_while (x0, x1))
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let x0 = self#pattern x0 in
            let x1 = self#expression x1 in
            let x2 = self#expression x2 in
            let x3 = self#direction_flag x3 in
            let x4 = self#expression x4 in
            Expression_desc.of_concrete (Pexp_for (x0, x1, x2, x3, x4))
          | Pexp_constraint (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#core_type x1 in
            Expression_desc.of_concrete (Pexp_constraint (x0, x1))
          | Pexp_coerce (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#option self#core_type x1 in
            let x2 = self#core_type x2 in
            Expression_desc.of_concrete (Pexp_coerce (x0, x1, x2))
          | Pexp_send (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#loc self#label x1 in
            Expression_desc.of_concrete (Pexp_send (x0, x1))
          | Pexp_new x0 ->
            let x0 = self#longident_loc x0 in
            Expression_desc.of_concrete (Pexp_new x0)
          | Pexp_setinstvar (x0, x1) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_setinstvar (x0, x1))
          | Pexp_override x0 ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#loc self#label x0 in let x1 = self#expression x1 in (x0, x1)) x0 in
            Expression_desc.of_concrete (Pexp_override x0)
          | Pexp_letmodule (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#module_expr x1 in
            let x2 = self#expression x2 in
            Expression_desc.of_concrete (Pexp_letmodule (x0, x1, x2))
          | Pexp_letexception (x0, x1) ->
            let x0 = self#extension_constructor x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_letexception (x0, x1))
          | Pexp_assert x0 ->
            let x0 = self#expression x0 in
            Expression_desc.of_concrete (Pexp_assert x0)
          | Pexp_lazy x0 ->
            let x0 = self#expression x0 in
            Expression_desc.of_concrete (Pexp_lazy x0)
          | Pexp_poly (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#option self#core_type x1 in
            Expression_desc.of_concrete (Pexp_poly (x0, x1))
          | Pexp_object x0 ->
            let x0 = self#class_structure x0 in
            Expression_desc.of_concrete (Pexp_object x0)
          | Pexp_newtype (x0, x1) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_newtype (x0, x1))
          | Pexp_pack x0 ->
            let x0 = self#module_expr x0 in
            Expression_desc.of_concrete (Pexp_pack x0)
          | Pexp_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#expression x2 in
            Expression_desc.of_concrete (Pexp_open (x0, x1, x2))
          | Pexp_extension x0 ->
            let x0 = self#extension x0 in
            Expression_desc.of_concrete (Pexp_extension x0)
          | Pexp_unreachable ->
            Expression_desc.of_concrete Pexp_unreachable
      method case : Case.t -> Case.t  =
        fun case ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let pc_lhs = self#pattern pc_lhs in
          let pc_guard = self#option self#expression pc_guard in
          let pc_rhs = self#expression pc_rhs in
          Case.of_concrete { pc_lhs; pc_guard; pc_rhs }
      method value_description : Value_description.t -> Value_description.t  =
        fun value_description ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let pval_name = self#loc self#string pval_name in
          let pval_type = self#core_type pval_type in
          let pval_prim = self#list self#string pval_prim in
          let pval_attributes = self#attributes pval_attributes in
          let pval_loc = self#location pval_loc in
          Value_description.of_concrete { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
      method type_declaration : Type_declaration.t -> Type_declaration.t  =
        fun type_declaration ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let ptype_name = self#loc self#string ptype_name in
          let ptype_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in (x0, x1)) ptype_params in
          let ptype_cstrs = self#list (fun (x0, x1, x2) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in let x2 = self#location x2 in (x0, x1, x2)) ptype_cstrs in
          let ptype_kind = self#type_kind ptype_kind in
          let ptype_private = self#private_flag ptype_private in
          let ptype_manifest = self#option self#core_type ptype_manifest in
          let ptype_attributes = self#attributes ptype_attributes in
          let ptype_loc = self#location ptype_loc in
          Type_declaration.of_concrete { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }
      method type_kind : Type_kind.t -> Type_kind.t  =
        fun type_kind ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            Type_kind.of_concrete Ptype_abstract
          | Ptype_variant x0 ->
            let x0 = self#list self#constructor_declaration x0 in
            Type_kind.of_concrete (Ptype_variant x0)
          | Ptype_record x0 ->
            let x0 = self#list self#label_declaration x0 in
            Type_kind.of_concrete (Ptype_record x0)
          | Ptype_open ->
            Type_kind.of_concrete Ptype_open
      method label_declaration : Label_declaration.t -> Label_declaration.t  =
        fun label_declaration ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let pld_name = self#loc self#string pld_name in
          let pld_mutable = self#mutable_flag pld_mutable in
          let pld_type = self#core_type pld_type in
          let pld_loc = self#location pld_loc in
          let pld_attributes = self#attributes pld_attributes in
          Label_declaration.of_concrete { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
      method constructor_declaration : Constructor_declaration.t -> Constructor_declaration.t  =
        fun constructor_declaration ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let pcd_name = self#loc self#string pcd_name in
          let pcd_args = self#constructor_arguments pcd_args in
          let pcd_res = self#option self#core_type pcd_res in
          let pcd_loc = self#location pcd_loc in
          let pcd_attributes = self#attributes pcd_attributes in
          Constructor_declaration.of_concrete { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
      method constructor_arguments : Constructor_arguments.t -> Constructor_arguments.t  =
        fun constructor_arguments ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let x0 = self#list self#core_type x0 in
            Constructor_arguments.of_concrete (Pcstr_tuple x0)
          | Pcstr_record x0 ->
            let x0 = self#list self#label_declaration x0 in
            Constructor_arguments.of_concrete (Pcstr_record x0)
      method type_extension : Type_extension.t -> Type_extension.t  =
        fun type_extension ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let ptyext_path = self#longident_loc ptyext_path in
          let ptyext_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in (x0, x1)) ptyext_params in
          let ptyext_constructors = self#list self#extension_constructor ptyext_constructors in
          let ptyext_private = self#private_flag ptyext_private in
          let ptyext_attributes = self#attributes ptyext_attributes in
          Type_extension.of_concrete { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }
      method extension_constructor : Extension_constructor.t -> Extension_constructor.t  =
        fun extension_constructor ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let pext_name = self#loc self#string pext_name in
          let pext_kind = self#extension_constructor_kind pext_kind in
          let pext_loc = self#location pext_loc in
          let pext_attributes = self#attributes pext_attributes in
          Extension_constructor.of_concrete { pext_name; pext_kind; pext_loc; pext_attributes }
      method extension_constructor_kind : Extension_constructor_kind.t -> Extension_constructor_kind.t  =
        fun extension_constructor_kind ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let x0 = self#constructor_arguments x0 in
            let x1 = self#option self#core_type x1 in
            Extension_constructor_kind.of_concrete (Pext_decl (x0, x1))
          | Pext_rebind x0 ->
            let x0 = self#longident_loc x0 in
            Extension_constructor_kind.of_concrete (Pext_rebind x0)
      method class_type : Class_type.t -> Class_type.t  =
        fun class_type ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let pcty_desc = self#class_type_desc pcty_desc in
          let pcty_loc = self#location pcty_loc in
          let pcty_attributes = self#attributes pcty_attributes in
          Class_type.of_concrete { pcty_desc; pcty_loc; pcty_attributes }
      method class_type_desc : Class_type_desc.t -> Class_type_desc.t  =
        fun class_type_desc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            Class_type_desc.of_concrete (Pcty_constr (x0, x1))
          | Pcty_signature x0 ->
            let x0 = self#class_signature x0 in
            Class_type_desc.of_concrete (Pcty_signature x0)
          | Pcty_arrow (x0, x1, x2) ->
            let x0 = self#arg_label x0 in
            let x1 = self#core_type x1 in
            let x2 = self#class_type x2 in
            Class_type_desc.of_concrete (Pcty_arrow (x0, x1, x2))
          | Pcty_extension x0 ->
            let x0 = self#extension x0 in
            Class_type_desc.of_concrete (Pcty_extension x0)
          | Pcty_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#class_type x2 in
            Class_type_desc.of_concrete (Pcty_open (x0, x1, x2))
      method class_signature : Class_signature.t -> Class_signature.t  =
        fun class_signature ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let pcsig_self = self#core_type pcsig_self in
          let pcsig_fields = self#list self#class_type_field pcsig_fields in
          Class_signature.of_concrete { pcsig_self; pcsig_fields }
      method class_type_field : Class_type_field.t -> Class_type_field.t  =
        fun class_type_field ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let pctf_desc = self#class_type_field_desc pctf_desc in
          let pctf_loc = self#location pctf_loc in
          let pctf_attributes = self#attributes pctf_attributes in
          Class_type_field.of_concrete { pctf_desc; pctf_loc; pctf_attributes }
      method class_type_field_desc : Class_type_field_desc.t -> Class_type_field_desc.t  =
        fun class_type_field_desc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let x0 = self#class_type x0 in
            Class_type_field_desc.of_concrete (Pctf_inherit x0)
          | Pctf_val x0 ->
            let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#loc self#label x0 in let x1 = self#mutable_flag x1 in let x2 = self#virtual_flag x2 in let x3 = self#core_type x3 in (x0, x1, x2, x3)) x0 in
            Class_type_field_desc.of_concrete (Pctf_val x0)
          | Pctf_method x0 ->
            let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#loc self#label x0 in let x1 = self#private_flag x1 in let x2 = self#virtual_flag x2 in let x3 = self#core_type x3 in (x0, x1, x2, x3)) x0 in
            Class_type_field_desc.of_concrete (Pctf_method x0)
          | Pctf_constraint x0 ->
            let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in (x0, x1)) x0 in
            Class_type_field_desc.of_concrete (Pctf_constraint x0)
          | Pctf_attribute x0 ->
            let x0 = self#attribute x0 in
            Class_type_field_desc.of_concrete (Pctf_attribute x0)
          | Pctf_extension x0 ->
            let x0 = self#extension x0 in
            Class_type_field_desc.of_concrete (Pctf_extension x0)
      method class_infos_class_expr : Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag pci_virt in
          let pci_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in (x0, x1)) pci_params in
          let pci_name = self#loc self#string pci_name in
          let pci_expr = self#class_expr pci_expr in
          let pci_loc = self#location pci_loc in
          let pci_attributes = self#attributes pci_attributes in
          Class_infos.of_concrete_class_expr { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
      method class_infos_class_type : Class_type.t Class_infos.t -> Class_type.t Class_infos.t  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag pci_virt in
          let pci_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in (x0, x1)) pci_params in
          let pci_name = self#loc self#string pci_name in
          let pci_expr = self#class_type pci_expr in
          let pci_loc = self#location pci_loc in
          let pci_attributes = self#attributes pci_attributes in
          Class_infos.of_concrete_class_type { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
      method class_description : Class_description.t -> Class_description.t  =
        fun class_description ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type concrete in
          Class_description.of_concrete concrete
      method class_type_declaration : Class_type_declaration.t -> Class_type_declaration.t  =
        fun class_type_declaration ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type concrete in
          Class_type_declaration.of_concrete concrete
      method class_expr : Class_expr.t -> Class_expr.t  =
        fun class_expr ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let pcl_desc = self#class_expr_desc pcl_desc in
          let pcl_loc = self#location pcl_loc in
          let pcl_attributes = self#attributes pcl_attributes in
          Class_expr.of_concrete { pcl_desc; pcl_loc; pcl_attributes }
      method class_expr_desc : Class_expr_desc.t -> Class_expr_desc.t  =
        fun class_expr_desc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            Class_expr_desc.of_concrete (Pcl_constr (x0, x1))
          | Pcl_structure x0 ->
            let x0 = self#class_structure x0 in
            Class_expr_desc.of_concrete (Pcl_structure x0)
          | Pcl_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label x0 in
            let x1 = self#option self#expression x1 in
            let x2 = self#pattern x2 in
            let x3 = self#class_expr x3 in
            Class_expr_desc.of_concrete (Pcl_fun (x0, x1, x2, x3))
          | Pcl_apply (x0, x1) ->
            let x0 = self#class_expr x0 in
            let x1 = self#list (fun (x0, x1) -> let x0 = self#arg_label x0 in let x1 = self#expression x1 in (x0, x1)) x1 in
            Class_expr_desc.of_concrete (Pcl_apply (x0, x1))
          | Pcl_let (x0, x1, x2) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            let x2 = self#class_expr x2 in
            Class_expr_desc.of_concrete (Pcl_let (x0, x1, x2))
          | Pcl_constraint (x0, x1) ->
            let x0 = self#class_expr x0 in
            let x1 = self#class_type x1 in
            Class_expr_desc.of_concrete (Pcl_constraint (x0, x1))
          | Pcl_extension x0 ->
            let x0 = self#extension x0 in
            Class_expr_desc.of_concrete (Pcl_extension x0)
          | Pcl_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#class_expr x2 in
            Class_expr_desc.of_concrete (Pcl_open (x0, x1, x2))
      method class_structure : Class_structure.t -> Class_structure.t  =
        fun class_structure ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let pcstr_self = self#pattern pcstr_self in
          let pcstr_fields = self#list self#class_field pcstr_fields in
          Class_structure.of_concrete { pcstr_self; pcstr_fields }
      method class_field : Class_field.t -> Class_field.t  =
        fun class_field ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let pcf_desc = self#class_field_desc pcf_desc in
          let pcf_loc = self#location pcf_loc in
          let pcf_attributes = self#attributes pcf_attributes in
          Class_field.of_concrete { pcf_desc; pcf_loc; pcf_attributes }
      method class_field_desc : Class_field_desc.t -> Class_field_desc.t  =
        fun class_field_desc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#class_expr x1 in
            let x2 = self#option (self#loc self#string) x2 in
            Class_field_desc.of_concrete (Pcf_inherit (x0, x1, x2))
          | Pcf_val x0 ->
            let x0 = (fun (x0, x1, x2) -> let x0 = self#loc self#label x0 in let x1 = self#mutable_flag x1 in let x2 = self#class_field_kind x2 in (x0, x1, x2)) x0 in
            Class_field_desc.of_concrete (Pcf_val x0)
          | Pcf_method x0 ->
            let x0 = (fun (x0, x1, x2) -> let x0 = self#loc self#label x0 in let x1 = self#private_flag x1 in let x2 = self#class_field_kind x2 in (x0, x1, x2)) x0 in
            Class_field_desc.of_concrete (Pcf_method x0)
          | Pcf_constraint x0 ->
            let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in (x0, x1)) x0 in
            Class_field_desc.of_concrete (Pcf_constraint x0)
          | Pcf_initializer x0 ->
            let x0 = self#expression x0 in
            Class_field_desc.of_concrete (Pcf_initializer x0)
          | Pcf_attribute x0 ->
            let x0 = self#attribute x0 in
            Class_field_desc.of_concrete (Pcf_attribute x0)
          | Pcf_extension x0 ->
            let x0 = self#extension x0 in
            Class_field_desc.of_concrete (Pcf_extension x0)
      method class_field_kind : Class_field_kind.t -> Class_field_kind.t  =
        fun class_field_kind ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let x0 = self#core_type x0 in
            Class_field_kind.of_concrete (Cfk_virtual x0)
          | Cfk_concrete (x0, x1) ->
            let x0 = self#override_flag x0 in
            let x1 = self#expression x1 in
            Class_field_kind.of_concrete (Cfk_concrete (x0, x1))
      method class_declaration : Class_declaration.t -> Class_declaration.t  =
        fun class_declaration ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_expr concrete in
          Class_declaration.of_concrete concrete
      method module_type : Module_type.t -> Module_type.t  =
        fun module_type ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let pmty_desc = self#module_type_desc pmty_desc in
          let pmty_loc = self#location pmty_loc in
          let pmty_attributes = self#attributes pmty_attributes in
          Module_type.of_concrete { pmty_desc; pmty_loc; pmty_attributes }
      method module_type_desc : Module_type_desc.t -> Module_type_desc.t  =
        fun module_type_desc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let x0 = self#longident_loc x0 in
            Module_type_desc.of_concrete (Pmty_ident x0)
          | Pmty_signature x0 ->
            let x0 = self#signature x0 in
            Module_type_desc.of_concrete (Pmty_signature x0)
          | Pmty_functor (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#option self#module_type x1 in
            let x2 = self#module_type x2 in
            Module_type_desc.of_concrete (Pmty_functor (x0, x1, x2))
          | Pmty_with (x0, x1) ->
            let x0 = self#module_type x0 in
            let x1 = self#list self#with_constraint x1 in
            Module_type_desc.of_concrete (Pmty_with (x0, x1))
          | Pmty_typeof x0 ->
            let x0 = self#module_expr x0 in
            Module_type_desc.of_concrete (Pmty_typeof x0)
          | Pmty_extension x0 ->
            let x0 = self#extension x0 in
            Module_type_desc.of_concrete (Pmty_extension x0)
          | Pmty_alias x0 ->
            let x0 = self#longident_loc x0 in
            Module_type_desc.of_concrete (Pmty_alias x0)
      method signature : Signature.t -> Signature.t  =
        fun signature ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let concrete = self#list self#signature_item concrete in
          Signature.of_concrete concrete
      method signature_item : Signature_item.t -> Signature_item.t  =
        fun signature_item ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let psig_desc = self#signature_item_desc psig_desc in
          let psig_loc = self#location psig_loc in
          Signature_item.of_concrete { psig_desc; psig_loc }
      method signature_item_desc : Signature_item_desc.t -> Signature_item_desc.t  =
        fun signature_item_desc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let x0 = self#value_description x0 in
            Signature_item_desc.of_concrete (Psig_value x0)
          | Psig_type (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#type_declaration x1 in
            Signature_item_desc.of_concrete (Psig_type (x0, x1))
          | Psig_typext x0 ->
            let x0 = self#type_extension x0 in
            Signature_item_desc.of_concrete (Psig_typext x0)
          | Psig_exception x0 ->
            let x0 = self#extension_constructor x0 in
            Signature_item_desc.of_concrete (Psig_exception x0)
          | Psig_module x0 ->
            let x0 = self#module_declaration x0 in
            Signature_item_desc.of_concrete (Psig_module x0)
          | Psig_recmodule x0 ->
            let x0 = self#list self#module_declaration x0 in
            Signature_item_desc.of_concrete (Psig_recmodule x0)
          | Psig_modtype x0 ->
            let x0 = self#module_type_declaration x0 in
            Signature_item_desc.of_concrete (Psig_modtype x0)
          | Psig_open x0 ->
            let x0 = self#open_description x0 in
            Signature_item_desc.of_concrete (Psig_open x0)
          | Psig_include x0 ->
            let x0 = self#include_description x0 in
            Signature_item_desc.of_concrete (Psig_include x0)
          | Psig_class x0 ->
            let x0 = self#list self#class_description x0 in
            Signature_item_desc.of_concrete (Psig_class x0)
          | Psig_class_type x0 ->
            let x0 = self#list self#class_type_declaration x0 in
            Signature_item_desc.of_concrete (Psig_class_type x0)
          | Psig_attribute x0 ->
            let x0 = self#attribute x0 in
            Signature_item_desc.of_concrete (Psig_attribute x0)
          | Psig_extension (x0, x1) ->
            let x0 = self#extension x0 in
            let x1 = self#attributes x1 in
            Signature_item_desc.of_concrete (Psig_extension (x0, x1))
      method module_declaration : Module_declaration.t -> Module_declaration.t  =
        fun module_declaration ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let pmd_name = self#loc self#string pmd_name in
          let pmd_type = self#module_type pmd_type in
          let pmd_attributes = self#attributes pmd_attributes in
          let pmd_loc = self#location pmd_loc in
          Module_declaration.of_concrete { pmd_name; pmd_type; pmd_attributes; pmd_loc }
      method module_type_declaration : Module_type_declaration.t -> Module_type_declaration.t  =
        fun module_type_declaration ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let pmtd_name = self#loc self#string pmtd_name in
          let pmtd_type = self#option self#module_type pmtd_type in
          let pmtd_attributes = self#attributes pmtd_attributes in
          let pmtd_loc = self#location pmtd_loc in
          Module_type_declaration.of_concrete { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
      method open_description : Open_description.t -> Open_description.t  =
        fun open_description ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let popen_lid = self#longident_loc popen_lid in
          let popen_override = self#override_flag popen_override in
          let popen_loc = self#location popen_loc in
          let popen_attributes = self#attributes popen_attributes in
          Open_description.of_concrete { popen_lid; popen_override; popen_loc; popen_attributes }
      method include_infos_module_expr : Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_expr pincl_mod in
          let pincl_loc = self#location pincl_loc in
          let pincl_attributes = self#attributes pincl_attributes in
          Include_infos.of_concrete_module_expr { pincl_mod; pincl_loc; pincl_attributes }
      method include_infos_module_type : Module_type.t Include_infos.t -> Module_type.t Include_infos.t  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_type pincl_mod in
          let pincl_loc = self#location pincl_loc in
          let pincl_attributes = self#attributes pincl_attributes in
          Include_infos.of_concrete_module_type { pincl_mod; pincl_loc; pincl_attributes }
      method include_description : Include_description.t -> Include_description.t  =
        fun include_description ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let concrete = self#include_infos_module_type concrete in
          Include_description.of_concrete concrete
      method include_declaration : Include_declaration.t -> Include_declaration.t  =
        fun include_declaration ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let concrete = self#include_infos_module_expr concrete in
          Include_declaration.of_concrete concrete
      method with_constraint : With_constraint.t -> With_constraint.t  =
        fun with_constraint ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#type_declaration x1 in
            With_constraint.of_concrete (Pwith_type (x0, x1))
          | Pwith_module (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#longident_loc x1 in
            With_constraint.of_concrete (Pwith_module (x0, x1))
          | Pwith_typesubst (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#type_declaration x1 in
            With_constraint.of_concrete (Pwith_typesubst (x0, x1))
          | Pwith_modsubst (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#longident_loc x1 in
            With_constraint.of_concrete (Pwith_modsubst (x0, x1))
      method module_expr : Module_expr.t -> Module_expr.t  =
        fun module_expr ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let pmod_desc = self#module_expr_desc pmod_desc in
          let pmod_loc = self#location pmod_loc in
          let pmod_attributes = self#attributes pmod_attributes in
          Module_expr.of_concrete { pmod_desc; pmod_loc; pmod_attributes }
      method module_expr_desc : Module_expr_desc.t -> Module_expr_desc.t  =
        fun module_expr_desc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let x0 = self#longident_loc x0 in
            Module_expr_desc.of_concrete (Pmod_ident x0)
          | Pmod_structure x0 ->
            let x0 = self#structure x0 in
            Module_expr_desc.of_concrete (Pmod_structure x0)
          | Pmod_functor (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#option self#module_type x1 in
            let x2 = self#module_expr x2 in
            Module_expr_desc.of_concrete (Pmod_functor (x0, x1, x2))
          | Pmod_apply (x0, x1) ->
            let x0 = self#module_expr x0 in
            let x1 = self#module_expr x1 in
            Module_expr_desc.of_concrete (Pmod_apply (x0, x1))
          | Pmod_constraint (x0, x1) ->
            let x0 = self#module_expr x0 in
            let x1 = self#module_type x1 in
            Module_expr_desc.of_concrete (Pmod_constraint (x0, x1))
          | Pmod_unpack x0 ->
            let x0 = self#expression x0 in
            Module_expr_desc.of_concrete (Pmod_unpack x0)
          | Pmod_extension x0 ->
            let x0 = self#extension x0 in
            Module_expr_desc.of_concrete (Pmod_extension x0)
      method structure : Structure.t -> Structure.t  =
        fun structure ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let concrete = self#list self#structure_item concrete in
          Structure.of_concrete concrete
      method structure_item : Structure_item.t -> Structure_item.t  =
        fun structure_item ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let pstr_desc = self#structure_item_desc pstr_desc in
          let pstr_loc = self#location pstr_loc in
          Structure_item.of_concrete { pstr_desc; pstr_loc }
      method structure_item_desc : Structure_item_desc.t -> Structure_item_desc.t  =
        fun structure_item_desc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#attributes x1 in
            Structure_item_desc.of_concrete (Pstr_eval (x0, x1))
          | Pstr_value (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            Structure_item_desc.of_concrete (Pstr_value (x0, x1))
          | Pstr_primitive x0 ->
            let x0 = self#value_description x0 in
            Structure_item_desc.of_concrete (Pstr_primitive x0)
          | Pstr_type (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#type_declaration x1 in
            Structure_item_desc.of_concrete (Pstr_type (x0, x1))
          | Pstr_typext x0 ->
            let x0 = self#type_extension x0 in
            Structure_item_desc.of_concrete (Pstr_typext x0)
          | Pstr_exception x0 ->
            let x0 = self#extension_constructor x0 in
            Structure_item_desc.of_concrete (Pstr_exception x0)
          | Pstr_module x0 ->
            let x0 = self#module_binding x0 in
            Structure_item_desc.of_concrete (Pstr_module x0)
          | Pstr_recmodule x0 ->
            let x0 = self#list self#module_binding x0 in
            Structure_item_desc.of_concrete (Pstr_recmodule x0)
          | Pstr_modtype x0 ->
            let x0 = self#module_type_declaration x0 in
            Structure_item_desc.of_concrete (Pstr_modtype x0)
          | Pstr_open x0 ->
            let x0 = self#open_description x0 in
            Structure_item_desc.of_concrete (Pstr_open x0)
          | Pstr_class x0 ->
            let x0 = self#list self#class_declaration x0 in
            Structure_item_desc.of_concrete (Pstr_class x0)
          | Pstr_class_type x0 ->
            let x0 = self#list self#class_type_declaration x0 in
            Structure_item_desc.of_concrete (Pstr_class_type x0)
          | Pstr_include x0 ->
            let x0 = self#include_declaration x0 in
            Structure_item_desc.of_concrete (Pstr_include x0)
          | Pstr_attribute x0 ->
            let x0 = self#attribute x0 in
            Structure_item_desc.of_concrete (Pstr_attribute x0)
          | Pstr_extension (x0, x1) ->
            let x0 = self#extension x0 in
            let x1 = self#attributes x1 in
            Structure_item_desc.of_concrete (Pstr_extension (x0, x1))
      method value_binding : Value_binding.t -> Value_binding.t  =
        fun value_binding ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let pvb_pat = self#pattern pvb_pat in
          let pvb_expr = self#expression pvb_expr in
          let pvb_attributes = self#attributes pvb_attributes in
          let pvb_loc = self#location pvb_loc in
          Value_binding.of_concrete { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
      method module_binding : Module_binding.t -> Module_binding.t  =
        fun module_binding ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let pmb_name = self#loc self#string pmb_name in
          let pmb_expr = self#module_expr pmb_expr in
          let pmb_attributes = self#attributes pmb_attributes in
          let pmb_loc = self#location pmb_loc in
          Module_binding.of_concrete { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
      method toplevel_phrase : Toplevel_phrase.t -> Toplevel_phrase.t  =
        fun toplevel_phrase ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let x0 = self#structure x0 in
            Toplevel_phrase.of_concrete (Ptop_def x0)
          | Ptop_dir (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#directive_argument x1 in
            Toplevel_phrase.of_concrete (Ptop_dir (x0, x1))
      method directive_argument : Directive_argument.t -> Directive_argument.t  =
        fun directive_argument ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            Directive_argument.of_concrete Pdir_none
          | Pdir_string x0 ->
            let x0 = self#string x0 in
            Directive_argument.of_concrete (Pdir_string x0)
          | Pdir_int (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            Directive_argument.of_concrete (Pdir_int (x0, x1))
          | Pdir_ident x0 ->
            let x0 = self#longident x0 in
            Directive_argument.of_concrete (Pdir_ident x0)
          | Pdir_bool x0 ->
            let x0 = self#bool x0 in
            Directive_argument.of_concrete (Pdir_bool x0)
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
      method longident : Longident.t -> unit  =
        fun longident ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            self#string x0
          | Ldot (x0, x1) ->
            self#longident x0;
            self#string x1
          | Lapply (x0, x1) ->
            self#longident x0;
            self#longident x1
      method longident_loc : Longident_loc.t -> unit  =
        fun longident_loc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          self#loc self#longident concrete
      method rec_flag : Rec_flag.t -> unit  =
        fun rec_flag ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            ()
          | Recursive ->
            ()
      method direction_flag : Direction_flag.t -> unit  =
        fun direction_flag ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            ()
          | Downto ->
            ()
      method private_flag : Private_flag.t -> unit  =
        fun private_flag ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            ()
          | Public ->
            ()
      method mutable_flag : Mutable_flag.t -> unit  =
        fun mutable_flag ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            ()
          | Mutable ->
            ()
      method virtual_flag : Virtual_flag.t -> unit  =
        fun virtual_flag ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            ()
          | Concrete ->
            ()
      method override_flag : Override_flag.t -> unit  =
        fun override_flag ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            ()
          | Fresh ->
            ()
      method closed_flag : Closed_flag.t -> unit  =
        fun closed_flag ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            ()
          | Open ->
            ()
      method label : Label.t -> unit  =
        fun label ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          self#string concrete
      method arg_label : Arg_label.t -> unit  =
        fun arg_label ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            ()
          | Labelled x0 ->
            self#string x0
          | Optional x0 ->
            self#string x0
      method variance : Variance.t -> unit  =
        fun variance ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            ()
          | Contravariant ->
            ()
          | Invariant ->
            ()
      method constant : Constant.t -> unit  =
        fun constant ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            self#string x0;
            self#option self#char x1
          | Pconst_char x0 ->
            self#char x0
          | Pconst_string (x0, x1) ->
            self#string x0;
            self#option self#string x1
          | Pconst_float (x0, x1) ->
            self#string x0;
            self#option self#char x1
      method attribute : Attribute.t -> unit  =
        fun attribute ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          self#loc self#string x0;
          self#payload x1
      method extension : Extension.t -> unit  =
        fun extension ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          self#loc self#string x0;
          self#payload x1
      method attributes : Attributes.t -> unit  =
        fun attributes ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          self#list self#attribute concrete
      method payload : Payload.t -> unit  =
        fun payload ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            self#structure x0
          | PSig x0 ->
            self#signature x0
          | PTyp x0 ->
            self#core_type x0
          | PPat (x0, x1) ->
            self#pattern x0;
            self#option self#expression x1
      method core_type : Core_type.t -> unit  =
        fun core_type ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          self#core_type_desc ptyp_desc;
          self#location ptyp_loc;
          self#attributes ptyp_attributes
      method core_type_desc : Core_type_desc.t -> unit  =
        fun core_type_desc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            ()
          | Ptyp_var x0 ->
            self#string x0
          | Ptyp_arrow (x0, x1, x2) ->
            self#arg_label x0;
            self#core_type x1;
            self#core_type x2
          | Ptyp_tuple x0 ->
            self#list self#core_type x0
          | Ptyp_constr (x0, x1) ->
            self#longident_loc x0;
            self#list self#core_type x1
          | Ptyp_object (x0, x1) ->
            self#list self#object_field x0;
            self#closed_flag x1
          | Ptyp_class (x0, x1) ->
            self#longident_loc x0;
            self#list self#core_type x1
          | Ptyp_alias (x0, x1) ->
            self#core_type x0;
            self#string x1
          | Ptyp_variant (x0, x1, x2) ->
            self#list self#row_field x0;
            self#closed_flag x1;
            self#option (self#list self#label) x2
          | Ptyp_poly (x0, x1) ->
            self#list (self#loc self#string) x0;
            self#core_type x1
          | Ptyp_package x0 ->
            self#package_type x0
          | Ptyp_extension x0 ->
            self#extension x0
      method package_type : Package_type.t -> unit  =
        fun package_type ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          self#longident_loc x0;
          self#list (fun (x0, x1) -> self#longident_loc x0; self#core_type x1) x1
      method row_field : Row_field.t -> unit  =
        fun row_field ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            self#loc self#label x0;
            self#attributes x1;
            self#bool x2;
            self#list self#core_type x3
          | Rinherit x0 ->
            self#core_type x0
      method object_field : Object_field.t -> unit  =
        fun object_field ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            self#loc self#label x0;
            self#attributes x1;
            self#core_type x2
          | Oinherit x0 ->
            self#core_type x0
      method pattern : Pattern.t -> unit  =
        fun pattern ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          self#pattern_desc ppat_desc;
          self#location ppat_loc;
          self#attributes ppat_attributes
      method pattern_desc : Pattern_desc.t -> unit  =
        fun pattern_desc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            ()
          | Ppat_var x0 ->
            self#loc self#string x0
          | Ppat_alias (x0, x1) ->
            self#pattern x0;
            self#loc self#string x1
          | Ppat_constant x0 ->
            self#constant x0
          | Ppat_interval (x0, x1) ->
            self#constant x0;
            self#constant x1
          | Ppat_tuple x0 ->
            self#list self#pattern x0
          | Ppat_construct (x0, x1) ->
            self#longident_loc x0;
            self#option self#pattern x1
          | Ppat_variant (x0, x1) ->
            self#label x0;
            self#option self#pattern x1
          | Ppat_record (x0, x1) ->
            self#list (fun (x0, x1) -> self#longident_loc x0; self#pattern x1) x0;
            self#closed_flag x1
          | Ppat_array x0 ->
            self#list self#pattern x0
          | Ppat_or (x0, x1) ->
            self#pattern x0;
            self#pattern x1
          | Ppat_constraint (x0, x1) ->
            self#pattern x0;
            self#core_type x1
          | Ppat_type x0 ->
            self#longident_loc x0
          | Ppat_lazy x0 ->
            self#pattern x0
          | Ppat_unpack x0 ->
            self#loc self#string x0
          | Ppat_exception x0 ->
            self#pattern x0
          | Ppat_extension x0 ->
            self#extension x0
          | Ppat_open (x0, x1) ->
            self#longident_loc x0;
            self#pattern x1
      method expression : Expression.t -> unit  =
        fun expression ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          self#expression_desc pexp_desc;
          self#location pexp_loc;
          self#attributes pexp_attributes
      method expression_desc : Expression_desc.t -> unit  =
        fun expression_desc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            self#longident_loc x0
          | Pexp_constant x0 ->
            self#constant x0
          | Pexp_let (x0, x1, x2) ->
            self#rec_flag x0;
            self#list self#value_binding x1;
            self#expression x2
          | Pexp_function x0 ->
            self#list self#case x0
          | Pexp_fun (x0, x1, x2, x3) ->
            self#arg_label x0;
            self#option self#expression x1;
            self#pattern x2;
            self#expression x3
          | Pexp_apply (x0, x1) ->
            self#expression x0;
            self#list (fun (x0, x1) -> self#arg_label x0; self#expression x1) x1
          | Pexp_match (x0, x1) ->
            self#expression x0;
            self#list self#case x1
          | Pexp_try (x0, x1) ->
            self#expression x0;
            self#list self#case x1
          | Pexp_tuple x0 ->
            self#list self#expression x0
          | Pexp_construct (x0, x1) ->
            self#longident_loc x0;
            self#option self#expression x1
          | Pexp_variant (x0, x1) ->
            self#label x0;
            self#option self#expression x1
          | Pexp_record (x0, x1) ->
            self#list (fun (x0, x1) -> self#longident_loc x0; self#expression x1) x0;
            self#option self#expression x1
          | Pexp_field (x0, x1) ->
            self#expression x0;
            self#longident_loc x1
          | Pexp_setfield (x0, x1, x2) ->
            self#expression x0;
            self#longident_loc x1;
            self#expression x2
          | Pexp_array x0 ->
            self#list self#expression x0
          | Pexp_ifthenelse (x0, x1, x2) ->
            self#expression x0;
            self#expression x1;
            self#option self#expression x2
          | Pexp_sequence (x0, x1) ->
            self#expression x0;
            self#expression x1
          | Pexp_while (x0, x1) ->
            self#expression x0;
            self#expression x1
          | Pexp_for (x0, x1, x2, x3, x4) ->
            self#pattern x0;
            self#expression x1;
            self#expression x2;
            self#direction_flag x3;
            self#expression x4
          | Pexp_constraint (x0, x1) ->
            self#expression x0;
            self#core_type x1
          | Pexp_coerce (x0, x1, x2) ->
            self#expression x0;
            self#option self#core_type x1;
            self#core_type x2
          | Pexp_send (x0, x1) ->
            self#expression x0;
            self#loc self#label x1
          | Pexp_new x0 ->
            self#longident_loc x0
          | Pexp_setinstvar (x0, x1) ->
            self#loc self#label x0;
            self#expression x1
          | Pexp_override x0 ->
            self#list (fun (x0, x1) -> self#loc self#label x0; self#expression x1) x0
          | Pexp_letmodule (x0, x1, x2) ->
            self#loc self#string x0;
            self#module_expr x1;
            self#expression x2
          | Pexp_letexception (x0, x1) ->
            self#extension_constructor x0;
            self#expression x1
          | Pexp_assert x0 ->
            self#expression x0
          | Pexp_lazy x0 ->
            self#expression x0
          | Pexp_poly (x0, x1) ->
            self#expression x0;
            self#option self#core_type x1
          | Pexp_object x0 ->
            self#class_structure x0
          | Pexp_newtype (x0, x1) ->
            self#loc self#string x0;
            self#expression x1
          | Pexp_pack x0 ->
            self#module_expr x0
          | Pexp_open (x0, x1, x2) ->
            self#override_flag x0;
            self#longident_loc x1;
            self#expression x2
          | Pexp_extension x0 ->
            self#extension x0
          | Pexp_unreachable ->
            ()
      method case : Case.t -> unit  =
        fun case ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          self#pattern pc_lhs;
          self#option self#expression pc_guard;
          self#expression pc_rhs
      method value_description : Value_description.t -> unit  =
        fun value_description ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          self#loc self#string pval_name;
          self#core_type pval_type;
          self#list self#string pval_prim;
          self#attributes pval_attributes;
          self#location pval_loc
      method type_declaration : Type_declaration.t -> unit  =
        fun type_declaration ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          self#loc self#string ptype_name;
          self#list (fun (x0, x1) -> self#core_type x0; self#variance x1) ptype_params;
          self#list (fun (x0, x1, x2) -> self#core_type x0; self#core_type x1; self#location x2) ptype_cstrs;
          self#type_kind ptype_kind;
          self#private_flag ptype_private;
          self#option self#core_type ptype_manifest;
          self#attributes ptype_attributes;
          self#location ptype_loc
      method type_kind : Type_kind.t -> unit  =
        fun type_kind ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            ()
          | Ptype_variant x0 ->
            self#list self#constructor_declaration x0
          | Ptype_record x0 ->
            self#list self#label_declaration x0
          | Ptype_open ->
            ()
      method label_declaration : Label_declaration.t -> unit  =
        fun label_declaration ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          self#loc self#string pld_name;
          self#mutable_flag pld_mutable;
          self#core_type pld_type;
          self#location pld_loc;
          self#attributes pld_attributes
      method constructor_declaration : Constructor_declaration.t -> unit  =
        fun constructor_declaration ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          self#loc self#string pcd_name;
          self#constructor_arguments pcd_args;
          self#option self#core_type pcd_res;
          self#location pcd_loc;
          self#attributes pcd_attributes
      method constructor_arguments : Constructor_arguments.t -> unit  =
        fun constructor_arguments ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            self#list self#core_type x0
          | Pcstr_record x0 ->
            self#list self#label_declaration x0
      method type_extension : Type_extension.t -> unit  =
        fun type_extension ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          self#longident_loc ptyext_path;
          self#list (fun (x0, x1) -> self#core_type x0; self#variance x1) ptyext_params;
          self#list self#extension_constructor ptyext_constructors;
          self#private_flag ptyext_private;
          self#attributes ptyext_attributes
      method extension_constructor : Extension_constructor.t -> unit  =
        fun extension_constructor ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          self#loc self#string pext_name;
          self#extension_constructor_kind pext_kind;
          self#location pext_loc;
          self#attributes pext_attributes
      method extension_constructor_kind : Extension_constructor_kind.t -> unit  =
        fun extension_constructor_kind ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            self#constructor_arguments x0;
            self#option self#core_type x1
          | Pext_rebind x0 ->
            self#longident_loc x0
      method class_type : Class_type.t -> unit  =
        fun class_type ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          self#class_type_desc pcty_desc;
          self#location pcty_loc;
          self#attributes pcty_attributes
      method class_type_desc : Class_type_desc.t -> unit  =
        fun class_type_desc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            self#longident_loc x0;
            self#list self#core_type x1
          | Pcty_signature x0 ->
            self#class_signature x0
          | Pcty_arrow (x0, x1, x2) ->
            self#arg_label x0;
            self#core_type x1;
            self#class_type x2
          | Pcty_extension x0 ->
            self#extension x0
          | Pcty_open (x0, x1, x2) ->
            self#override_flag x0;
            self#longident_loc x1;
            self#class_type x2
      method class_signature : Class_signature.t -> unit  =
        fun class_signature ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          self#core_type pcsig_self;
          self#list self#class_type_field pcsig_fields
      method class_type_field : Class_type_field.t -> unit  =
        fun class_type_field ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          self#class_type_field_desc pctf_desc;
          self#location pctf_loc;
          self#attributes pctf_attributes
      method class_type_field_desc : Class_type_field_desc.t -> unit  =
        fun class_type_field_desc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            self#class_type x0
          | Pctf_val x0 ->
            (fun (x0, x1, x2, x3) -> self#loc self#label x0; self#mutable_flag x1; self#virtual_flag x2; self#core_type x3) x0
          | Pctf_method x0 ->
            (fun (x0, x1, x2, x3) -> self#loc self#label x0; self#private_flag x1; self#virtual_flag x2; self#core_type x3) x0
          | Pctf_constraint x0 ->
            (fun (x0, x1) -> self#core_type x0; self#core_type x1) x0
          | Pctf_attribute x0 ->
            self#attribute x0
          | Pctf_extension x0 ->
            self#extension x0
      method class_infos_class_expr : Class_expr.t Class_infos.t -> unit  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          self#virtual_flag pci_virt;
          self#list (fun (x0, x1) -> self#core_type x0; self#variance x1) pci_params;
          self#loc self#string pci_name;
          self#class_expr pci_expr;
          self#location pci_loc;
          self#attributes pci_attributes
      method class_infos_class_type : Class_type.t Class_infos.t -> unit  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          self#virtual_flag pci_virt;
          self#list (fun (x0, x1) -> self#core_type x0; self#variance x1) pci_params;
          self#loc self#string pci_name;
          self#class_type pci_expr;
          self#location pci_loc;
          self#attributes pci_attributes
      method class_description : Class_description.t -> unit  =
        fun class_description ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          self#class_infos_class_type concrete
      method class_type_declaration : Class_type_declaration.t -> unit  =
        fun class_type_declaration ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          self#class_infos_class_type concrete
      method class_expr : Class_expr.t -> unit  =
        fun class_expr ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          self#class_expr_desc pcl_desc;
          self#location pcl_loc;
          self#attributes pcl_attributes
      method class_expr_desc : Class_expr_desc.t -> unit  =
        fun class_expr_desc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            self#longident_loc x0;
            self#list self#core_type x1
          | Pcl_structure x0 ->
            self#class_structure x0
          | Pcl_fun (x0, x1, x2, x3) ->
            self#arg_label x0;
            self#option self#expression x1;
            self#pattern x2;
            self#class_expr x3
          | Pcl_apply (x0, x1) ->
            self#class_expr x0;
            self#list (fun (x0, x1) -> self#arg_label x0; self#expression x1) x1
          | Pcl_let (x0, x1, x2) ->
            self#rec_flag x0;
            self#list self#value_binding x1;
            self#class_expr x2
          | Pcl_constraint (x0, x1) ->
            self#class_expr x0;
            self#class_type x1
          | Pcl_extension x0 ->
            self#extension x0
          | Pcl_open (x0, x1, x2) ->
            self#override_flag x0;
            self#longident_loc x1;
            self#class_expr x2
      method class_structure : Class_structure.t -> unit  =
        fun class_structure ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          self#pattern pcstr_self;
          self#list self#class_field pcstr_fields
      method class_field : Class_field.t -> unit  =
        fun class_field ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          self#class_field_desc pcf_desc;
          self#location pcf_loc;
          self#attributes pcf_attributes
      method class_field_desc : Class_field_desc.t -> unit  =
        fun class_field_desc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            self#override_flag x0;
            self#class_expr x1;
            self#option (self#loc self#string) x2
          | Pcf_val x0 ->
            (fun (x0, x1, x2) -> self#loc self#label x0; self#mutable_flag x1; self#class_field_kind x2) x0
          | Pcf_method x0 ->
            (fun (x0, x1, x2) -> self#loc self#label x0; self#private_flag x1; self#class_field_kind x2) x0
          | Pcf_constraint x0 ->
            (fun (x0, x1) -> self#core_type x0; self#core_type x1) x0
          | Pcf_initializer x0 ->
            self#expression x0
          | Pcf_attribute x0 ->
            self#attribute x0
          | Pcf_extension x0 ->
            self#extension x0
      method class_field_kind : Class_field_kind.t -> unit  =
        fun class_field_kind ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            self#core_type x0
          | Cfk_concrete (x0, x1) ->
            self#override_flag x0;
            self#expression x1
      method class_declaration : Class_declaration.t -> unit  =
        fun class_declaration ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          self#class_infos_class_expr concrete
      method module_type : Module_type.t -> unit  =
        fun module_type ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          self#module_type_desc pmty_desc;
          self#location pmty_loc;
          self#attributes pmty_attributes
      method module_type_desc : Module_type_desc.t -> unit  =
        fun module_type_desc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            self#longident_loc x0
          | Pmty_signature x0 ->
            self#signature x0
          | Pmty_functor (x0, x1, x2) ->
            self#loc self#string x0;
            self#option self#module_type x1;
            self#module_type x2
          | Pmty_with (x0, x1) ->
            self#module_type x0;
            self#list self#with_constraint x1
          | Pmty_typeof x0 ->
            self#module_expr x0
          | Pmty_extension x0 ->
            self#extension x0
          | Pmty_alias x0 ->
            self#longident_loc x0
      method signature : Signature.t -> unit  =
        fun signature ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          self#list self#signature_item concrete
      method signature_item : Signature_item.t -> unit  =
        fun signature_item ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          self#signature_item_desc psig_desc;
          self#location psig_loc
      method signature_item_desc : Signature_item_desc.t -> unit  =
        fun signature_item_desc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            self#value_description x0
          | Psig_type (x0, x1) ->
            self#rec_flag x0;
            self#list self#type_declaration x1
          | Psig_typext x0 ->
            self#type_extension x0
          | Psig_exception x0 ->
            self#extension_constructor x0
          | Psig_module x0 ->
            self#module_declaration x0
          | Psig_recmodule x0 ->
            self#list self#module_declaration x0
          | Psig_modtype x0 ->
            self#module_type_declaration x0
          | Psig_open x0 ->
            self#open_description x0
          | Psig_include x0 ->
            self#include_description x0
          | Psig_class x0 ->
            self#list self#class_description x0
          | Psig_class_type x0 ->
            self#list self#class_type_declaration x0
          | Psig_attribute x0 ->
            self#attribute x0
          | Psig_extension (x0, x1) ->
            self#extension x0;
            self#attributes x1
      method module_declaration : Module_declaration.t -> unit  =
        fun module_declaration ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          self#loc self#string pmd_name;
          self#module_type pmd_type;
          self#attributes pmd_attributes;
          self#location pmd_loc
      method module_type_declaration : Module_type_declaration.t -> unit  =
        fun module_type_declaration ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          self#loc self#string pmtd_name;
          self#option self#module_type pmtd_type;
          self#attributes pmtd_attributes;
          self#location pmtd_loc
      method open_description : Open_description.t -> unit  =
        fun open_description ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          self#longident_loc popen_lid;
          self#override_flag popen_override;
          self#location popen_loc;
          self#attributes popen_attributes
      method include_infos_module_expr : Module_expr.t Include_infos.t -> unit  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          self#module_expr pincl_mod;
          self#location pincl_loc;
          self#attributes pincl_attributes
      method include_infos_module_type : Module_type.t Include_infos.t -> unit  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          self#module_type pincl_mod;
          self#location pincl_loc;
          self#attributes pincl_attributes
      method include_description : Include_description.t -> unit  =
        fun include_description ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          self#include_infos_module_type concrete
      method include_declaration : Include_declaration.t -> unit  =
        fun include_declaration ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          self#include_infos_module_expr concrete
      method with_constraint : With_constraint.t -> unit  =
        fun with_constraint ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            self#longident_loc x0;
            self#type_declaration x1
          | Pwith_module (x0, x1) ->
            self#longident_loc x0;
            self#longident_loc x1
          | Pwith_typesubst (x0, x1) ->
            self#longident_loc x0;
            self#type_declaration x1
          | Pwith_modsubst (x0, x1) ->
            self#longident_loc x0;
            self#longident_loc x1
      method module_expr : Module_expr.t -> unit  =
        fun module_expr ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          self#module_expr_desc pmod_desc;
          self#location pmod_loc;
          self#attributes pmod_attributes
      method module_expr_desc : Module_expr_desc.t -> unit  =
        fun module_expr_desc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            self#longident_loc x0
          | Pmod_structure x0 ->
            self#structure x0
          | Pmod_functor (x0, x1, x2) ->
            self#loc self#string x0;
            self#option self#module_type x1;
            self#module_expr x2
          | Pmod_apply (x0, x1) ->
            self#module_expr x0;
            self#module_expr x1
          | Pmod_constraint (x0, x1) ->
            self#module_expr x0;
            self#module_type x1
          | Pmod_unpack x0 ->
            self#expression x0
          | Pmod_extension x0 ->
            self#extension x0
      method structure : Structure.t -> unit  =
        fun structure ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          self#list self#structure_item concrete
      method structure_item : Structure_item.t -> unit  =
        fun structure_item ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          self#structure_item_desc pstr_desc;
          self#location pstr_loc
      method structure_item_desc : Structure_item_desc.t -> unit  =
        fun structure_item_desc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            self#expression x0;
            self#attributes x1
          | Pstr_value (x0, x1) ->
            self#rec_flag x0;
            self#list self#value_binding x1
          | Pstr_primitive x0 ->
            self#value_description x0
          | Pstr_type (x0, x1) ->
            self#rec_flag x0;
            self#list self#type_declaration x1
          | Pstr_typext x0 ->
            self#type_extension x0
          | Pstr_exception x0 ->
            self#extension_constructor x0
          | Pstr_module x0 ->
            self#module_binding x0
          | Pstr_recmodule x0 ->
            self#list self#module_binding x0
          | Pstr_modtype x0 ->
            self#module_type_declaration x0
          | Pstr_open x0 ->
            self#open_description x0
          | Pstr_class x0 ->
            self#list self#class_declaration x0
          | Pstr_class_type x0 ->
            self#list self#class_type_declaration x0
          | Pstr_include x0 ->
            self#include_declaration x0
          | Pstr_attribute x0 ->
            self#attribute x0
          | Pstr_extension (x0, x1) ->
            self#extension x0;
            self#attributes x1
      method value_binding : Value_binding.t -> unit  =
        fun value_binding ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          self#pattern pvb_pat;
          self#expression pvb_expr;
          self#attributes pvb_attributes;
          self#location pvb_loc
      method module_binding : Module_binding.t -> unit  =
        fun module_binding ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          self#loc self#string pmb_name;
          self#module_expr pmb_expr;
          self#attributes pmb_attributes;
          self#location pmb_loc
      method toplevel_phrase : Toplevel_phrase.t -> unit  =
        fun toplevel_phrase ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            self#structure x0
          | Ptop_dir (x0, x1) ->
            self#string x0;
            self#directive_argument x1
      method directive_argument : Directive_argument.t -> unit  =
        fun directive_argument ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            ()
          | Pdir_string x0 ->
            self#string x0
          | Pdir_int (x0, x1) ->
            self#string x0;
            self#option self#char x1
          | Pdir_ident x0 ->
            self#longident x0
          | Pdir_bool x0 ->
            self#bool x0
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
      method longident : Longident.t -> 'acc -> 'acc  =
        fun longident acc ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let acc = self#string x0 acc in
            acc
          | Ldot (x0, x1) ->
            let acc = self#longident x0 acc in
            let acc = self#string x1 acc in
            acc
          | Lapply (x0, x1) ->
            let acc = self#longident x0 acc in
            let acc = self#longident x1 acc in
            acc
      method longident_loc : Longident_loc.t -> 'acc -> 'acc  =
        fun longident_loc acc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let acc = self#loc self#longident concrete acc in
          acc
      method rec_flag : Rec_flag.t -> 'acc -> 'acc  =
        fun rec_flag acc ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            acc
          | Recursive ->
            acc
      method direction_flag : Direction_flag.t -> 'acc -> 'acc  =
        fun direction_flag acc ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            acc
          | Downto ->
            acc
      method private_flag : Private_flag.t -> 'acc -> 'acc  =
        fun private_flag acc ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            acc
          | Public ->
            acc
      method mutable_flag : Mutable_flag.t -> 'acc -> 'acc  =
        fun mutable_flag acc ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            acc
          | Mutable ->
            acc
      method virtual_flag : Virtual_flag.t -> 'acc -> 'acc  =
        fun virtual_flag acc ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            acc
          | Concrete ->
            acc
      method override_flag : Override_flag.t -> 'acc -> 'acc  =
        fun override_flag acc ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            acc
          | Fresh ->
            acc
      method closed_flag : Closed_flag.t -> 'acc -> 'acc  =
        fun closed_flag acc ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            acc
          | Open ->
            acc
      method label : Label.t -> 'acc -> 'acc  =
        fun label acc ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let acc = self#string concrete acc in
          acc
      method arg_label : Arg_label.t -> 'acc -> 'acc  =
        fun arg_label acc ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            acc
          | Labelled x0 ->
            let acc = self#string x0 acc in
            acc
          | Optional x0 ->
            let acc = self#string x0 acc in
            acc
      method variance : Variance.t -> 'acc -> 'acc  =
        fun variance acc ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            acc
          | Contravariant ->
            acc
          | Invariant ->
            acc
      method constant : Constant.t -> 'acc -> 'acc  =
        fun constant acc ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#option self#char x1 acc in
            acc
          | Pconst_char x0 ->
            let acc = self#char x0 acc in
            acc
          | Pconst_string (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#option self#string x1 acc in
            acc
          | Pconst_float (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#option self#char x1 acc in
            acc
      method attribute : Attribute.t -> 'acc -> 'acc  =
        fun attribute acc ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let acc = self#loc self#string x0 acc in
          let acc = self#payload x1 acc in
          acc
      method extension : Extension.t -> 'acc -> 'acc  =
        fun extension acc ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let acc = self#loc self#string x0 acc in
          let acc = self#payload x1 acc in
          acc
      method attributes : Attributes.t -> 'acc -> 'acc  =
        fun attributes acc ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let acc = self#list self#attribute concrete acc in
          acc
      method payload : Payload.t -> 'acc -> 'acc  =
        fun payload acc ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let acc = self#structure x0 acc in
            acc
          | PSig x0 ->
            let acc = self#signature x0 acc in
            acc
          | PTyp x0 ->
            let acc = self#core_type x0 acc in
            acc
          | PPat (x0, x1) ->
            let acc = self#pattern x0 acc in
            let acc = self#option self#expression x1 acc in
            acc
      method core_type : Core_type.t -> 'acc -> 'acc  =
        fun core_type acc ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let acc = self#core_type_desc ptyp_desc acc in
          let acc = self#location ptyp_loc acc in
          let acc = self#attributes ptyp_attributes acc in
          acc
      method core_type_desc : Core_type_desc.t -> 'acc -> 'acc  =
        fun core_type_desc acc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            acc
          | Ptyp_var x0 ->
            let acc = self#string x0 acc in
            acc
          | Ptyp_arrow (x0, x1, x2) ->
            let acc = self#arg_label x0 acc in
            let acc = self#core_type x1 acc in
            let acc = self#core_type x2 acc in
            acc
          | Ptyp_tuple x0 ->
            let acc = self#list self#core_type x0 acc in
            acc
          | Ptyp_constr (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#list self#core_type x1 acc in
            acc
          | Ptyp_object (x0, x1) ->
            let acc = self#list self#object_field x0 acc in
            let acc = self#closed_flag x1 acc in
            acc
          | Ptyp_class (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#list self#core_type x1 acc in
            acc
          | Ptyp_alias (x0, x1) ->
            let acc = self#core_type x0 acc in
            let acc = self#string x1 acc in
            acc
          | Ptyp_variant (x0, x1, x2) ->
            let acc = self#list self#row_field x0 acc in
            let acc = self#closed_flag x1 acc in
            let acc = self#option (self#list self#label) x2 acc in
            acc
          | Ptyp_poly (x0, x1) ->
            let acc = self#list (self#loc self#string) x0 acc in
            let acc = self#core_type x1 acc in
            acc
          | Ptyp_package x0 ->
            let acc = self#package_type x0 acc in
            acc
          | Ptyp_extension x0 ->
            let acc = self#extension x0 acc in
            acc
      method package_type : Package_type.t -> 'acc -> 'acc  =
        fun package_type acc ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let acc = self#longident_loc x0 acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#longident_loc x0 acc in let acc = self#core_type x1 acc in acc) x1 acc in
          acc
      method row_field : Row_field.t -> 'acc -> 'acc  =
        fun row_field acc ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let acc = self#loc self#label x0 acc in
            let acc = self#attributes x1 acc in
            let acc = self#bool x2 acc in
            let acc = self#list self#core_type x3 acc in
            acc
          | Rinherit x0 ->
            let acc = self#core_type x0 acc in
            acc
      method object_field : Object_field.t -> 'acc -> 'acc  =
        fun object_field acc ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let acc = self#loc self#label x0 acc in
            let acc = self#attributes x1 acc in
            let acc = self#core_type x2 acc in
            acc
          | Oinherit x0 ->
            let acc = self#core_type x0 acc in
            acc
      method pattern : Pattern.t -> 'acc -> 'acc  =
        fun pattern acc ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let acc = self#pattern_desc ppat_desc acc in
          let acc = self#location ppat_loc acc in
          let acc = self#attributes ppat_attributes acc in
          acc
      method pattern_desc : Pattern_desc.t -> 'acc -> 'acc  =
        fun pattern_desc acc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            acc
          | Ppat_var x0 ->
            let acc = self#loc self#string x0 acc in
            acc
          | Ppat_alias (x0, x1) ->
            let acc = self#pattern x0 acc in
            let acc = self#loc self#string x1 acc in
            acc
          | Ppat_constant x0 ->
            let acc = self#constant x0 acc in
            acc
          | Ppat_interval (x0, x1) ->
            let acc = self#constant x0 acc in
            let acc = self#constant x1 acc in
            acc
          | Ppat_tuple x0 ->
            let acc = self#list self#pattern x0 acc in
            acc
          | Ppat_construct (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#option self#pattern x1 acc in
            acc
          | Ppat_variant (x0, x1) ->
            let acc = self#label x0 acc in
            let acc = self#option self#pattern x1 acc in
            acc
          | Ppat_record (x0, x1) ->
            let acc = self#list (fun (x0, x1) acc -> let acc = self#longident_loc x0 acc in let acc = self#pattern x1 acc in acc) x0 acc in
            let acc = self#closed_flag x1 acc in
            acc
          | Ppat_array x0 ->
            let acc = self#list self#pattern x0 acc in
            acc
          | Ppat_or (x0, x1) ->
            let acc = self#pattern x0 acc in
            let acc = self#pattern x1 acc in
            acc
          | Ppat_constraint (x0, x1) ->
            let acc = self#pattern x0 acc in
            let acc = self#core_type x1 acc in
            acc
          | Ppat_type x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Ppat_lazy x0 ->
            let acc = self#pattern x0 acc in
            acc
          | Ppat_unpack x0 ->
            let acc = self#loc self#string x0 acc in
            acc
          | Ppat_exception x0 ->
            let acc = self#pattern x0 acc in
            acc
          | Ppat_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Ppat_open (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#pattern x1 acc in
            acc
      method expression : Expression.t -> 'acc -> 'acc  =
        fun expression acc ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let acc = self#expression_desc pexp_desc acc in
          let acc = self#location pexp_loc acc in
          let acc = self#attributes pexp_attributes acc in
          acc
      method expression_desc : Expression_desc.t -> 'acc -> 'acc  =
        fun expression_desc acc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Pexp_constant x0 ->
            let acc = self#constant x0 acc in
            acc
          | Pexp_let (x0, x1, x2) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#value_binding x1 acc in
            let acc = self#expression x2 acc in
            acc
          | Pexp_function x0 ->
            let acc = self#list self#case x0 acc in
            acc
          | Pexp_fun (x0, x1, x2, x3) ->
            let acc = self#arg_label x0 acc in
            let acc = self#option self#expression x1 acc in
            let acc = self#pattern x2 acc in
            let acc = self#expression x3 acc in
            acc
          | Pexp_apply (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#list (fun (x0, x1) acc -> let acc = self#arg_label x0 acc in let acc = self#expression x1 acc in acc) x1 acc in
            acc
          | Pexp_match (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#list self#case x1 acc in
            acc
          | Pexp_try (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#list self#case x1 acc in
            acc
          | Pexp_tuple x0 ->
            let acc = self#list self#expression x0 acc in
            acc
          | Pexp_construct (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#option self#expression x1 acc in
            acc
          | Pexp_variant (x0, x1) ->
            let acc = self#label x0 acc in
            let acc = self#option self#expression x1 acc in
            acc
          | Pexp_record (x0, x1) ->
            let acc = self#list (fun (x0, x1) acc -> let acc = self#longident_loc x0 acc in let acc = self#expression x1 acc in acc) x0 acc in
            let acc = self#option self#expression x1 acc in
            acc
          | Pexp_field (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#longident_loc x1 acc in
            acc
          | Pexp_setfield (x0, x1, x2) ->
            let acc = self#expression x0 acc in
            let acc = self#longident_loc x1 acc in
            let acc = self#expression x2 acc in
            acc
          | Pexp_array x0 ->
            let acc = self#list self#expression x0 acc in
            acc
          | Pexp_ifthenelse (x0, x1, x2) ->
            let acc = self#expression x0 acc in
            let acc = self#expression x1 acc in
            let acc = self#option self#expression x2 acc in
            acc
          | Pexp_sequence (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_while (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let acc = self#pattern x0 acc in
            let acc = self#expression x1 acc in
            let acc = self#expression x2 acc in
            let acc = self#direction_flag x3 acc in
            let acc = self#expression x4 acc in
            acc
          | Pexp_constraint (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#core_type x1 acc in
            acc
          | Pexp_coerce (x0, x1, x2) ->
            let acc = self#expression x0 acc in
            let acc = self#option self#core_type x1 acc in
            let acc = self#core_type x2 acc in
            acc
          | Pexp_send (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#loc self#label x1 acc in
            acc
          | Pexp_new x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Pexp_setinstvar (x0, x1) ->
            let acc = self#loc self#label x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_override x0 ->
            let acc = self#list (fun (x0, x1) acc -> let acc = self#loc self#label x0 acc in let acc = self#expression x1 acc in acc) x0 acc in
            acc
          | Pexp_letmodule (x0, x1, x2) ->
            let acc = self#loc self#string x0 acc in
            let acc = self#module_expr x1 acc in
            let acc = self#expression x2 acc in
            acc
          | Pexp_letexception (x0, x1) ->
            let acc = self#extension_constructor x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_assert x0 ->
            let acc = self#expression x0 acc in
            acc
          | Pexp_lazy x0 ->
            let acc = self#expression x0 acc in
            acc
          | Pexp_poly (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#option self#core_type x1 acc in
            acc
          | Pexp_object x0 ->
            let acc = self#class_structure x0 acc in
            acc
          | Pexp_newtype (x0, x1) ->
            let acc = self#loc self#string x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_pack x0 ->
            let acc = self#module_expr x0 acc in
            acc
          | Pexp_open (x0, x1, x2) ->
            let acc = self#override_flag x0 acc in
            let acc = self#longident_loc x1 acc in
            let acc = self#expression x2 acc in
            acc
          | Pexp_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Pexp_unreachable ->
            acc
      method case : Case.t -> 'acc -> 'acc  =
        fun case acc ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let acc = self#pattern pc_lhs acc in
          let acc = self#option self#expression pc_guard acc in
          let acc = self#expression pc_rhs acc in
          acc
      method value_description : Value_description.t -> 'acc -> 'acc  =
        fun value_description acc ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let acc = self#loc self#string pval_name acc in
          let acc = self#core_type pval_type acc in
          let acc = self#list self#string pval_prim acc in
          let acc = self#attributes pval_attributes acc in
          let acc = self#location pval_loc acc in
          acc
      method type_declaration : Type_declaration.t -> 'acc -> 'acc  =
        fun type_declaration acc ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let acc = self#loc self#string ptype_name acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#variance x1 acc in acc) ptype_params acc in
          let acc = self#list (fun (x0, x1, x2) acc -> let acc = self#core_type x0 acc in let acc = self#core_type x1 acc in let acc = self#location x2 acc in acc) ptype_cstrs acc in
          let acc = self#type_kind ptype_kind acc in
          let acc = self#private_flag ptype_private acc in
          let acc = self#option self#core_type ptype_manifest acc in
          let acc = self#attributes ptype_attributes acc in
          let acc = self#location ptype_loc acc in
          acc
      method type_kind : Type_kind.t -> 'acc -> 'acc  =
        fun type_kind acc ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            acc
          | Ptype_variant x0 ->
            let acc = self#list self#constructor_declaration x0 acc in
            acc
          | Ptype_record x0 ->
            let acc = self#list self#label_declaration x0 acc in
            acc
          | Ptype_open ->
            acc
      method label_declaration : Label_declaration.t -> 'acc -> 'acc  =
        fun label_declaration acc ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let acc = self#loc self#string pld_name acc in
          let acc = self#mutable_flag pld_mutable acc in
          let acc = self#core_type pld_type acc in
          let acc = self#location pld_loc acc in
          let acc = self#attributes pld_attributes acc in
          acc
      method constructor_declaration : Constructor_declaration.t -> 'acc -> 'acc  =
        fun constructor_declaration acc ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let acc = self#loc self#string pcd_name acc in
          let acc = self#constructor_arguments pcd_args acc in
          let acc = self#option self#core_type pcd_res acc in
          let acc = self#location pcd_loc acc in
          let acc = self#attributes pcd_attributes acc in
          acc
      method constructor_arguments : Constructor_arguments.t -> 'acc -> 'acc  =
        fun constructor_arguments acc ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let acc = self#list self#core_type x0 acc in
            acc
          | Pcstr_record x0 ->
            let acc = self#list self#label_declaration x0 acc in
            acc
      method type_extension : Type_extension.t -> 'acc -> 'acc  =
        fun type_extension acc ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let acc = self#longident_loc ptyext_path acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#variance x1 acc in acc) ptyext_params acc in
          let acc = self#list self#extension_constructor ptyext_constructors acc in
          let acc = self#private_flag ptyext_private acc in
          let acc = self#attributes ptyext_attributes acc in
          acc
      method extension_constructor : Extension_constructor.t -> 'acc -> 'acc  =
        fun extension_constructor acc ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let acc = self#loc self#string pext_name acc in
          let acc = self#extension_constructor_kind pext_kind acc in
          let acc = self#location pext_loc acc in
          let acc = self#attributes pext_attributes acc in
          acc
      method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> 'acc  =
        fun extension_constructor_kind acc ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let acc = self#constructor_arguments x0 acc in
            let acc = self#option self#core_type x1 acc in
            acc
          | Pext_rebind x0 ->
            let acc = self#longident_loc x0 acc in
            acc
      method class_type : Class_type.t -> 'acc -> 'acc  =
        fun class_type acc ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let acc = self#class_type_desc pcty_desc acc in
          let acc = self#location pcty_loc acc in
          let acc = self#attributes pcty_attributes acc in
          acc
      method class_type_desc : Class_type_desc.t -> 'acc -> 'acc  =
        fun class_type_desc acc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#list self#core_type x1 acc in
            acc
          | Pcty_signature x0 ->
            let acc = self#class_signature x0 acc in
            acc
          | Pcty_arrow (x0, x1, x2) ->
            let acc = self#arg_label x0 acc in
            let acc = self#core_type x1 acc in
            let acc = self#class_type x2 acc in
            acc
          | Pcty_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Pcty_open (x0, x1, x2) ->
            let acc = self#override_flag x0 acc in
            let acc = self#longident_loc x1 acc in
            let acc = self#class_type x2 acc in
            acc
      method class_signature : Class_signature.t -> 'acc -> 'acc  =
        fun class_signature acc ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let acc = self#core_type pcsig_self acc in
          let acc = self#list self#class_type_field pcsig_fields acc in
          acc
      method class_type_field : Class_type_field.t -> 'acc -> 'acc  =
        fun class_type_field acc ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let acc = self#class_type_field_desc pctf_desc acc in
          let acc = self#location pctf_loc acc in
          let acc = self#attributes pctf_attributes acc in
          acc
      method class_type_field_desc : Class_type_field_desc.t -> 'acc -> 'acc  =
        fun class_type_field_desc acc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let acc = self#class_type x0 acc in
            acc
          | Pctf_val x0 ->
            let acc = (fun (x0, x1, x2, x3) acc -> let acc = self#loc self#label x0 acc in let acc = self#mutable_flag x1 acc in let acc = self#virtual_flag x2 acc in let acc = self#core_type x3 acc in acc) x0 acc in
            acc
          | Pctf_method x0 ->
            let acc = (fun (x0, x1, x2, x3) acc -> let acc = self#loc self#label x0 acc in let acc = self#private_flag x1 acc in let acc = self#virtual_flag x2 acc in let acc = self#core_type x3 acc in acc) x0 acc in
            acc
          | Pctf_constraint x0 ->
            let acc = (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#core_type x1 acc in acc) x0 acc in
            acc
          | Pctf_attribute x0 ->
            let acc = self#attribute x0 acc in
            acc
          | Pctf_extension x0 ->
            let acc = self#extension x0 acc in
            acc
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> 'acc  =
        fun class_infos acc ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let acc = self#virtual_flag pci_virt acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#variance x1 acc in acc) pci_params acc in
          let acc = self#loc self#string pci_name acc in
          let acc = self#class_expr pci_expr acc in
          let acc = self#location pci_loc acc in
          let acc = self#attributes pci_attributes acc in
          acc
      method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> 'acc  =
        fun class_infos acc ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let acc = self#virtual_flag pci_virt acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#variance x1 acc in acc) pci_params acc in
          let acc = self#loc self#string pci_name acc in
          let acc = self#class_type pci_expr acc in
          let acc = self#location pci_loc acc in
          let acc = self#attributes pci_attributes acc in
          acc
      method class_description : Class_description.t -> 'acc -> 'acc  =
        fun class_description acc ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let acc = self#class_infos_class_type concrete acc in
          acc
      method class_type_declaration : Class_type_declaration.t -> 'acc -> 'acc  =
        fun class_type_declaration acc ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let acc = self#class_infos_class_type concrete acc in
          acc
      method class_expr : Class_expr.t -> 'acc -> 'acc  =
        fun class_expr acc ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let acc = self#class_expr_desc pcl_desc acc in
          let acc = self#location pcl_loc acc in
          let acc = self#attributes pcl_attributes acc in
          acc
      method class_expr_desc : Class_expr_desc.t -> 'acc -> 'acc  =
        fun class_expr_desc acc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#list self#core_type x1 acc in
            acc
          | Pcl_structure x0 ->
            let acc = self#class_structure x0 acc in
            acc
          | Pcl_fun (x0, x1, x2, x3) ->
            let acc = self#arg_label x0 acc in
            let acc = self#option self#expression x1 acc in
            let acc = self#pattern x2 acc in
            let acc = self#class_expr x3 acc in
            acc
          | Pcl_apply (x0, x1) ->
            let acc = self#class_expr x0 acc in
            let acc = self#list (fun (x0, x1) acc -> let acc = self#arg_label x0 acc in let acc = self#expression x1 acc in acc) x1 acc in
            acc
          | Pcl_let (x0, x1, x2) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#value_binding x1 acc in
            let acc = self#class_expr x2 acc in
            acc
          | Pcl_constraint (x0, x1) ->
            let acc = self#class_expr x0 acc in
            let acc = self#class_type x1 acc in
            acc
          | Pcl_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Pcl_open (x0, x1, x2) ->
            let acc = self#override_flag x0 acc in
            let acc = self#longident_loc x1 acc in
            let acc = self#class_expr x2 acc in
            acc
      method class_structure : Class_structure.t -> 'acc -> 'acc  =
        fun class_structure acc ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let acc = self#pattern pcstr_self acc in
          let acc = self#list self#class_field pcstr_fields acc in
          acc
      method class_field : Class_field.t -> 'acc -> 'acc  =
        fun class_field acc ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let acc = self#class_field_desc pcf_desc acc in
          let acc = self#location pcf_loc acc in
          let acc = self#attributes pcf_attributes acc in
          acc
      method class_field_desc : Class_field_desc.t -> 'acc -> 'acc  =
        fun class_field_desc acc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let acc = self#override_flag x0 acc in
            let acc = self#class_expr x1 acc in
            let acc = self#option (self#loc self#string) x2 acc in
            acc
          | Pcf_val x0 ->
            let acc = (fun (x0, x1, x2) acc -> let acc = self#loc self#label x0 acc in let acc = self#mutable_flag x1 acc in let acc = self#class_field_kind x2 acc in acc) x0 acc in
            acc
          | Pcf_method x0 ->
            let acc = (fun (x0, x1, x2) acc -> let acc = self#loc self#label x0 acc in let acc = self#private_flag x1 acc in let acc = self#class_field_kind x2 acc in acc) x0 acc in
            acc
          | Pcf_constraint x0 ->
            let acc = (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#core_type x1 acc in acc) x0 acc in
            acc
          | Pcf_initializer x0 ->
            let acc = self#expression x0 acc in
            acc
          | Pcf_attribute x0 ->
            let acc = self#attribute x0 acc in
            acc
          | Pcf_extension x0 ->
            let acc = self#extension x0 acc in
            acc
      method class_field_kind : Class_field_kind.t -> 'acc -> 'acc  =
        fun class_field_kind acc ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let acc = self#core_type x0 acc in
            acc
          | Cfk_concrete (x0, x1) ->
            let acc = self#override_flag x0 acc in
            let acc = self#expression x1 acc in
            acc
      method class_declaration : Class_declaration.t -> 'acc -> 'acc  =
        fun class_declaration acc ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let acc = self#class_infos_class_expr concrete acc in
          acc
      method module_type : Module_type.t -> 'acc -> 'acc  =
        fun module_type acc ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let acc = self#module_type_desc pmty_desc acc in
          let acc = self#location pmty_loc acc in
          let acc = self#attributes pmty_attributes acc in
          acc
      method module_type_desc : Module_type_desc.t -> 'acc -> 'acc  =
        fun module_type_desc acc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Pmty_signature x0 ->
            let acc = self#signature x0 acc in
            acc
          | Pmty_functor (x0, x1, x2) ->
            let acc = self#loc self#string x0 acc in
            let acc = self#option self#module_type x1 acc in
            let acc = self#module_type x2 acc in
            acc
          | Pmty_with (x0, x1) ->
            let acc = self#module_type x0 acc in
            let acc = self#list self#with_constraint x1 acc in
            acc
          | Pmty_typeof x0 ->
            let acc = self#module_expr x0 acc in
            acc
          | Pmty_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Pmty_alias x0 ->
            let acc = self#longident_loc x0 acc in
            acc
      method signature : Signature.t -> 'acc -> 'acc  =
        fun signature acc ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let acc = self#list self#signature_item concrete acc in
          acc
      method signature_item : Signature_item.t -> 'acc -> 'acc  =
        fun signature_item acc ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let acc = self#signature_item_desc psig_desc acc in
          let acc = self#location psig_loc acc in
          acc
      method signature_item_desc : Signature_item_desc.t -> 'acc -> 'acc  =
        fun signature_item_desc acc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let acc = self#value_description x0 acc in
            acc
          | Psig_type (x0, x1) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#type_declaration x1 acc in
            acc
          | Psig_typext x0 ->
            let acc = self#type_extension x0 acc in
            acc
          | Psig_exception x0 ->
            let acc = self#extension_constructor x0 acc in
            acc
          | Psig_module x0 ->
            let acc = self#module_declaration x0 acc in
            acc
          | Psig_recmodule x0 ->
            let acc = self#list self#module_declaration x0 acc in
            acc
          | Psig_modtype x0 ->
            let acc = self#module_type_declaration x0 acc in
            acc
          | Psig_open x0 ->
            let acc = self#open_description x0 acc in
            acc
          | Psig_include x0 ->
            let acc = self#include_description x0 acc in
            acc
          | Psig_class x0 ->
            let acc = self#list self#class_description x0 acc in
            acc
          | Psig_class_type x0 ->
            let acc = self#list self#class_type_declaration x0 acc in
            acc
          | Psig_attribute x0 ->
            let acc = self#attribute x0 acc in
            acc
          | Psig_extension (x0, x1) ->
            let acc = self#extension x0 acc in
            let acc = self#attributes x1 acc in
            acc
      method module_declaration : Module_declaration.t -> 'acc -> 'acc  =
        fun module_declaration acc ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let acc = self#loc self#string pmd_name acc in
          let acc = self#module_type pmd_type acc in
          let acc = self#attributes pmd_attributes acc in
          let acc = self#location pmd_loc acc in
          acc
      method module_type_declaration : Module_type_declaration.t -> 'acc -> 'acc  =
        fun module_type_declaration acc ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let acc = self#loc self#string pmtd_name acc in
          let acc = self#option self#module_type pmtd_type acc in
          let acc = self#attributes pmtd_attributes acc in
          let acc = self#location pmtd_loc acc in
          acc
      method open_description : Open_description.t -> 'acc -> 'acc  =
        fun open_description acc ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let acc = self#longident_loc popen_lid acc in
          let acc = self#override_flag popen_override acc in
          let acc = self#location popen_loc acc in
          let acc = self#attributes popen_attributes acc in
          acc
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> 'acc  =
        fun include_infos acc ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let acc = self#module_expr pincl_mod acc in
          let acc = self#location pincl_loc acc in
          let acc = self#attributes pincl_attributes acc in
          acc
      method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> 'acc  =
        fun include_infos acc ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let acc = self#module_type pincl_mod acc in
          let acc = self#location pincl_loc acc in
          let acc = self#attributes pincl_attributes acc in
          acc
      method include_description : Include_description.t -> 'acc -> 'acc  =
        fun include_description acc ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let acc = self#include_infos_module_type concrete acc in
          acc
      method include_declaration : Include_declaration.t -> 'acc -> 'acc  =
        fun include_declaration acc ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let acc = self#include_infos_module_expr concrete acc in
          acc
      method with_constraint : With_constraint.t -> 'acc -> 'acc  =
        fun with_constraint acc ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#type_declaration x1 acc in
            acc
          | Pwith_module (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#longident_loc x1 acc in
            acc
          | Pwith_typesubst (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#type_declaration x1 acc in
            acc
          | Pwith_modsubst (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#longident_loc x1 acc in
            acc
      method module_expr : Module_expr.t -> 'acc -> 'acc  =
        fun module_expr acc ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let acc = self#module_expr_desc pmod_desc acc in
          let acc = self#location pmod_loc acc in
          let acc = self#attributes pmod_attributes acc in
          acc
      method module_expr_desc : Module_expr_desc.t -> 'acc -> 'acc  =
        fun module_expr_desc acc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Pmod_structure x0 ->
            let acc = self#structure x0 acc in
            acc
          | Pmod_functor (x0, x1, x2) ->
            let acc = self#loc self#string x0 acc in
            let acc = self#option self#module_type x1 acc in
            let acc = self#module_expr x2 acc in
            acc
          | Pmod_apply (x0, x1) ->
            let acc = self#module_expr x0 acc in
            let acc = self#module_expr x1 acc in
            acc
          | Pmod_constraint (x0, x1) ->
            let acc = self#module_expr x0 acc in
            let acc = self#module_type x1 acc in
            acc
          | Pmod_unpack x0 ->
            let acc = self#expression x0 acc in
            acc
          | Pmod_extension x0 ->
            let acc = self#extension x0 acc in
            acc
      method structure : Structure.t -> 'acc -> 'acc  =
        fun structure acc ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let acc = self#list self#structure_item concrete acc in
          acc
      method structure_item : Structure_item.t -> 'acc -> 'acc  =
        fun structure_item acc ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let acc = self#structure_item_desc pstr_desc acc in
          let acc = self#location pstr_loc acc in
          acc
      method structure_item_desc : Structure_item_desc.t -> 'acc -> 'acc  =
        fun structure_item_desc acc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#attributes x1 acc in
            acc
          | Pstr_value (x0, x1) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#value_binding x1 acc in
            acc
          | Pstr_primitive x0 ->
            let acc = self#value_description x0 acc in
            acc
          | Pstr_type (x0, x1) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#type_declaration x1 acc in
            acc
          | Pstr_typext x0 ->
            let acc = self#type_extension x0 acc in
            acc
          | Pstr_exception x0 ->
            let acc = self#extension_constructor x0 acc in
            acc
          | Pstr_module x0 ->
            let acc = self#module_binding x0 acc in
            acc
          | Pstr_recmodule x0 ->
            let acc = self#list self#module_binding x0 acc in
            acc
          | Pstr_modtype x0 ->
            let acc = self#module_type_declaration x0 acc in
            acc
          | Pstr_open x0 ->
            let acc = self#open_description x0 acc in
            acc
          | Pstr_class x0 ->
            let acc = self#list self#class_declaration x0 acc in
            acc
          | Pstr_class_type x0 ->
            let acc = self#list self#class_type_declaration x0 acc in
            acc
          | Pstr_include x0 ->
            let acc = self#include_declaration x0 acc in
            acc
          | Pstr_attribute x0 ->
            let acc = self#attribute x0 acc in
            acc
          | Pstr_extension (x0, x1) ->
            let acc = self#extension x0 acc in
            let acc = self#attributes x1 acc in
            acc
      method value_binding : Value_binding.t -> 'acc -> 'acc  =
        fun value_binding acc ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let acc = self#pattern pvb_pat acc in
          let acc = self#expression pvb_expr acc in
          let acc = self#attributes pvb_attributes acc in
          let acc = self#location pvb_loc acc in
          acc
      method module_binding : Module_binding.t -> 'acc -> 'acc  =
        fun module_binding acc ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let acc = self#loc self#string pmb_name acc in
          let acc = self#module_expr pmb_expr acc in
          let acc = self#attributes pmb_attributes acc in
          let acc = self#location pmb_loc acc in
          acc
      method toplevel_phrase : Toplevel_phrase.t -> 'acc -> 'acc  =
        fun toplevel_phrase acc ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let acc = self#structure x0 acc in
            acc
          | Ptop_dir (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#directive_argument x1 acc in
            acc
      method directive_argument : Directive_argument.t -> 'acc -> 'acc  =
        fun directive_argument acc ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            acc
          | Pdir_string x0 ->
            let acc = self#string x0 acc in
            acc
          | Pdir_int (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#option self#char x1 acc in
            acc
          | Pdir_ident x0 ->
            let acc = self#longident x0 acc in
            acc
          | Pdir_bool x0 ->
            let acc = self#bool x0 acc in
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
      method longident : Longident.t -> 'acc -> (Longident.t * 'acc)  =
        fun longident acc ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let (x0, acc) = self#string x0 acc in
            (Longident.of_concrete (Lident x0), acc)
          | Ldot (x0, x1) ->
            let (x0, acc) = self#longident x0 acc in
            let (x1, acc) = self#string x1 acc in
            (Longident.of_concrete (Ldot (x0, x1)), acc)
          | Lapply (x0, x1) ->
            let (x0, acc) = self#longident x0 acc in
            let (x1, acc) = self#longident x1 acc in
            (Longident.of_concrete (Lapply (x0, x1)), acc)
      method longident_loc : Longident_loc.t -> 'acc -> (Longident_loc.t * 'acc)  =
        fun longident_loc acc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let (concrete, acc) = self#loc self#longident concrete acc in
          (Longident_loc.of_concrete concrete, acc)
      method rec_flag : Rec_flag.t -> 'acc -> (Rec_flag.t * 'acc)  =
        fun rec_flag acc ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            (Rec_flag.of_concrete Nonrecursive, acc)
          | Recursive ->
            (Rec_flag.of_concrete Recursive, acc)
      method direction_flag : Direction_flag.t -> 'acc -> (Direction_flag.t * 'acc)  =
        fun direction_flag acc ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            (Direction_flag.of_concrete Upto, acc)
          | Downto ->
            (Direction_flag.of_concrete Downto, acc)
      method private_flag : Private_flag.t -> 'acc -> (Private_flag.t * 'acc)  =
        fun private_flag acc ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            (Private_flag.of_concrete Private, acc)
          | Public ->
            (Private_flag.of_concrete Public, acc)
      method mutable_flag : Mutable_flag.t -> 'acc -> (Mutable_flag.t * 'acc)  =
        fun mutable_flag acc ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            (Mutable_flag.of_concrete Immutable, acc)
          | Mutable ->
            (Mutable_flag.of_concrete Mutable, acc)
      method virtual_flag : Virtual_flag.t -> 'acc -> (Virtual_flag.t * 'acc)  =
        fun virtual_flag acc ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            (Virtual_flag.of_concrete Virtual, acc)
          | Concrete ->
            (Virtual_flag.of_concrete Concrete, acc)
      method override_flag : Override_flag.t -> 'acc -> (Override_flag.t * 'acc)  =
        fun override_flag acc ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            (Override_flag.of_concrete Override, acc)
          | Fresh ->
            (Override_flag.of_concrete Fresh, acc)
      method closed_flag : Closed_flag.t -> 'acc -> (Closed_flag.t * 'acc)  =
        fun closed_flag acc ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            (Closed_flag.of_concrete Closed, acc)
          | Open ->
            (Closed_flag.of_concrete Open, acc)
      method label : Label.t -> 'acc -> (Label.t * 'acc)  =
        fun label acc ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let (concrete, acc) = self#string concrete acc in
          (Label.of_concrete concrete, acc)
      method arg_label : Arg_label.t -> 'acc -> (Arg_label.t * 'acc)  =
        fun arg_label acc ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            (Arg_label.of_concrete Nolabel, acc)
          | Labelled x0 ->
            let (x0, acc) = self#string x0 acc in
            (Arg_label.of_concrete (Labelled x0), acc)
          | Optional x0 ->
            let (x0, acc) = self#string x0 acc in
            (Arg_label.of_concrete (Optional x0), acc)
      method variance : Variance.t -> 'acc -> (Variance.t * 'acc)  =
        fun variance acc ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            (Variance.of_concrete Covariant, acc)
          | Contravariant ->
            (Variance.of_concrete Contravariant, acc)
          | Invariant ->
            (Variance.of_concrete Invariant, acc)
      method constant : Constant.t -> 'acc -> (Constant.t * 'acc)  =
        fun constant acc ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#option self#char x1 acc in
            (Constant.of_concrete (Pconst_integer (x0, x1)), acc)
          | Pconst_char x0 ->
            let (x0, acc) = self#char x0 acc in
            (Constant.of_concrete (Pconst_char x0), acc)
          | Pconst_string (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#option self#string x1 acc in
            (Constant.of_concrete (Pconst_string (x0, x1)), acc)
          | Pconst_float (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#option self#char x1 acc in
            (Constant.of_concrete (Pconst_float (x0, x1)), acc)
      method attribute : Attribute.t -> 'acc -> (Attribute.t * 'acc)  =
        fun attribute acc ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let (x0, acc) = self#loc self#string x0 acc in
          let (x1, acc) = self#payload x1 acc in
          (Attribute.of_concrete (x0, x1), acc)
      method extension : Extension.t -> 'acc -> (Extension.t * 'acc)  =
        fun extension acc ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let (x0, acc) = self#loc self#string x0 acc in
          let (x1, acc) = self#payload x1 acc in
          (Extension.of_concrete (x0, x1), acc)
      method attributes : Attributes.t -> 'acc -> (Attributes.t * 'acc)  =
        fun attributes acc ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let (concrete, acc) = self#list self#attribute concrete acc in
          (Attributes.of_concrete concrete, acc)
      method payload : Payload.t -> 'acc -> (Payload.t * 'acc)  =
        fun payload acc ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let (x0, acc) = self#structure x0 acc in
            (Payload.of_concrete (PStr x0), acc)
          | PSig x0 ->
            let (x0, acc) = self#signature x0 acc in
            (Payload.of_concrete (PSig x0), acc)
          | PTyp x0 ->
            let (x0, acc) = self#core_type x0 acc in
            (Payload.of_concrete (PTyp x0), acc)
          | PPat (x0, x1) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            (Payload.of_concrete (PPat (x0, x1)), acc)
      method core_type : Core_type.t -> 'acc -> (Core_type.t * 'acc)  =
        fun core_type acc ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let (ptyp_desc, acc) = self#core_type_desc ptyp_desc acc in
          let (ptyp_loc, acc) = self#location ptyp_loc acc in
          let (ptyp_attributes, acc) = self#attributes ptyp_attributes acc in
          (Core_type.of_concrete { ptyp_desc; ptyp_loc; ptyp_attributes }, acc)
      method core_type_desc : Core_type_desc.t -> 'acc -> (Core_type_desc.t * 'acc)  =
        fun core_type_desc acc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            (Core_type_desc.of_concrete Ptyp_any, acc)
          | Ptyp_var x0 ->
            let (x0, acc) = self#string x0 acc in
            (Core_type_desc.of_concrete (Ptyp_var x0), acc)
          | Ptyp_arrow (x0, x1, x2) ->
            let (x0, acc) = self#arg_label x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            let (x2, acc) = self#core_type x2 acc in
            (Core_type_desc.of_concrete (Ptyp_arrow (x0, x1, x2)), acc)
          | Ptyp_tuple x0 ->
            let (x0, acc) = self#list self#core_type x0 acc in
            (Core_type_desc.of_concrete (Ptyp_tuple x0), acc)
          | Ptyp_constr (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#list self#core_type x1 acc in
            (Core_type_desc.of_concrete (Ptyp_constr (x0, x1)), acc)
          | Ptyp_object (x0, x1) ->
            let (x0, acc) = self#list self#object_field x0 acc in
            let (x1, acc) = self#closed_flag x1 acc in
            (Core_type_desc.of_concrete (Ptyp_object (x0, x1)), acc)
          | Ptyp_class (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#list self#core_type x1 acc in
            (Core_type_desc.of_concrete (Ptyp_class (x0, x1)), acc)
          | Ptyp_alias (x0, x1) ->
            let (x0, acc) = self#core_type x0 acc in
            let (x1, acc) = self#string x1 acc in
            (Core_type_desc.of_concrete (Ptyp_alias (x0, x1)), acc)
          | Ptyp_variant (x0, x1, x2) ->
            let (x0, acc) = self#list self#row_field x0 acc in
            let (x1, acc) = self#closed_flag x1 acc in
            let (x2, acc) = self#option (self#list self#label) x2 acc in
            (Core_type_desc.of_concrete (Ptyp_variant (x0, x1, x2)), acc)
          | Ptyp_poly (x0, x1) ->
            let (x0, acc) = self#list (self#loc self#string) x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            (Core_type_desc.of_concrete (Ptyp_poly (x0, x1)), acc)
          | Ptyp_package x0 ->
            let (x0, acc) = self#package_type x0 acc in
            (Core_type_desc.of_concrete (Ptyp_package x0), acc)
          | Ptyp_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Core_type_desc.of_concrete (Ptyp_extension x0), acc)
      method package_type : Package_type.t -> 'acc -> (Package_type.t * 'acc)  =
        fun package_type acc ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let (x0, acc) = self#longident_loc x0 acc in
          let (x1, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#longident_loc x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) x1 acc in
          (Package_type.of_concrete (x0, x1), acc)
      method row_field : Row_field.t -> 'acc -> (Row_field.t * 'acc)  =
        fun row_field acc ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let (x0, acc) = self#loc self#label x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            let (x2, acc) = self#bool x2 acc in
            let (x3, acc) = self#list self#core_type x3 acc in
            (Row_field.of_concrete (Rtag (x0, x1, x2, x3)), acc)
          | Rinherit x0 ->
            let (x0, acc) = self#core_type x0 acc in
            (Row_field.of_concrete (Rinherit x0), acc)
      method object_field : Object_field.t -> 'acc -> (Object_field.t * 'acc)  =
        fun object_field acc ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let (x0, acc) = self#loc self#label x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            let (x2, acc) = self#core_type x2 acc in
            (Object_field.of_concrete (Otag (x0, x1, x2)), acc)
          | Oinherit x0 ->
            let (x0, acc) = self#core_type x0 acc in
            (Object_field.of_concrete (Oinherit x0), acc)
      method pattern : Pattern.t -> 'acc -> (Pattern.t * 'acc)  =
        fun pattern acc ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let (ppat_desc, acc) = self#pattern_desc ppat_desc acc in
          let (ppat_loc, acc) = self#location ppat_loc acc in
          let (ppat_attributes, acc) = self#attributes ppat_attributes acc in
          (Pattern.of_concrete { ppat_desc; ppat_loc; ppat_attributes }, acc)
      method pattern_desc : Pattern_desc.t -> 'acc -> (Pattern_desc.t * 'acc)  =
        fun pattern_desc acc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            (Pattern_desc.of_concrete Ppat_any, acc)
          | Ppat_var x0 ->
            let (x0, acc) = self#loc self#string x0 acc in
            (Pattern_desc.of_concrete (Ppat_var x0), acc)
          | Ppat_alias (x0, x1) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#loc self#string x1 acc in
            (Pattern_desc.of_concrete (Ppat_alias (x0, x1)), acc)
          | Ppat_constant x0 ->
            let (x0, acc) = self#constant x0 acc in
            (Pattern_desc.of_concrete (Ppat_constant x0), acc)
          | Ppat_interval (x0, x1) ->
            let (x0, acc) = self#constant x0 acc in
            let (x1, acc) = self#constant x1 acc in
            (Pattern_desc.of_concrete (Ppat_interval (x0, x1)), acc)
          | Ppat_tuple x0 ->
            let (x0, acc) = self#list self#pattern x0 acc in
            (Pattern_desc.of_concrete (Ppat_tuple x0), acc)
          | Ppat_construct (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#option self#pattern x1 acc in
            (Pattern_desc.of_concrete (Ppat_construct (x0, x1)), acc)
          | Ppat_variant (x0, x1) ->
            let (x0, acc) = self#label x0 acc in
            let (x1, acc) = self#option self#pattern x1 acc in
            (Pattern_desc.of_concrete (Ppat_variant (x0, x1)), acc)
          | Ppat_record (x0, x1) ->
            let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#longident_loc x0 acc in let (x1, acc) = self#pattern x1 acc in ((x0, x1), acc)) x0 acc in
            let (x1, acc) = self#closed_flag x1 acc in
            (Pattern_desc.of_concrete (Ppat_record (x0, x1)), acc)
          | Ppat_array x0 ->
            let (x0, acc) = self#list self#pattern x0 acc in
            (Pattern_desc.of_concrete (Ppat_array x0), acc)
          | Ppat_or (x0, x1) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#pattern x1 acc in
            (Pattern_desc.of_concrete (Ppat_or (x0, x1)), acc)
          | Ppat_constraint (x0, x1) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            (Pattern_desc.of_concrete (Ppat_constraint (x0, x1)), acc)
          | Ppat_type x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Pattern_desc.of_concrete (Ppat_type x0), acc)
          | Ppat_lazy x0 ->
            let (x0, acc) = self#pattern x0 acc in
            (Pattern_desc.of_concrete (Ppat_lazy x0), acc)
          | Ppat_unpack x0 ->
            let (x0, acc) = self#loc self#string x0 acc in
            (Pattern_desc.of_concrete (Ppat_unpack x0), acc)
          | Ppat_exception x0 ->
            let (x0, acc) = self#pattern x0 acc in
            (Pattern_desc.of_concrete (Ppat_exception x0), acc)
          | Ppat_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Pattern_desc.of_concrete (Ppat_extension x0), acc)
          | Ppat_open (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#pattern x1 acc in
            (Pattern_desc.of_concrete (Ppat_open (x0, x1)), acc)
      method expression : Expression.t -> 'acc -> (Expression.t * 'acc)  =
        fun expression acc ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let (pexp_desc, acc) = self#expression_desc pexp_desc acc in
          let (pexp_loc, acc) = self#location pexp_loc acc in
          let (pexp_attributes, acc) = self#attributes pexp_attributes acc in
          (Expression.of_concrete { pexp_desc; pexp_loc; pexp_attributes }, acc)
      method expression_desc : Expression_desc.t -> 'acc -> (Expression_desc.t * 'acc)  =
        fun expression_desc acc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Expression_desc.of_concrete (Pexp_ident x0), acc)
          | Pexp_constant x0 ->
            let (x0, acc) = self#constant x0 acc in
            (Expression_desc.of_concrete (Pexp_constant x0), acc)
          | Pexp_let (x0, x1, x2) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#value_binding x1 acc in
            let (x2, acc) = self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_let (x0, x1, x2)), acc)
          | Pexp_function x0 ->
            let (x0, acc) = self#list self#case x0 acc in
            (Expression_desc.of_concrete (Pexp_function x0), acc)
          | Pexp_fun (x0, x1, x2, x3) ->
            let (x0, acc) = self#arg_label x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            let (x2, acc) = self#pattern x2 acc in
            let (x3, acc) = self#expression x3 acc in
            (Expression_desc.of_concrete (Pexp_fun (x0, x1, x2, x3)), acc)
          | Pexp_apply (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#arg_label x0 acc in let (x1, acc) = self#expression x1 acc in ((x0, x1), acc)) x1 acc in
            (Expression_desc.of_concrete (Pexp_apply (x0, x1)), acc)
          | Pexp_match (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#list self#case x1 acc in
            (Expression_desc.of_concrete (Pexp_match (x0, x1)), acc)
          | Pexp_try (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#list self#case x1 acc in
            (Expression_desc.of_concrete (Pexp_try (x0, x1)), acc)
          | Pexp_tuple x0 ->
            let (x0, acc) = self#list self#expression x0 acc in
            (Expression_desc.of_concrete (Pexp_tuple x0), acc)
          | Pexp_construct (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_construct (x0, x1)), acc)
          | Pexp_variant (x0, x1) ->
            let (x0, acc) = self#label x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_variant (x0, x1)), acc)
          | Pexp_record (x0, x1) ->
            let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#longident_loc x0 acc in let (x1, acc) = self#expression x1 acc in ((x0, x1), acc)) x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_record (x0, x1)), acc)
          | Pexp_field (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            (Expression_desc.of_concrete (Pexp_field (x0, x1)), acc)
          | Pexp_setfield (x0, x1, x2) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            let (x2, acc) = self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_setfield (x0, x1, x2)), acc)
          | Pexp_array x0 ->
            let (x0, acc) = self#list self#expression x0 acc in
            (Expression_desc.of_concrete (Pexp_array x0), acc)
          | Pexp_ifthenelse (x0, x1, x2) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#expression x1 acc in
            let (x2, acc) = self#option self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_ifthenelse (x0, x1, x2)), acc)
          | Pexp_sequence (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_sequence (x0, x1)), acc)
          | Pexp_while (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_while (x0, x1)), acc)
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#expression x1 acc in
            let (x2, acc) = self#expression x2 acc in
            let (x3, acc) = self#direction_flag x3 acc in
            let (x4, acc) = self#expression x4 acc in
            (Expression_desc.of_concrete (Pexp_for (x0, x1, x2, x3, x4)), acc)
          | Pexp_constraint (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            (Expression_desc.of_concrete (Pexp_constraint (x0, x1)), acc)
          | Pexp_coerce (x0, x1, x2) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#option self#core_type x1 acc in
            let (x2, acc) = self#core_type x2 acc in
            (Expression_desc.of_concrete (Pexp_coerce (x0, x1, x2)), acc)
          | Pexp_send (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#loc self#label x1 acc in
            (Expression_desc.of_concrete (Pexp_send (x0, x1)), acc)
          | Pexp_new x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Expression_desc.of_concrete (Pexp_new x0), acc)
          | Pexp_setinstvar (x0, x1) ->
            let (x0, acc) = self#loc self#label x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_setinstvar (x0, x1)), acc)
          | Pexp_override x0 ->
            let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#expression x1 acc in ((x0, x1), acc)) x0 acc in
            (Expression_desc.of_concrete (Pexp_override x0), acc)
          | Pexp_letmodule (x0, x1, x2) ->
            let (x0, acc) = self#loc self#string x0 acc in
            let (x1, acc) = self#module_expr x1 acc in
            let (x2, acc) = self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_letmodule (x0, x1, x2)), acc)
          | Pexp_letexception (x0, x1) ->
            let (x0, acc) = self#extension_constructor x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_letexception (x0, x1)), acc)
          | Pexp_assert x0 ->
            let (x0, acc) = self#expression x0 acc in
            (Expression_desc.of_concrete (Pexp_assert x0), acc)
          | Pexp_lazy x0 ->
            let (x0, acc) = self#expression x0 acc in
            (Expression_desc.of_concrete (Pexp_lazy x0), acc)
          | Pexp_poly (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#option self#core_type x1 acc in
            (Expression_desc.of_concrete (Pexp_poly (x0, x1)), acc)
          | Pexp_object x0 ->
            let (x0, acc) = self#class_structure x0 acc in
            (Expression_desc.of_concrete (Pexp_object x0), acc)
          | Pexp_newtype (x0, x1) ->
            let (x0, acc) = self#loc self#string x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_newtype (x0, x1)), acc)
          | Pexp_pack x0 ->
            let (x0, acc) = self#module_expr x0 acc in
            (Expression_desc.of_concrete (Pexp_pack x0), acc)
          | Pexp_open (x0, x1, x2) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            let (x2, acc) = self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_open (x0, x1, x2)), acc)
          | Pexp_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Expression_desc.of_concrete (Pexp_extension x0), acc)
          | Pexp_unreachable ->
            (Expression_desc.of_concrete Pexp_unreachable, acc)
      method case : Case.t -> 'acc -> (Case.t * 'acc)  =
        fun case acc ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let (pc_lhs, acc) = self#pattern pc_lhs acc in
          let (pc_guard, acc) = self#option self#expression pc_guard acc in
          let (pc_rhs, acc) = self#expression pc_rhs acc in
          (Case.of_concrete { pc_lhs; pc_guard; pc_rhs }, acc)
      method value_description : Value_description.t -> 'acc -> (Value_description.t * 'acc)  =
        fun value_description acc ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let (pval_name, acc) = self#loc self#string pval_name acc in
          let (pval_type, acc) = self#core_type pval_type acc in
          let (pval_prim, acc) = self#list self#string pval_prim acc in
          let (pval_attributes, acc) = self#attributes pval_attributes acc in
          let (pval_loc, acc) = self#location pval_loc acc in
          (Value_description.of_concrete { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }, acc)
      method type_declaration : Type_declaration.t -> 'acc -> (Type_declaration.t * 'acc)  =
        fun type_declaration acc ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let (ptype_name, acc) = self#loc self#string ptype_name acc in
          let (ptype_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#variance x1 acc in ((x0, x1), acc)) ptype_params acc in
          let (ptype_cstrs, acc) = self#list (fun (x0, x1, x2) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#core_type x1 acc in let (x2, acc) = self#location x2 acc in ((x0, x1, x2), acc)) ptype_cstrs acc in
          let (ptype_kind, acc) = self#type_kind ptype_kind acc in
          let (ptype_private, acc) = self#private_flag ptype_private acc in
          let (ptype_manifest, acc) = self#option self#core_type ptype_manifest acc in
          let (ptype_attributes, acc) = self#attributes ptype_attributes acc in
          let (ptype_loc, acc) = self#location ptype_loc acc in
          (Type_declaration.of_concrete { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }, acc)
      method type_kind : Type_kind.t -> 'acc -> (Type_kind.t * 'acc)  =
        fun type_kind acc ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            (Type_kind.of_concrete Ptype_abstract, acc)
          | Ptype_variant x0 ->
            let (x0, acc) = self#list self#constructor_declaration x0 acc in
            (Type_kind.of_concrete (Ptype_variant x0), acc)
          | Ptype_record x0 ->
            let (x0, acc) = self#list self#label_declaration x0 acc in
            (Type_kind.of_concrete (Ptype_record x0), acc)
          | Ptype_open ->
            (Type_kind.of_concrete Ptype_open, acc)
      method label_declaration : Label_declaration.t -> 'acc -> (Label_declaration.t * 'acc)  =
        fun label_declaration acc ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let (pld_name, acc) = self#loc self#string pld_name acc in
          let (pld_mutable, acc) = self#mutable_flag pld_mutable acc in
          let (pld_type, acc) = self#core_type pld_type acc in
          let (pld_loc, acc) = self#location pld_loc acc in
          let (pld_attributes, acc) = self#attributes pld_attributes acc in
          (Label_declaration.of_concrete { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }, acc)
      method constructor_declaration : Constructor_declaration.t -> 'acc -> (Constructor_declaration.t * 'acc)  =
        fun constructor_declaration acc ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let (pcd_name, acc) = self#loc self#string pcd_name acc in
          let (pcd_args, acc) = self#constructor_arguments pcd_args acc in
          let (pcd_res, acc) = self#option self#core_type pcd_res acc in
          let (pcd_loc, acc) = self#location pcd_loc acc in
          let (pcd_attributes, acc) = self#attributes pcd_attributes acc in
          (Constructor_declaration.of_concrete { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }, acc)
      method constructor_arguments : Constructor_arguments.t -> 'acc -> (Constructor_arguments.t * 'acc)  =
        fun constructor_arguments acc ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let (x0, acc) = self#list self#core_type x0 acc in
            (Constructor_arguments.of_concrete (Pcstr_tuple x0), acc)
          | Pcstr_record x0 ->
            let (x0, acc) = self#list self#label_declaration x0 acc in
            (Constructor_arguments.of_concrete (Pcstr_record x0), acc)
      method type_extension : Type_extension.t -> 'acc -> (Type_extension.t * 'acc)  =
        fun type_extension acc ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let (ptyext_path, acc) = self#longident_loc ptyext_path acc in
          let (ptyext_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#variance x1 acc in ((x0, x1), acc)) ptyext_params acc in
          let (ptyext_constructors, acc) = self#list self#extension_constructor ptyext_constructors acc in
          let (ptyext_private, acc) = self#private_flag ptyext_private acc in
          let (ptyext_attributes, acc) = self#attributes ptyext_attributes acc in
          (Type_extension.of_concrete { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }, acc)
      method extension_constructor : Extension_constructor.t -> 'acc -> (Extension_constructor.t * 'acc)  =
        fun extension_constructor acc ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let (pext_name, acc) = self#loc self#string pext_name acc in
          let (pext_kind, acc) = self#extension_constructor_kind pext_kind acc in
          let (pext_loc, acc) = self#location pext_loc acc in
          let (pext_attributes, acc) = self#attributes pext_attributes acc in
          (Extension_constructor.of_concrete { pext_name; pext_kind; pext_loc; pext_attributes }, acc)
      method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> (Extension_constructor_kind.t * 'acc)  =
        fun extension_constructor_kind acc ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let (x0, acc) = self#constructor_arguments x0 acc in
            let (x1, acc) = self#option self#core_type x1 acc in
            (Extension_constructor_kind.of_concrete (Pext_decl (x0, x1)), acc)
          | Pext_rebind x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Extension_constructor_kind.of_concrete (Pext_rebind x0), acc)
      method class_type : Class_type.t -> 'acc -> (Class_type.t * 'acc)  =
        fun class_type acc ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let (pcty_desc, acc) = self#class_type_desc pcty_desc acc in
          let (pcty_loc, acc) = self#location pcty_loc acc in
          let (pcty_attributes, acc) = self#attributes pcty_attributes acc in
          (Class_type.of_concrete { pcty_desc; pcty_loc; pcty_attributes }, acc)
      method class_type_desc : Class_type_desc.t -> 'acc -> (Class_type_desc.t * 'acc)  =
        fun class_type_desc acc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#list self#core_type x1 acc in
            (Class_type_desc.of_concrete (Pcty_constr (x0, x1)), acc)
          | Pcty_signature x0 ->
            let (x0, acc) = self#class_signature x0 acc in
            (Class_type_desc.of_concrete (Pcty_signature x0), acc)
          | Pcty_arrow (x0, x1, x2) ->
            let (x0, acc) = self#arg_label x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            let (x2, acc) = self#class_type x2 acc in
            (Class_type_desc.of_concrete (Pcty_arrow (x0, x1, x2)), acc)
          | Pcty_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Class_type_desc.of_concrete (Pcty_extension x0), acc)
          | Pcty_open (x0, x1, x2) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            let (x2, acc) = self#class_type x2 acc in
            (Class_type_desc.of_concrete (Pcty_open (x0, x1, x2)), acc)
      method class_signature : Class_signature.t -> 'acc -> (Class_signature.t * 'acc)  =
        fun class_signature acc ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let (pcsig_self, acc) = self#core_type pcsig_self acc in
          let (pcsig_fields, acc) = self#list self#class_type_field pcsig_fields acc in
          (Class_signature.of_concrete { pcsig_self; pcsig_fields }, acc)
      method class_type_field : Class_type_field.t -> 'acc -> (Class_type_field.t * 'acc)  =
        fun class_type_field acc ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let (pctf_desc, acc) = self#class_type_field_desc pctf_desc acc in
          let (pctf_loc, acc) = self#location pctf_loc acc in
          let (pctf_attributes, acc) = self#attributes pctf_attributes acc in
          (Class_type_field.of_concrete { pctf_desc; pctf_loc; pctf_attributes }, acc)
      method class_type_field_desc : Class_type_field_desc.t -> 'acc -> (Class_type_field_desc.t * 'acc)  =
        fun class_type_field_desc acc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let (x0, acc) = self#class_type x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_inherit x0), acc)
          | Pctf_val x0 ->
            let (x0, acc) = (fun (x0, x1, x2, x3) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#mutable_flag x1 acc in let (x2, acc) = self#virtual_flag x2 acc in let (x3, acc) = self#core_type x3 acc in ((x0, x1, x2, x3), acc)) x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_val x0), acc)
          | Pctf_method x0 ->
            let (x0, acc) = (fun (x0, x1, x2, x3) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#private_flag x1 acc in let (x2, acc) = self#virtual_flag x2 acc in let (x3, acc) = self#core_type x3 acc in ((x0, x1, x2, x3), acc)) x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_method x0), acc)
          | Pctf_constraint x0 ->
            let (x0, acc) = (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_constraint x0), acc)
          | Pctf_attribute x0 ->
            let (x0, acc) = self#attribute x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_attribute x0), acc)
          | Pctf_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_extension x0), acc)
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> (Class_expr.t Class_infos.t * 'acc)  =
        fun class_infos acc ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let (pci_virt, acc) = self#virtual_flag pci_virt acc in
          let (pci_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#variance x1 acc in ((x0, x1), acc)) pci_params acc in
          let (pci_name, acc) = self#loc self#string pci_name acc in
          let (pci_expr, acc) = self#class_expr pci_expr acc in
          let (pci_loc, acc) = self#location pci_loc acc in
          let (pci_attributes, acc) = self#attributes pci_attributes acc in
          (Class_infos.of_concrete_class_expr { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }, acc)
      method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> (Class_type.t Class_infos.t * 'acc)  =
        fun class_infos acc ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let (pci_virt, acc) = self#virtual_flag pci_virt acc in
          let (pci_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#variance x1 acc in ((x0, x1), acc)) pci_params acc in
          let (pci_name, acc) = self#loc self#string pci_name acc in
          let (pci_expr, acc) = self#class_type pci_expr acc in
          let (pci_loc, acc) = self#location pci_loc acc in
          let (pci_attributes, acc) = self#attributes pci_attributes acc in
          (Class_infos.of_concrete_class_type { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }, acc)
      method class_description : Class_description.t -> 'acc -> (Class_description.t * 'acc)  =
        fun class_description acc ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let (concrete, acc) = self#class_infos_class_type concrete acc in
          (Class_description.of_concrete concrete, acc)
      method class_type_declaration : Class_type_declaration.t -> 'acc -> (Class_type_declaration.t * 'acc)  =
        fun class_type_declaration acc ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let (concrete, acc) = self#class_infos_class_type concrete acc in
          (Class_type_declaration.of_concrete concrete, acc)
      method class_expr : Class_expr.t -> 'acc -> (Class_expr.t * 'acc)  =
        fun class_expr acc ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let (pcl_desc, acc) = self#class_expr_desc pcl_desc acc in
          let (pcl_loc, acc) = self#location pcl_loc acc in
          let (pcl_attributes, acc) = self#attributes pcl_attributes acc in
          (Class_expr.of_concrete { pcl_desc; pcl_loc; pcl_attributes }, acc)
      method class_expr_desc : Class_expr_desc.t -> 'acc -> (Class_expr_desc.t * 'acc)  =
        fun class_expr_desc acc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#list self#core_type x1 acc in
            (Class_expr_desc.of_concrete (Pcl_constr (x0, x1)), acc)
          | Pcl_structure x0 ->
            let (x0, acc) = self#class_structure x0 acc in
            (Class_expr_desc.of_concrete (Pcl_structure x0), acc)
          | Pcl_fun (x0, x1, x2, x3) ->
            let (x0, acc) = self#arg_label x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            let (x2, acc) = self#pattern x2 acc in
            let (x3, acc) = self#class_expr x3 acc in
            (Class_expr_desc.of_concrete (Pcl_fun (x0, x1, x2, x3)), acc)
          | Pcl_apply (x0, x1) ->
            let (x0, acc) = self#class_expr x0 acc in
            let (x1, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#arg_label x0 acc in let (x1, acc) = self#expression x1 acc in ((x0, x1), acc)) x1 acc in
            (Class_expr_desc.of_concrete (Pcl_apply (x0, x1)), acc)
          | Pcl_let (x0, x1, x2) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#value_binding x1 acc in
            let (x2, acc) = self#class_expr x2 acc in
            (Class_expr_desc.of_concrete (Pcl_let (x0, x1, x2)), acc)
          | Pcl_constraint (x0, x1) ->
            let (x0, acc) = self#class_expr x0 acc in
            let (x1, acc) = self#class_type x1 acc in
            (Class_expr_desc.of_concrete (Pcl_constraint (x0, x1)), acc)
          | Pcl_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Class_expr_desc.of_concrete (Pcl_extension x0), acc)
          | Pcl_open (x0, x1, x2) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            let (x2, acc) = self#class_expr x2 acc in
            (Class_expr_desc.of_concrete (Pcl_open (x0, x1, x2)), acc)
      method class_structure : Class_structure.t -> 'acc -> (Class_structure.t * 'acc)  =
        fun class_structure acc ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let (pcstr_self, acc) = self#pattern pcstr_self acc in
          let (pcstr_fields, acc) = self#list self#class_field pcstr_fields acc in
          (Class_structure.of_concrete { pcstr_self; pcstr_fields }, acc)
      method class_field : Class_field.t -> 'acc -> (Class_field.t * 'acc)  =
        fun class_field acc ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let (pcf_desc, acc) = self#class_field_desc pcf_desc acc in
          let (pcf_loc, acc) = self#location pcf_loc acc in
          let (pcf_attributes, acc) = self#attributes pcf_attributes acc in
          (Class_field.of_concrete { pcf_desc; pcf_loc; pcf_attributes }, acc)
      method class_field_desc : Class_field_desc.t -> 'acc -> (Class_field_desc.t * 'acc)  =
        fun class_field_desc acc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#class_expr x1 acc in
            let (x2, acc) = self#option (self#loc self#string) x2 acc in
            (Class_field_desc.of_concrete (Pcf_inherit (x0, x1, x2)), acc)
          | Pcf_val x0 ->
            let (x0, acc) = (fun (x0, x1, x2) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#mutable_flag x1 acc in let (x2, acc) = self#class_field_kind x2 acc in ((x0, x1, x2), acc)) x0 acc in
            (Class_field_desc.of_concrete (Pcf_val x0), acc)
          | Pcf_method x0 ->
            let (x0, acc) = (fun (x0, x1, x2) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#private_flag x1 acc in let (x2, acc) = self#class_field_kind x2 acc in ((x0, x1, x2), acc)) x0 acc in
            (Class_field_desc.of_concrete (Pcf_method x0), acc)
          | Pcf_constraint x0 ->
            let (x0, acc) = (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) x0 acc in
            (Class_field_desc.of_concrete (Pcf_constraint x0), acc)
          | Pcf_initializer x0 ->
            let (x0, acc) = self#expression x0 acc in
            (Class_field_desc.of_concrete (Pcf_initializer x0), acc)
          | Pcf_attribute x0 ->
            let (x0, acc) = self#attribute x0 acc in
            (Class_field_desc.of_concrete (Pcf_attribute x0), acc)
          | Pcf_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Class_field_desc.of_concrete (Pcf_extension x0), acc)
      method class_field_kind : Class_field_kind.t -> 'acc -> (Class_field_kind.t * 'acc)  =
        fun class_field_kind acc ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let (x0, acc) = self#core_type x0 acc in
            (Class_field_kind.of_concrete (Cfk_virtual x0), acc)
          | Cfk_concrete (x0, x1) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Class_field_kind.of_concrete (Cfk_concrete (x0, x1)), acc)
      method class_declaration : Class_declaration.t -> 'acc -> (Class_declaration.t * 'acc)  =
        fun class_declaration acc ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let (concrete, acc) = self#class_infos_class_expr concrete acc in
          (Class_declaration.of_concrete concrete, acc)
      method module_type : Module_type.t -> 'acc -> (Module_type.t * 'acc)  =
        fun module_type acc ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let (pmty_desc, acc) = self#module_type_desc pmty_desc acc in
          let (pmty_loc, acc) = self#location pmty_loc acc in
          let (pmty_attributes, acc) = self#attributes pmty_attributes acc in
          (Module_type.of_concrete { pmty_desc; pmty_loc; pmty_attributes }, acc)
      method module_type_desc : Module_type_desc.t -> 'acc -> (Module_type_desc.t * 'acc)  =
        fun module_type_desc acc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Module_type_desc.of_concrete (Pmty_ident x0), acc)
          | Pmty_signature x0 ->
            let (x0, acc) = self#signature x0 acc in
            (Module_type_desc.of_concrete (Pmty_signature x0), acc)
          | Pmty_functor (x0, x1, x2) ->
            let (x0, acc) = self#loc self#string x0 acc in
            let (x1, acc) = self#option self#module_type x1 acc in
            let (x2, acc) = self#module_type x2 acc in
            (Module_type_desc.of_concrete (Pmty_functor (x0, x1, x2)), acc)
          | Pmty_with (x0, x1) ->
            let (x0, acc) = self#module_type x0 acc in
            let (x1, acc) = self#list self#with_constraint x1 acc in
            (Module_type_desc.of_concrete (Pmty_with (x0, x1)), acc)
          | Pmty_typeof x0 ->
            let (x0, acc) = self#module_expr x0 acc in
            (Module_type_desc.of_concrete (Pmty_typeof x0), acc)
          | Pmty_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Module_type_desc.of_concrete (Pmty_extension x0), acc)
          | Pmty_alias x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Module_type_desc.of_concrete (Pmty_alias x0), acc)
      method signature : Signature.t -> 'acc -> (Signature.t * 'acc)  =
        fun signature acc ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let (concrete, acc) = self#list self#signature_item concrete acc in
          (Signature.of_concrete concrete, acc)
      method signature_item : Signature_item.t -> 'acc -> (Signature_item.t * 'acc)  =
        fun signature_item acc ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let (psig_desc, acc) = self#signature_item_desc psig_desc acc in
          let (psig_loc, acc) = self#location psig_loc acc in
          (Signature_item.of_concrete { psig_desc; psig_loc }, acc)
      method signature_item_desc : Signature_item_desc.t -> 'acc -> (Signature_item_desc.t * 'acc)  =
        fun signature_item_desc acc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let (x0, acc) = self#value_description x0 acc in
            (Signature_item_desc.of_concrete (Psig_value x0), acc)
          | Psig_type (x0, x1) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#type_declaration x1 acc in
            (Signature_item_desc.of_concrete (Psig_type (x0, x1)), acc)
          | Psig_typext x0 ->
            let (x0, acc) = self#type_extension x0 acc in
            (Signature_item_desc.of_concrete (Psig_typext x0), acc)
          | Psig_exception x0 ->
            let (x0, acc) = self#extension_constructor x0 acc in
            (Signature_item_desc.of_concrete (Psig_exception x0), acc)
          | Psig_module x0 ->
            let (x0, acc) = self#module_declaration x0 acc in
            (Signature_item_desc.of_concrete (Psig_module x0), acc)
          | Psig_recmodule x0 ->
            let (x0, acc) = self#list self#module_declaration x0 acc in
            (Signature_item_desc.of_concrete (Psig_recmodule x0), acc)
          | Psig_modtype x0 ->
            let (x0, acc) = self#module_type_declaration x0 acc in
            (Signature_item_desc.of_concrete (Psig_modtype x0), acc)
          | Psig_open x0 ->
            let (x0, acc) = self#open_description x0 acc in
            (Signature_item_desc.of_concrete (Psig_open x0), acc)
          | Psig_include x0 ->
            let (x0, acc) = self#include_description x0 acc in
            (Signature_item_desc.of_concrete (Psig_include x0), acc)
          | Psig_class x0 ->
            let (x0, acc) = self#list self#class_description x0 acc in
            (Signature_item_desc.of_concrete (Psig_class x0), acc)
          | Psig_class_type x0 ->
            let (x0, acc) = self#list self#class_type_declaration x0 acc in
            (Signature_item_desc.of_concrete (Psig_class_type x0), acc)
          | Psig_attribute x0 ->
            let (x0, acc) = self#attribute x0 acc in
            (Signature_item_desc.of_concrete (Psig_attribute x0), acc)
          | Psig_extension (x0, x1) ->
            let (x0, acc) = self#extension x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            (Signature_item_desc.of_concrete (Psig_extension (x0, x1)), acc)
      method module_declaration : Module_declaration.t -> 'acc -> (Module_declaration.t * 'acc)  =
        fun module_declaration acc ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let (pmd_name, acc) = self#loc self#string pmd_name acc in
          let (pmd_type, acc) = self#module_type pmd_type acc in
          let (pmd_attributes, acc) = self#attributes pmd_attributes acc in
          let (pmd_loc, acc) = self#location pmd_loc acc in
          (Module_declaration.of_concrete { pmd_name; pmd_type; pmd_attributes; pmd_loc }, acc)
      method module_type_declaration : Module_type_declaration.t -> 'acc -> (Module_type_declaration.t * 'acc)  =
        fun module_type_declaration acc ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let (pmtd_name, acc) = self#loc self#string pmtd_name acc in
          let (pmtd_type, acc) = self#option self#module_type pmtd_type acc in
          let (pmtd_attributes, acc) = self#attributes pmtd_attributes acc in
          let (pmtd_loc, acc) = self#location pmtd_loc acc in
          (Module_type_declaration.of_concrete { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }, acc)
      method open_description : Open_description.t -> 'acc -> (Open_description.t * 'acc)  =
        fun open_description acc ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let (popen_lid, acc) = self#longident_loc popen_lid acc in
          let (popen_override, acc) = self#override_flag popen_override acc in
          let (popen_loc, acc) = self#location popen_loc acc in
          let (popen_attributes, acc) = self#attributes popen_attributes acc in
          (Open_description.of_concrete { popen_lid; popen_override; popen_loc; popen_attributes }, acc)
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> (Module_expr.t Include_infos.t * 'acc)  =
        fun include_infos acc ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let (pincl_mod, acc) = self#module_expr pincl_mod acc in
          let (pincl_loc, acc) = self#location pincl_loc acc in
          let (pincl_attributes, acc) = self#attributes pincl_attributes acc in
          (Include_infos.of_concrete_module_expr { pincl_mod; pincl_loc; pincl_attributes }, acc)
      method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> (Module_type.t Include_infos.t * 'acc)  =
        fun include_infos acc ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let (pincl_mod, acc) = self#module_type pincl_mod acc in
          let (pincl_loc, acc) = self#location pincl_loc acc in
          let (pincl_attributes, acc) = self#attributes pincl_attributes acc in
          (Include_infos.of_concrete_module_type { pincl_mod; pincl_loc; pincl_attributes }, acc)
      method include_description : Include_description.t -> 'acc -> (Include_description.t * 'acc)  =
        fun include_description acc ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let (concrete, acc) = self#include_infos_module_type concrete acc in
          (Include_description.of_concrete concrete, acc)
      method include_declaration : Include_declaration.t -> 'acc -> (Include_declaration.t * 'acc)  =
        fun include_declaration acc ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let (concrete, acc) = self#include_infos_module_expr concrete acc in
          (Include_declaration.of_concrete concrete, acc)
      method with_constraint : With_constraint.t -> 'acc -> (With_constraint.t * 'acc)  =
        fun with_constraint acc ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#type_declaration x1 acc in
            (With_constraint.of_concrete (Pwith_type (x0, x1)), acc)
          | Pwith_module (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            (With_constraint.of_concrete (Pwith_module (x0, x1)), acc)
          | Pwith_typesubst (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#type_declaration x1 acc in
            (With_constraint.of_concrete (Pwith_typesubst (x0, x1)), acc)
          | Pwith_modsubst (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            (With_constraint.of_concrete (Pwith_modsubst (x0, x1)), acc)
      method module_expr : Module_expr.t -> 'acc -> (Module_expr.t * 'acc)  =
        fun module_expr acc ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let (pmod_desc, acc) = self#module_expr_desc pmod_desc acc in
          let (pmod_loc, acc) = self#location pmod_loc acc in
          let (pmod_attributes, acc) = self#attributes pmod_attributes acc in
          (Module_expr.of_concrete { pmod_desc; pmod_loc; pmod_attributes }, acc)
      method module_expr_desc : Module_expr_desc.t -> 'acc -> (Module_expr_desc.t * 'acc)  =
        fun module_expr_desc acc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Module_expr_desc.of_concrete (Pmod_ident x0), acc)
          | Pmod_structure x0 ->
            let (x0, acc) = self#structure x0 acc in
            (Module_expr_desc.of_concrete (Pmod_structure x0), acc)
          | Pmod_functor (x0, x1, x2) ->
            let (x0, acc) = self#loc self#string x0 acc in
            let (x1, acc) = self#option self#module_type x1 acc in
            let (x2, acc) = self#module_expr x2 acc in
            (Module_expr_desc.of_concrete (Pmod_functor (x0, x1, x2)), acc)
          | Pmod_apply (x0, x1) ->
            let (x0, acc) = self#module_expr x0 acc in
            let (x1, acc) = self#module_expr x1 acc in
            (Module_expr_desc.of_concrete (Pmod_apply (x0, x1)), acc)
          | Pmod_constraint (x0, x1) ->
            let (x0, acc) = self#module_expr x0 acc in
            let (x1, acc) = self#module_type x1 acc in
            (Module_expr_desc.of_concrete (Pmod_constraint (x0, x1)), acc)
          | Pmod_unpack x0 ->
            let (x0, acc) = self#expression x0 acc in
            (Module_expr_desc.of_concrete (Pmod_unpack x0), acc)
          | Pmod_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Module_expr_desc.of_concrete (Pmod_extension x0), acc)
      method structure : Structure.t -> 'acc -> (Structure.t * 'acc)  =
        fun structure acc ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let (concrete, acc) = self#list self#structure_item concrete acc in
          (Structure.of_concrete concrete, acc)
      method structure_item : Structure_item.t -> 'acc -> (Structure_item.t * 'acc)  =
        fun structure_item acc ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let (pstr_desc, acc) = self#structure_item_desc pstr_desc acc in
          let (pstr_loc, acc) = self#location pstr_loc acc in
          (Structure_item.of_concrete { pstr_desc; pstr_loc }, acc)
      method structure_item_desc : Structure_item_desc.t -> 'acc -> (Structure_item_desc.t * 'acc)  =
        fun structure_item_desc acc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            (Structure_item_desc.of_concrete (Pstr_eval (x0, x1)), acc)
          | Pstr_value (x0, x1) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#value_binding x1 acc in
            (Structure_item_desc.of_concrete (Pstr_value (x0, x1)), acc)
          | Pstr_primitive x0 ->
            let (x0, acc) = self#value_description x0 acc in
            (Structure_item_desc.of_concrete (Pstr_primitive x0), acc)
          | Pstr_type (x0, x1) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#type_declaration x1 acc in
            (Structure_item_desc.of_concrete (Pstr_type (x0, x1)), acc)
          | Pstr_typext x0 ->
            let (x0, acc) = self#type_extension x0 acc in
            (Structure_item_desc.of_concrete (Pstr_typext x0), acc)
          | Pstr_exception x0 ->
            let (x0, acc) = self#extension_constructor x0 acc in
            (Structure_item_desc.of_concrete (Pstr_exception x0), acc)
          | Pstr_module x0 ->
            let (x0, acc) = self#module_binding x0 acc in
            (Structure_item_desc.of_concrete (Pstr_module x0), acc)
          | Pstr_recmodule x0 ->
            let (x0, acc) = self#list self#module_binding x0 acc in
            (Structure_item_desc.of_concrete (Pstr_recmodule x0), acc)
          | Pstr_modtype x0 ->
            let (x0, acc) = self#module_type_declaration x0 acc in
            (Structure_item_desc.of_concrete (Pstr_modtype x0), acc)
          | Pstr_open x0 ->
            let (x0, acc) = self#open_description x0 acc in
            (Structure_item_desc.of_concrete (Pstr_open x0), acc)
          | Pstr_class x0 ->
            let (x0, acc) = self#list self#class_declaration x0 acc in
            (Structure_item_desc.of_concrete (Pstr_class x0), acc)
          | Pstr_class_type x0 ->
            let (x0, acc) = self#list self#class_type_declaration x0 acc in
            (Structure_item_desc.of_concrete (Pstr_class_type x0), acc)
          | Pstr_include x0 ->
            let (x0, acc) = self#include_declaration x0 acc in
            (Structure_item_desc.of_concrete (Pstr_include x0), acc)
          | Pstr_attribute x0 ->
            let (x0, acc) = self#attribute x0 acc in
            (Structure_item_desc.of_concrete (Pstr_attribute x0), acc)
          | Pstr_extension (x0, x1) ->
            let (x0, acc) = self#extension x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            (Structure_item_desc.of_concrete (Pstr_extension (x0, x1)), acc)
      method value_binding : Value_binding.t -> 'acc -> (Value_binding.t * 'acc)  =
        fun value_binding acc ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let (pvb_pat, acc) = self#pattern pvb_pat acc in
          let (pvb_expr, acc) = self#expression pvb_expr acc in
          let (pvb_attributes, acc) = self#attributes pvb_attributes acc in
          let (pvb_loc, acc) = self#location pvb_loc acc in
          (Value_binding.of_concrete { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }, acc)
      method module_binding : Module_binding.t -> 'acc -> (Module_binding.t * 'acc)  =
        fun module_binding acc ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let (pmb_name, acc) = self#loc self#string pmb_name acc in
          let (pmb_expr, acc) = self#module_expr pmb_expr acc in
          let (pmb_attributes, acc) = self#attributes pmb_attributes acc in
          let (pmb_loc, acc) = self#location pmb_loc acc in
          (Module_binding.of_concrete { pmb_name; pmb_expr; pmb_attributes; pmb_loc }, acc)
      method toplevel_phrase : Toplevel_phrase.t -> 'acc -> (Toplevel_phrase.t * 'acc)  =
        fun toplevel_phrase acc ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let (x0, acc) = self#structure x0 acc in
            (Toplevel_phrase.of_concrete (Ptop_def x0), acc)
          | Ptop_dir (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#directive_argument x1 acc in
            (Toplevel_phrase.of_concrete (Ptop_dir (x0, x1)), acc)
      method directive_argument : Directive_argument.t -> 'acc -> (Directive_argument.t * 'acc)  =
        fun directive_argument acc ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            (Directive_argument.of_concrete Pdir_none, acc)
          | Pdir_string x0 ->
            let (x0, acc) = self#string x0 acc in
            (Directive_argument.of_concrete (Pdir_string x0), acc)
          | Pdir_int (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#option self#char x1 acc in
            (Directive_argument.of_concrete (Pdir_int (x0, x1)), acc)
          | Pdir_ident x0 ->
            let (x0, acc) = self#longident x0 acc in
            (Directive_argument.of_concrete (Pdir_ident x0), acc)
          | Pdir_bool x0 ->
            let (x0, acc) = self#bool x0 acc in
            (Directive_argument.of_concrete (Pdir_bool x0), acc)
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
      method longident : 'ctx -> Longident.t -> Longident.t  =
        fun _ctx longident ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let x0 = self#string _ctx x0 in
            Longident.of_concrete (Lident x0)
          | Ldot (x0, x1) ->
            let x0 = self#longident _ctx x0 in
            let x1 = self#string _ctx x1 in
            Longident.of_concrete (Ldot (x0, x1))
          | Lapply (x0, x1) ->
            let x0 = self#longident _ctx x0 in
            let x1 = self#longident _ctx x1 in
            Longident.of_concrete (Lapply (x0, x1))
      method longident_loc : 'ctx -> Longident_loc.t -> Longident_loc.t  =
        fun _ctx longident_loc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let concrete = self#loc self#longident _ctx concrete in
          Longident_loc.of_concrete concrete
      method rec_flag : 'ctx -> Rec_flag.t -> Rec_flag.t  =
        fun _ctx rec_flag ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            Rec_flag.of_concrete Nonrecursive
          | Recursive ->
            Rec_flag.of_concrete Recursive
      method direction_flag : 'ctx -> Direction_flag.t -> Direction_flag.t  =
        fun _ctx direction_flag ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            Direction_flag.of_concrete Upto
          | Downto ->
            Direction_flag.of_concrete Downto
      method private_flag : 'ctx -> Private_flag.t -> Private_flag.t  =
        fun _ctx private_flag ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            Private_flag.of_concrete Private
          | Public ->
            Private_flag.of_concrete Public
      method mutable_flag : 'ctx -> Mutable_flag.t -> Mutable_flag.t  =
        fun _ctx mutable_flag ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            Mutable_flag.of_concrete Immutable
          | Mutable ->
            Mutable_flag.of_concrete Mutable
      method virtual_flag : 'ctx -> Virtual_flag.t -> Virtual_flag.t  =
        fun _ctx virtual_flag ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            Virtual_flag.of_concrete Virtual
          | Concrete ->
            Virtual_flag.of_concrete Concrete
      method override_flag : 'ctx -> Override_flag.t -> Override_flag.t  =
        fun _ctx override_flag ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            Override_flag.of_concrete Override
          | Fresh ->
            Override_flag.of_concrete Fresh
      method closed_flag : 'ctx -> Closed_flag.t -> Closed_flag.t  =
        fun _ctx closed_flag ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            Closed_flag.of_concrete Closed
          | Open ->
            Closed_flag.of_concrete Open
      method label : 'ctx -> Label.t -> Label.t  =
        fun _ctx label ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let concrete = self#string _ctx concrete in
          Label.of_concrete concrete
      method arg_label : 'ctx -> Arg_label.t -> Arg_label.t  =
        fun _ctx arg_label ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            Arg_label.of_concrete Nolabel
          | Labelled x0 ->
            let x0 = self#string _ctx x0 in
            Arg_label.of_concrete (Labelled x0)
          | Optional x0 ->
            let x0 = self#string _ctx x0 in
            Arg_label.of_concrete (Optional x0)
      method variance : 'ctx -> Variance.t -> Variance.t  =
        fun _ctx variance ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            Variance.of_concrete Covariant
          | Contravariant ->
            Variance.of_concrete Contravariant
          | Invariant ->
            Variance.of_concrete Invariant
      method constant : 'ctx -> Constant.t -> Constant.t  =
        fun _ctx constant ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#option self#char _ctx x1 in
            Constant.of_concrete (Pconst_integer (x0, x1))
          | Pconst_char x0 ->
            let x0 = self#char _ctx x0 in
            Constant.of_concrete (Pconst_char x0)
          | Pconst_string (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#option self#string _ctx x1 in
            Constant.of_concrete (Pconst_string (x0, x1))
          | Pconst_float (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#option self#char _ctx x1 in
            Constant.of_concrete (Pconst_float (x0, x1))
      method attribute : 'ctx -> Attribute.t -> Attribute.t  =
        fun _ctx attribute ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string _ctx x0 in
          let x1 = self#payload _ctx x1 in
          Attribute.of_concrete (x0, x1)
      method extension : 'ctx -> Extension.t -> Extension.t  =
        fun _ctx extension ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string _ctx x0 in
          let x1 = self#payload _ctx x1 in
          Extension.of_concrete (x0, x1)
      method attributes : 'ctx -> Attributes.t -> Attributes.t  =
        fun _ctx attributes ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let concrete = self#list self#attribute _ctx concrete in
          Attributes.of_concrete concrete
      method payload : 'ctx -> Payload.t -> Payload.t  =
        fun _ctx payload ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let x0 = self#structure _ctx x0 in
            Payload.of_concrete (PStr x0)
          | PSig x0 ->
            let x0 = self#signature _ctx x0 in
            Payload.of_concrete (PSig x0)
          | PTyp x0 ->
            let x0 = self#core_type _ctx x0 in
            Payload.of_concrete (PTyp x0)
          | PPat (x0, x1) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            Payload.of_concrete (PPat (x0, x1))
      method core_type : 'ctx -> Core_type.t -> Core_type.t  =
        fun _ctx core_type ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let ptyp_desc = self#core_type_desc _ctx ptyp_desc in
          let ptyp_loc = self#location _ctx ptyp_loc in
          let ptyp_attributes = self#attributes _ctx ptyp_attributes in
          Core_type.of_concrete { ptyp_desc; ptyp_loc; ptyp_attributes }
      method core_type_desc : 'ctx -> Core_type_desc.t -> Core_type_desc.t  =
        fun _ctx core_type_desc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            Core_type_desc.of_concrete Ptyp_any
          | Ptyp_var x0 ->
            let x0 = self#string _ctx x0 in
            Core_type_desc.of_concrete (Ptyp_var x0)
          | Ptyp_arrow (x0, x1, x2) ->
            let x0 = self#arg_label _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            let x2 = self#core_type _ctx x2 in
            Core_type_desc.of_concrete (Ptyp_arrow (x0, x1, x2))
          | Ptyp_tuple x0 ->
            let x0 = self#list self#core_type _ctx x0 in
            Core_type_desc.of_concrete (Ptyp_tuple x0)
          | Ptyp_constr (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#list self#core_type _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_constr (x0, x1))
          | Ptyp_object (x0, x1) ->
            let x0 = self#list self#object_field _ctx x0 in
            let x1 = self#closed_flag _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_object (x0, x1))
          | Ptyp_class (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#list self#core_type _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_class (x0, x1))
          | Ptyp_alias (x0, x1) ->
            let x0 = self#core_type _ctx x0 in
            let x1 = self#string _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_alias (x0, x1))
          | Ptyp_variant (x0, x1, x2) ->
            let x0 = self#list self#row_field _ctx x0 in
            let x1 = self#closed_flag _ctx x1 in
            let x2 = self#option (self#list self#label) _ctx x2 in
            Core_type_desc.of_concrete (Ptyp_variant (x0, x1, x2))
          | Ptyp_poly (x0, x1) ->
            let x0 = self#list (self#loc self#string) _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_poly (x0, x1))
          | Ptyp_package x0 ->
            let x0 = self#package_type _ctx x0 in
            Core_type_desc.of_concrete (Ptyp_package x0)
          | Ptyp_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Core_type_desc.of_concrete (Ptyp_extension x0)
      method package_type : 'ctx -> Package_type.t -> Package_type.t  =
        fun _ctx package_type ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#longident_loc _ctx x0 in
          let x1 = self#list (fun _ctx (x0, x1) -> let x0 = self#longident_loc _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx x1 in
          Package_type.of_concrete (x0, x1)
      method row_field : 'ctx -> Row_field.t -> Row_field.t  =
        fun _ctx row_field ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let x0 = self#loc self#label _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            let x2 = self#bool _ctx x2 in
            let x3 = self#list self#core_type _ctx x3 in
            Row_field.of_concrete (Rtag (x0, x1, x2, x3))
          | Rinherit x0 ->
            let x0 = self#core_type _ctx x0 in
            Row_field.of_concrete (Rinherit x0)
      method object_field : 'ctx -> Object_field.t -> Object_field.t  =
        fun _ctx object_field ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let x0 = self#loc self#label _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            let x2 = self#core_type _ctx x2 in
            Object_field.of_concrete (Otag (x0, x1, x2))
          | Oinherit x0 ->
            let x0 = self#core_type _ctx x0 in
            Object_field.of_concrete (Oinherit x0)
      method pattern : 'ctx -> Pattern.t -> Pattern.t  =
        fun _ctx pattern ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let ppat_desc = self#pattern_desc _ctx ppat_desc in
          let ppat_loc = self#location _ctx ppat_loc in
          let ppat_attributes = self#attributes _ctx ppat_attributes in
          Pattern.of_concrete { ppat_desc; ppat_loc; ppat_attributes }
      method pattern_desc : 'ctx -> Pattern_desc.t -> Pattern_desc.t  =
        fun _ctx pattern_desc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            Pattern_desc.of_concrete Ppat_any
          | Ppat_var x0 ->
            let x0 = self#loc self#string _ctx x0 in
            Pattern_desc.of_concrete (Ppat_var x0)
          | Ppat_alias (x0, x1) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#loc self#string _ctx x1 in
            Pattern_desc.of_concrete (Ppat_alias (x0, x1))
          | Ppat_constant x0 ->
            let x0 = self#constant _ctx x0 in
            Pattern_desc.of_concrete (Ppat_constant x0)
          | Ppat_interval (x0, x1) ->
            let x0 = self#constant _ctx x0 in
            let x1 = self#constant _ctx x1 in
            Pattern_desc.of_concrete (Ppat_interval (x0, x1))
          | Ppat_tuple x0 ->
            let x0 = self#list self#pattern _ctx x0 in
            Pattern_desc.of_concrete (Ppat_tuple x0)
          | Ppat_construct (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#option self#pattern _ctx x1 in
            Pattern_desc.of_concrete (Ppat_construct (x0, x1))
          | Ppat_variant (x0, x1) ->
            let x0 = self#label _ctx x0 in
            let x1 = self#option self#pattern _ctx x1 in
            Pattern_desc.of_concrete (Ppat_variant (x0, x1))
          | Ppat_record (x0, x1) ->
            let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#longident_loc _ctx x0 in let x1 = self#pattern _ctx x1 in (x0, x1)) _ctx x0 in
            let x1 = self#closed_flag _ctx x1 in
            Pattern_desc.of_concrete (Ppat_record (x0, x1))
          | Ppat_array x0 ->
            let x0 = self#list self#pattern _ctx x0 in
            Pattern_desc.of_concrete (Ppat_array x0)
          | Ppat_or (x0, x1) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#pattern _ctx x1 in
            Pattern_desc.of_concrete (Ppat_or (x0, x1))
          | Ppat_constraint (x0, x1) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            Pattern_desc.of_concrete (Ppat_constraint (x0, x1))
          | Ppat_type x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Pattern_desc.of_concrete (Ppat_type x0)
          | Ppat_lazy x0 ->
            let x0 = self#pattern _ctx x0 in
            Pattern_desc.of_concrete (Ppat_lazy x0)
          | Ppat_unpack x0 ->
            let x0 = self#loc self#string _ctx x0 in
            Pattern_desc.of_concrete (Ppat_unpack x0)
          | Ppat_exception x0 ->
            let x0 = self#pattern _ctx x0 in
            Pattern_desc.of_concrete (Ppat_exception x0)
          | Ppat_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Pattern_desc.of_concrete (Ppat_extension x0)
          | Ppat_open (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#pattern _ctx x1 in
            Pattern_desc.of_concrete (Ppat_open (x0, x1))
      method expression : 'ctx -> Expression.t -> Expression.t  =
        fun _ctx expression ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let pexp_desc = self#expression_desc _ctx pexp_desc in
          let pexp_loc = self#location _ctx pexp_loc in
          let pexp_attributes = self#attributes _ctx pexp_attributes in
          Expression.of_concrete { pexp_desc; pexp_loc; pexp_attributes }
      method expression_desc : 'ctx -> Expression_desc.t -> Expression_desc.t  =
        fun _ctx expression_desc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Expression_desc.of_concrete (Pexp_ident x0)
          | Pexp_constant x0 ->
            let x0 = self#constant _ctx x0 in
            Expression_desc.of_concrete (Pexp_constant x0)
          | Pexp_let (x0, x1, x2) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#value_binding _ctx x1 in
            let x2 = self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_let (x0, x1, x2))
          | Pexp_function x0 ->
            let x0 = self#list self#case _ctx x0 in
            Expression_desc.of_concrete (Pexp_function x0)
          | Pexp_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            let x2 = self#pattern _ctx x2 in
            let x3 = self#expression _ctx x3 in
            Expression_desc.of_concrete (Pexp_fun (x0, x1, x2, x3))
          | Pexp_apply (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#list (fun _ctx (x0, x1) -> let x0 = self#arg_label _ctx x0 in let x1 = self#expression _ctx x1 in (x0, x1)) _ctx x1 in
            Expression_desc.of_concrete (Pexp_apply (x0, x1))
          | Pexp_match (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#list self#case _ctx x1 in
            Expression_desc.of_concrete (Pexp_match (x0, x1))
          | Pexp_try (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#list self#case _ctx x1 in
            Expression_desc.of_concrete (Pexp_try (x0, x1))
          | Pexp_tuple x0 ->
            let x0 = self#list self#expression _ctx x0 in
            Expression_desc.of_concrete (Pexp_tuple x0)
          | Pexp_construct (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_construct (x0, x1))
          | Pexp_variant (x0, x1) ->
            let x0 = self#label _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_variant (x0, x1))
          | Pexp_record (x0, x1) ->
            let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#longident_loc _ctx x0 in let x1 = self#expression _ctx x1 in (x0, x1)) _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_record (x0, x1))
          | Pexp_field (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            Expression_desc.of_concrete (Pexp_field (x0, x1))
          | Pexp_setfield (x0, x1, x2) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            let x2 = self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_setfield (x0, x1, x2))
          | Pexp_array x0 ->
            let x0 = self#list self#expression _ctx x0 in
            Expression_desc.of_concrete (Pexp_array x0)
          | Pexp_ifthenelse (x0, x1, x2) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#expression _ctx x1 in
            let x2 = self#option self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_ifthenelse (x0, x1, x2))
          | Pexp_sequence (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_sequence (x0, x1))
          | Pexp_while (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_while (x0, x1))
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#expression _ctx x1 in
            let x2 = self#expression _ctx x2 in
            let x3 = self#direction_flag _ctx x3 in
            let x4 = self#expression _ctx x4 in
            Expression_desc.of_concrete (Pexp_for (x0, x1, x2, x3, x4))
          | Pexp_constraint (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            Expression_desc.of_concrete (Pexp_constraint (x0, x1))
          | Pexp_coerce (x0, x1, x2) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#option self#core_type _ctx x1 in
            let x2 = self#core_type _ctx x2 in
            Expression_desc.of_concrete (Pexp_coerce (x0, x1, x2))
          | Pexp_send (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#loc self#label _ctx x1 in
            Expression_desc.of_concrete (Pexp_send (x0, x1))
          | Pexp_new x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Expression_desc.of_concrete (Pexp_new x0)
          | Pexp_setinstvar (x0, x1) ->
            let x0 = self#loc self#label _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_setinstvar (x0, x1))
          | Pexp_override x0 ->
            let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#expression _ctx x1 in (x0, x1)) _ctx x0 in
            Expression_desc.of_concrete (Pexp_override x0)
          | Pexp_letmodule (x0, x1, x2) ->
            let x0 = self#loc self#string _ctx x0 in
            let x1 = self#module_expr _ctx x1 in
            let x2 = self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_letmodule (x0, x1, x2))
          | Pexp_letexception (x0, x1) ->
            let x0 = self#extension_constructor _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_letexception (x0, x1))
          | Pexp_assert x0 ->
            let x0 = self#expression _ctx x0 in
            Expression_desc.of_concrete (Pexp_assert x0)
          | Pexp_lazy x0 ->
            let x0 = self#expression _ctx x0 in
            Expression_desc.of_concrete (Pexp_lazy x0)
          | Pexp_poly (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#option self#core_type _ctx x1 in
            Expression_desc.of_concrete (Pexp_poly (x0, x1))
          | Pexp_object x0 ->
            let x0 = self#class_structure _ctx x0 in
            Expression_desc.of_concrete (Pexp_object x0)
          | Pexp_newtype (x0, x1) ->
            let x0 = self#loc self#string _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_newtype (x0, x1))
          | Pexp_pack x0 ->
            let x0 = self#module_expr _ctx x0 in
            Expression_desc.of_concrete (Pexp_pack x0)
          | Pexp_open (x0, x1, x2) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            let x2 = self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_open (x0, x1, x2))
          | Pexp_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Expression_desc.of_concrete (Pexp_extension x0)
          | Pexp_unreachable ->
            Expression_desc.of_concrete Pexp_unreachable
      method case : 'ctx -> Case.t -> Case.t  =
        fun _ctx case ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let pc_lhs = self#pattern _ctx pc_lhs in
          let pc_guard = self#option self#expression _ctx pc_guard in
          let pc_rhs = self#expression _ctx pc_rhs in
          Case.of_concrete { pc_lhs; pc_guard; pc_rhs }
      method value_description : 'ctx -> Value_description.t -> Value_description.t  =
        fun _ctx value_description ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let pval_name = self#loc self#string _ctx pval_name in
          let pval_type = self#core_type _ctx pval_type in
          let pval_prim = self#list self#string _ctx pval_prim in
          let pval_attributes = self#attributes _ctx pval_attributes in
          let pval_loc = self#location _ctx pval_loc in
          Value_description.of_concrete { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
      method type_declaration : 'ctx -> Type_declaration.t -> Type_declaration.t  =
        fun _ctx type_declaration ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let ptype_name = self#loc self#string _ctx ptype_name in
          let ptype_params = self#list (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#variance _ctx x1 in (x0, x1)) _ctx ptype_params in
          let ptype_cstrs = self#list (fun _ctx (x0, x1, x2) -> let x0 = self#core_type _ctx x0 in let x1 = self#core_type _ctx x1 in let x2 = self#location _ctx x2 in (x0, x1, x2)) _ctx ptype_cstrs in
          let ptype_kind = self#type_kind _ctx ptype_kind in
          let ptype_private = self#private_flag _ctx ptype_private in
          let ptype_manifest = self#option self#core_type _ctx ptype_manifest in
          let ptype_attributes = self#attributes _ctx ptype_attributes in
          let ptype_loc = self#location _ctx ptype_loc in
          Type_declaration.of_concrete { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }
      method type_kind : 'ctx -> Type_kind.t -> Type_kind.t  =
        fun _ctx type_kind ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            Type_kind.of_concrete Ptype_abstract
          | Ptype_variant x0 ->
            let x0 = self#list self#constructor_declaration _ctx x0 in
            Type_kind.of_concrete (Ptype_variant x0)
          | Ptype_record x0 ->
            let x0 = self#list self#label_declaration _ctx x0 in
            Type_kind.of_concrete (Ptype_record x0)
          | Ptype_open ->
            Type_kind.of_concrete Ptype_open
      method label_declaration : 'ctx -> Label_declaration.t -> Label_declaration.t  =
        fun _ctx label_declaration ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let pld_name = self#loc self#string _ctx pld_name in
          let pld_mutable = self#mutable_flag _ctx pld_mutable in
          let pld_type = self#core_type _ctx pld_type in
          let pld_loc = self#location _ctx pld_loc in
          let pld_attributes = self#attributes _ctx pld_attributes in
          Label_declaration.of_concrete { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
      method constructor_declaration : 'ctx -> Constructor_declaration.t -> Constructor_declaration.t  =
        fun _ctx constructor_declaration ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let pcd_name = self#loc self#string _ctx pcd_name in
          let pcd_args = self#constructor_arguments _ctx pcd_args in
          let pcd_res = self#option self#core_type _ctx pcd_res in
          let pcd_loc = self#location _ctx pcd_loc in
          let pcd_attributes = self#attributes _ctx pcd_attributes in
          Constructor_declaration.of_concrete { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
      method constructor_arguments : 'ctx -> Constructor_arguments.t -> Constructor_arguments.t  =
        fun _ctx constructor_arguments ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let x0 = self#list self#core_type _ctx x0 in
            Constructor_arguments.of_concrete (Pcstr_tuple x0)
          | Pcstr_record x0 ->
            let x0 = self#list self#label_declaration _ctx x0 in
            Constructor_arguments.of_concrete (Pcstr_record x0)
      method type_extension : 'ctx -> Type_extension.t -> Type_extension.t  =
        fun _ctx type_extension ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let ptyext_path = self#longident_loc _ctx ptyext_path in
          let ptyext_params = self#list (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#variance _ctx x1 in (x0, x1)) _ctx ptyext_params in
          let ptyext_constructors = self#list self#extension_constructor _ctx ptyext_constructors in
          let ptyext_private = self#private_flag _ctx ptyext_private in
          let ptyext_attributes = self#attributes _ctx ptyext_attributes in
          Type_extension.of_concrete { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }
      method extension_constructor : 'ctx -> Extension_constructor.t -> Extension_constructor.t  =
        fun _ctx extension_constructor ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let pext_name = self#loc self#string _ctx pext_name in
          let pext_kind = self#extension_constructor_kind _ctx pext_kind in
          let pext_loc = self#location _ctx pext_loc in
          let pext_attributes = self#attributes _ctx pext_attributes in
          Extension_constructor.of_concrete { pext_name; pext_kind; pext_loc; pext_attributes }
      method extension_constructor_kind : 'ctx -> Extension_constructor_kind.t -> Extension_constructor_kind.t  =
        fun _ctx extension_constructor_kind ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let x0 = self#constructor_arguments _ctx x0 in
            let x1 = self#option self#core_type _ctx x1 in
            Extension_constructor_kind.of_concrete (Pext_decl (x0, x1))
          | Pext_rebind x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Extension_constructor_kind.of_concrete (Pext_rebind x0)
      method class_type : 'ctx -> Class_type.t -> Class_type.t  =
        fun _ctx class_type ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let pcty_desc = self#class_type_desc _ctx pcty_desc in
          let pcty_loc = self#location _ctx pcty_loc in
          let pcty_attributes = self#attributes _ctx pcty_attributes in
          Class_type.of_concrete { pcty_desc; pcty_loc; pcty_attributes }
      method class_type_desc : 'ctx -> Class_type_desc.t -> Class_type_desc.t  =
        fun _ctx class_type_desc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#list self#core_type _ctx x1 in
            Class_type_desc.of_concrete (Pcty_constr (x0, x1))
          | Pcty_signature x0 ->
            let x0 = self#class_signature _ctx x0 in
            Class_type_desc.of_concrete (Pcty_signature x0)
          | Pcty_arrow (x0, x1, x2) ->
            let x0 = self#arg_label _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            let x2 = self#class_type _ctx x2 in
            Class_type_desc.of_concrete (Pcty_arrow (x0, x1, x2))
          | Pcty_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Class_type_desc.of_concrete (Pcty_extension x0)
          | Pcty_open (x0, x1, x2) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            let x2 = self#class_type _ctx x2 in
            Class_type_desc.of_concrete (Pcty_open (x0, x1, x2))
      method class_signature : 'ctx -> Class_signature.t -> Class_signature.t  =
        fun _ctx class_signature ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let pcsig_self = self#core_type _ctx pcsig_self in
          let pcsig_fields = self#list self#class_type_field _ctx pcsig_fields in
          Class_signature.of_concrete { pcsig_self; pcsig_fields }
      method class_type_field : 'ctx -> Class_type_field.t -> Class_type_field.t  =
        fun _ctx class_type_field ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let pctf_desc = self#class_type_field_desc _ctx pctf_desc in
          let pctf_loc = self#location _ctx pctf_loc in
          let pctf_attributes = self#attributes _ctx pctf_attributes in
          Class_type_field.of_concrete { pctf_desc; pctf_loc; pctf_attributes }
      method class_type_field_desc : 'ctx -> Class_type_field_desc.t -> Class_type_field_desc.t  =
        fun _ctx class_type_field_desc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let x0 = self#class_type _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_inherit x0)
          | Pctf_val x0 ->
            let x0 = (fun _ctx (x0, x1, x2, x3) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#mutable_flag _ctx x1 in let x2 = self#virtual_flag _ctx x2 in let x3 = self#core_type _ctx x3 in (x0, x1, x2, x3)) _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_val x0)
          | Pctf_method x0 ->
            let x0 = (fun _ctx (x0, x1, x2, x3) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#private_flag _ctx x1 in let x2 = self#virtual_flag _ctx x2 in let x3 = self#core_type _ctx x3 in (x0, x1, x2, x3)) _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_method x0)
          | Pctf_constraint x0 ->
            let x0 = (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_constraint x0)
          | Pctf_attribute x0 ->
            let x0 = self#attribute _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_attribute x0)
          | Pctf_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_extension x0)
      method class_infos_class_expr : 'ctx -> Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t  =
        fun _ctx class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag _ctx pci_virt in
          let pci_params = self#list (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#variance _ctx x1 in (x0, x1)) _ctx pci_params in
          let pci_name = self#loc self#string _ctx pci_name in
          let pci_expr = self#class_expr _ctx pci_expr in
          let pci_loc = self#location _ctx pci_loc in
          let pci_attributes = self#attributes _ctx pci_attributes in
          Class_infos.of_concrete_class_expr { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
      method class_infos_class_type : 'ctx -> Class_type.t Class_infos.t -> Class_type.t Class_infos.t  =
        fun _ctx class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag _ctx pci_virt in
          let pci_params = self#list (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#variance _ctx x1 in (x0, x1)) _ctx pci_params in
          let pci_name = self#loc self#string _ctx pci_name in
          let pci_expr = self#class_type _ctx pci_expr in
          let pci_loc = self#location _ctx pci_loc in
          let pci_attributes = self#attributes _ctx pci_attributes in
          Class_infos.of_concrete_class_type { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
      method class_description : 'ctx -> Class_description.t -> Class_description.t  =
        fun _ctx class_description ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type _ctx concrete in
          Class_description.of_concrete concrete
      method class_type_declaration : 'ctx -> Class_type_declaration.t -> Class_type_declaration.t  =
        fun _ctx class_type_declaration ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type _ctx concrete in
          Class_type_declaration.of_concrete concrete
      method class_expr : 'ctx -> Class_expr.t -> Class_expr.t  =
        fun _ctx class_expr ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let pcl_desc = self#class_expr_desc _ctx pcl_desc in
          let pcl_loc = self#location _ctx pcl_loc in
          let pcl_attributes = self#attributes _ctx pcl_attributes in
          Class_expr.of_concrete { pcl_desc; pcl_loc; pcl_attributes }
      method class_expr_desc : 'ctx -> Class_expr_desc.t -> Class_expr_desc.t  =
        fun _ctx class_expr_desc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#list self#core_type _ctx x1 in
            Class_expr_desc.of_concrete (Pcl_constr (x0, x1))
          | Pcl_structure x0 ->
            let x0 = self#class_structure _ctx x0 in
            Class_expr_desc.of_concrete (Pcl_structure x0)
          | Pcl_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            let x2 = self#pattern _ctx x2 in
            let x3 = self#class_expr _ctx x3 in
            Class_expr_desc.of_concrete (Pcl_fun (x0, x1, x2, x3))
          | Pcl_apply (x0, x1) ->
            let x0 = self#class_expr _ctx x0 in
            let x1 = self#list (fun _ctx (x0, x1) -> let x0 = self#arg_label _ctx x0 in let x1 = self#expression _ctx x1 in (x0, x1)) _ctx x1 in
            Class_expr_desc.of_concrete (Pcl_apply (x0, x1))
          | Pcl_let (x0, x1, x2) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#value_binding _ctx x1 in
            let x2 = self#class_expr _ctx x2 in
            Class_expr_desc.of_concrete (Pcl_let (x0, x1, x2))
          | Pcl_constraint (x0, x1) ->
            let x0 = self#class_expr _ctx x0 in
            let x1 = self#class_type _ctx x1 in
            Class_expr_desc.of_concrete (Pcl_constraint (x0, x1))
          | Pcl_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Class_expr_desc.of_concrete (Pcl_extension x0)
          | Pcl_open (x0, x1, x2) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            let x2 = self#class_expr _ctx x2 in
            Class_expr_desc.of_concrete (Pcl_open (x0, x1, x2))
      method class_structure : 'ctx -> Class_structure.t -> Class_structure.t  =
        fun _ctx class_structure ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let pcstr_self = self#pattern _ctx pcstr_self in
          let pcstr_fields = self#list self#class_field _ctx pcstr_fields in
          Class_structure.of_concrete { pcstr_self; pcstr_fields }
      method class_field : 'ctx -> Class_field.t -> Class_field.t  =
        fun _ctx class_field ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let pcf_desc = self#class_field_desc _ctx pcf_desc in
          let pcf_loc = self#location _ctx pcf_loc in
          let pcf_attributes = self#attributes _ctx pcf_attributes in
          Class_field.of_concrete { pcf_desc; pcf_loc; pcf_attributes }
      method class_field_desc : 'ctx -> Class_field_desc.t -> Class_field_desc.t  =
        fun _ctx class_field_desc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#class_expr _ctx x1 in
            let x2 = self#option (self#loc self#string) _ctx x2 in
            Class_field_desc.of_concrete (Pcf_inherit (x0, x1, x2))
          | Pcf_val x0 ->
            let x0 = (fun _ctx (x0, x1, x2) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#mutable_flag _ctx x1 in let x2 = self#class_field_kind _ctx x2 in (x0, x1, x2)) _ctx x0 in
            Class_field_desc.of_concrete (Pcf_val x0)
          | Pcf_method x0 ->
            let x0 = (fun _ctx (x0, x1, x2) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#private_flag _ctx x1 in let x2 = self#class_field_kind _ctx x2 in (x0, x1, x2)) _ctx x0 in
            Class_field_desc.of_concrete (Pcf_method x0)
          | Pcf_constraint x0 ->
            let x0 = (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx x0 in
            Class_field_desc.of_concrete (Pcf_constraint x0)
          | Pcf_initializer x0 ->
            let x0 = self#expression _ctx x0 in
            Class_field_desc.of_concrete (Pcf_initializer x0)
          | Pcf_attribute x0 ->
            let x0 = self#attribute _ctx x0 in
            Class_field_desc.of_concrete (Pcf_attribute x0)
          | Pcf_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Class_field_desc.of_concrete (Pcf_extension x0)
      method class_field_kind : 'ctx -> Class_field_kind.t -> Class_field_kind.t  =
        fun _ctx class_field_kind ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let x0 = self#core_type _ctx x0 in
            Class_field_kind.of_concrete (Cfk_virtual x0)
          | Cfk_concrete (x0, x1) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Class_field_kind.of_concrete (Cfk_concrete (x0, x1))
      method class_declaration : 'ctx -> Class_declaration.t -> Class_declaration.t  =
        fun _ctx class_declaration ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_expr _ctx concrete in
          Class_declaration.of_concrete concrete
      method module_type : 'ctx -> Module_type.t -> Module_type.t  =
        fun _ctx module_type ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let pmty_desc = self#module_type_desc _ctx pmty_desc in
          let pmty_loc = self#location _ctx pmty_loc in
          let pmty_attributes = self#attributes _ctx pmty_attributes in
          Module_type.of_concrete { pmty_desc; pmty_loc; pmty_attributes }
      method module_type_desc : 'ctx -> Module_type_desc.t -> Module_type_desc.t  =
        fun _ctx module_type_desc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Module_type_desc.of_concrete (Pmty_ident x0)
          | Pmty_signature x0 ->
            let x0 = self#signature _ctx x0 in
            Module_type_desc.of_concrete (Pmty_signature x0)
          | Pmty_functor (x0, x1, x2) ->
            let x0 = self#loc self#string _ctx x0 in
            let x1 = self#option self#module_type _ctx x1 in
            let x2 = self#module_type _ctx x2 in
            Module_type_desc.of_concrete (Pmty_functor (x0, x1, x2))
          | Pmty_with (x0, x1) ->
            let x0 = self#module_type _ctx x0 in
            let x1 = self#list self#with_constraint _ctx x1 in
            Module_type_desc.of_concrete (Pmty_with (x0, x1))
          | Pmty_typeof x0 ->
            let x0 = self#module_expr _ctx x0 in
            Module_type_desc.of_concrete (Pmty_typeof x0)
          | Pmty_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Module_type_desc.of_concrete (Pmty_extension x0)
          | Pmty_alias x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Module_type_desc.of_concrete (Pmty_alias x0)
      method signature : 'ctx -> Signature.t -> Signature.t  =
        fun _ctx signature ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let concrete = self#list self#signature_item _ctx concrete in
          Signature.of_concrete concrete
      method signature_item : 'ctx -> Signature_item.t -> Signature_item.t  =
        fun _ctx signature_item ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let psig_desc = self#signature_item_desc _ctx psig_desc in
          let psig_loc = self#location _ctx psig_loc in
          Signature_item.of_concrete { psig_desc; psig_loc }
      method signature_item_desc : 'ctx -> Signature_item_desc.t -> Signature_item_desc.t  =
        fun _ctx signature_item_desc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let x0 = self#value_description _ctx x0 in
            Signature_item_desc.of_concrete (Psig_value x0)
          | Psig_type (x0, x1) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#type_declaration _ctx x1 in
            Signature_item_desc.of_concrete (Psig_type (x0, x1))
          | Psig_typext x0 ->
            let x0 = self#type_extension _ctx x0 in
            Signature_item_desc.of_concrete (Psig_typext x0)
          | Psig_exception x0 ->
            let x0 = self#extension_constructor _ctx x0 in
            Signature_item_desc.of_concrete (Psig_exception x0)
          | Psig_module x0 ->
            let x0 = self#module_declaration _ctx x0 in
            Signature_item_desc.of_concrete (Psig_module x0)
          | Psig_recmodule x0 ->
            let x0 = self#list self#module_declaration _ctx x0 in
            Signature_item_desc.of_concrete (Psig_recmodule x0)
          | Psig_modtype x0 ->
            let x0 = self#module_type_declaration _ctx x0 in
            Signature_item_desc.of_concrete (Psig_modtype x0)
          | Psig_open x0 ->
            let x0 = self#open_description _ctx x0 in
            Signature_item_desc.of_concrete (Psig_open x0)
          | Psig_include x0 ->
            let x0 = self#include_description _ctx x0 in
            Signature_item_desc.of_concrete (Psig_include x0)
          | Psig_class x0 ->
            let x0 = self#list self#class_description _ctx x0 in
            Signature_item_desc.of_concrete (Psig_class x0)
          | Psig_class_type x0 ->
            let x0 = self#list self#class_type_declaration _ctx x0 in
            Signature_item_desc.of_concrete (Psig_class_type x0)
          | Psig_attribute x0 ->
            let x0 = self#attribute _ctx x0 in
            Signature_item_desc.of_concrete (Psig_attribute x0)
          | Psig_extension (x0, x1) ->
            let x0 = self#extension _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            Signature_item_desc.of_concrete (Psig_extension (x0, x1))
      method module_declaration : 'ctx -> Module_declaration.t -> Module_declaration.t  =
        fun _ctx module_declaration ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let pmd_name = self#loc self#string _ctx pmd_name in
          let pmd_type = self#module_type _ctx pmd_type in
          let pmd_attributes = self#attributes _ctx pmd_attributes in
          let pmd_loc = self#location _ctx pmd_loc in
          Module_declaration.of_concrete { pmd_name; pmd_type; pmd_attributes; pmd_loc }
      method module_type_declaration : 'ctx -> Module_type_declaration.t -> Module_type_declaration.t  =
        fun _ctx module_type_declaration ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let pmtd_name = self#loc self#string _ctx pmtd_name in
          let pmtd_type = self#option self#module_type _ctx pmtd_type in
          let pmtd_attributes = self#attributes _ctx pmtd_attributes in
          let pmtd_loc = self#location _ctx pmtd_loc in
          Module_type_declaration.of_concrete { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
      method open_description : 'ctx -> Open_description.t -> Open_description.t  =
        fun _ctx open_description ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let popen_lid = self#longident_loc _ctx popen_lid in
          let popen_override = self#override_flag _ctx popen_override in
          let popen_loc = self#location _ctx popen_loc in
          let popen_attributes = self#attributes _ctx popen_attributes in
          Open_description.of_concrete { popen_lid; popen_override; popen_loc; popen_attributes }
      method include_infos_module_expr : 'ctx -> Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t  =
        fun _ctx include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_expr _ctx pincl_mod in
          let pincl_loc = self#location _ctx pincl_loc in
          let pincl_attributes = self#attributes _ctx pincl_attributes in
          Include_infos.of_concrete_module_expr { pincl_mod; pincl_loc; pincl_attributes }
      method include_infos_module_type : 'ctx -> Module_type.t Include_infos.t -> Module_type.t Include_infos.t  =
        fun _ctx include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_type _ctx pincl_mod in
          let pincl_loc = self#location _ctx pincl_loc in
          let pincl_attributes = self#attributes _ctx pincl_attributes in
          Include_infos.of_concrete_module_type { pincl_mod; pincl_loc; pincl_attributes }
      method include_description : 'ctx -> Include_description.t -> Include_description.t  =
        fun _ctx include_description ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let concrete = self#include_infos_module_type _ctx concrete in
          Include_description.of_concrete concrete
      method include_declaration : 'ctx -> Include_declaration.t -> Include_declaration.t  =
        fun _ctx include_declaration ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let concrete = self#include_infos_module_expr _ctx concrete in
          Include_declaration.of_concrete concrete
      method with_constraint : 'ctx -> With_constraint.t -> With_constraint.t  =
        fun _ctx with_constraint ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#type_declaration _ctx x1 in
            With_constraint.of_concrete (Pwith_type (x0, x1))
          | Pwith_module (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            With_constraint.of_concrete (Pwith_module (x0, x1))
          | Pwith_typesubst (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#type_declaration _ctx x1 in
            With_constraint.of_concrete (Pwith_typesubst (x0, x1))
          | Pwith_modsubst (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            With_constraint.of_concrete (Pwith_modsubst (x0, x1))
      method module_expr : 'ctx -> Module_expr.t -> Module_expr.t  =
        fun _ctx module_expr ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let pmod_desc = self#module_expr_desc _ctx pmod_desc in
          let pmod_loc = self#location _ctx pmod_loc in
          let pmod_attributes = self#attributes _ctx pmod_attributes in
          Module_expr.of_concrete { pmod_desc; pmod_loc; pmod_attributes }
      method module_expr_desc : 'ctx -> Module_expr_desc.t -> Module_expr_desc.t  =
        fun _ctx module_expr_desc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Module_expr_desc.of_concrete (Pmod_ident x0)
          | Pmod_structure x0 ->
            let x0 = self#structure _ctx x0 in
            Module_expr_desc.of_concrete (Pmod_structure x0)
          | Pmod_functor (x0, x1, x2) ->
            let x0 = self#loc self#string _ctx x0 in
            let x1 = self#option self#module_type _ctx x1 in
            let x2 = self#module_expr _ctx x2 in
            Module_expr_desc.of_concrete (Pmod_functor (x0, x1, x2))
          | Pmod_apply (x0, x1) ->
            let x0 = self#module_expr _ctx x0 in
            let x1 = self#module_expr _ctx x1 in
            Module_expr_desc.of_concrete (Pmod_apply (x0, x1))
          | Pmod_constraint (x0, x1) ->
            let x0 = self#module_expr _ctx x0 in
            let x1 = self#module_type _ctx x1 in
            Module_expr_desc.of_concrete (Pmod_constraint (x0, x1))
          | Pmod_unpack x0 ->
            let x0 = self#expression _ctx x0 in
            Module_expr_desc.of_concrete (Pmod_unpack x0)
          | Pmod_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Module_expr_desc.of_concrete (Pmod_extension x0)
      method structure : 'ctx -> Structure.t -> Structure.t  =
        fun _ctx structure ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let concrete = self#list self#structure_item _ctx concrete in
          Structure.of_concrete concrete
      method structure_item : 'ctx -> Structure_item.t -> Structure_item.t  =
        fun _ctx structure_item ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let pstr_desc = self#structure_item_desc _ctx pstr_desc in
          let pstr_loc = self#location _ctx pstr_loc in
          Structure_item.of_concrete { pstr_desc; pstr_loc }
      method structure_item_desc : 'ctx -> Structure_item_desc.t -> Structure_item_desc.t  =
        fun _ctx structure_item_desc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            Structure_item_desc.of_concrete (Pstr_eval (x0, x1))
          | Pstr_value (x0, x1) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#value_binding _ctx x1 in
            Structure_item_desc.of_concrete (Pstr_value (x0, x1))
          | Pstr_primitive x0 ->
            let x0 = self#value_description _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_primitive x0)
          | Pstr_type (x0, x1) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#type_declaration _ctx x1 in
            Structure_item_desc.of_concrete (Pstr_type (x0, x1))
          | Pstr_typext x0 ->
            let x0 = self#type_extension _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_typext x0)
          | Pstr_exception x0 ->
            let x0 = self#extension_constructor _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_exception x0)
          | Pstr_module x0 ->
            let x0 = self#module_binding _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_module x0)
          | Pstr_recmodule x0 ->
            let x0 = self#list self#module_binding _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_recmodule x0)
          | Pstr_modtype x0 ->
            let x0 = self#module_type_declaration _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_modtype x0)
          | Pstr_open x0 ->
            let x0 = self#open_description _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_open x0)
          | Pstr_class x0 ->
            let x0 = self#list self#class_declaration _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_class x0)
          | Pstr_class_type x0 ->
            let x0 = self#list self#class_type_declaration _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_class_type x0)
          | Pstr_include x0 ->
            let x0 = self#include_declaration _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_include x0)
          | Pstr_attribute x0 ->
            let x0 = self#attribute _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_attribute x0)
          | Pstr_extension (x0, x1) ->
            let x0 = self#extension _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            Structure_item_desc.of_concrete (Pstr_extension (x0, x1))
      method value_binding : 'ctx -> Value_binding.t -> Value_binding.t  =
        fun _ctx value_binding ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let pvb_pat = self#pattern _ctx pvb_pat in
          let pvb_expr = self#expression _ctx pvb_expr in
          let pvb_attributes = self#attributes _ctx pvb_attributes in
          let pvb_loc = self#location _ctx pvb_loc in
          Value_binding.of_concrete { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
      method module_binding : 'ctx -> Module_binding.t -> Module_binding.t  =
        fun _ctx module_binding ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let pmb_name = self#loc self#string _ctx pmb_name in
          let pmb_expr = self#module_expr _ctx pmb_expr in
          let pmb_attributes = self#attributes _ctx pmb_attributes in
          let pmb_loc = self#location _ctx pmb_loc in
          Module_binding.of_concrete { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
      method toplevel_phrase : 'ctx -> Toplevel_phrase.t -> Toplevel_phrase.t  =
        fun _ctx toplevel_phrase ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let x0 = self#structure _ctx x0 in
            Toplevel_phrase.of_concrete (Ptop_def x0)
          | Ptop_dir (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#directive_argument _ctx x1 in
            Toplevel_phrase.of_concrete (Ptop_dir (x0, x1))
      method directive_argument : 'ctx -> Directive_argument.t -> Directive_argument.t  =
        fun _ctx directive_argument ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            Directive_argument.of_concrete Pdir_none
          | Pdir_string x0 ->
            let x0 = self#string _ctx x0 in
            Directive_argument.of_concrete (Pdir_string x0)
          | Pdir_int (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#option self#char _ctx x1 in
            Directive_argument.of_concrete (Pdir_int (x0, x1))
          | Pdir_ident x0 ->
            let x0 = self#longident _ctx x0 in
            Directive_argument.of_concrete (Pdir_ident x0)
          | Pdir_bool x0 ->
            let x0 = self#bool _ctx x0 in
            Directive_argument.of_concrete (Pdir_bool x0)
    end

  class virtual ['res] lift =
    object (self)
      method virtual record : (string * 'res) list -> 'res
      method virtual constr : string -> 'res list -> 'res
      method virtual tuple : 'res list -> 'res
      method virtual bool : bool -> 'res
      method virtual char : char -> 'res
      method virtual int : int -> 'res
      method virtual list : 'a . ('a -> 'res) -> 'a list -> 'res
      method virtual option : 'a . ('a -> 'res) -> 'a option -> 'res
      method virtual string : string -> 'res
      method virtual location : Astlib.Location.t -> 'res
      method virtual loc : 'a . ('a -> 'res) -> 'a Astlib.Loc.t -> 'res
      method longident : Longident.t -> 'res  =
        fun longident ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let x0 = self#string x0 in
            self#constr "Lident" [x0]
          | Ldot (x0, x1) ->
            let x0 = self#longident x0 in
            let x1 = self#string x1 in
            self#constr "Ldot" [x0; x1]
          | Lapply (x0, x1) ->
            let x0 = self#longident x0 in
            let x1 = self#longident x1 in
            self#constr "Lapply" [x0; x1]
      method longident_loc : Longident_loc.t -> 'res  =
        fun longident_loc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let concrete = self#loc self#longident concrete in
          concrete
      method rec_flag : Rec_flag.t -> 'res  =
        fun rec_flag ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            self#constr "Nonrecursive" []
          | Recursive ->
            self#constr "Recursive" []
      method direction_flag : Direction_flag.t -> 'res  =
        fun direction_flag ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            self#constr "Upto" []
          | Downto ->
            self#constr "Downto" []
      method private_flag : Private_flag.t -> 'res  =
        fun private_flag ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            self#constr "Private" []
          | Public ->
            self#constr "Public" []
      method mutable_flag : Mutable_flag.t -> 'res  =
        fun mutable_flag ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            self#constr "Immutable" []
          | Mutable ->
            self#constr "Mutable" []
      method virtual_flag : Virtual_flag.t -> 'res  =
        fun virtual_flag ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            self#constr "Virtual" []
          | Concrete ->
            self#constr "Concrete" []
      method override_flag : Override_flag.t -> 'res  =
        fun override_flag ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            self#constr "Override" []
          | Fresh ->
            self#constr "Fresh" []
      method closed_flag : Closed_flag.t -> 'res  =
        fun closed_flag ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            self#constr "Closed" []
          | Open ->
            self#constr "Open" []
      method label : Label.t -> 'res  =
        fun label ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let concrete = self#string concrete in
          concrete
      method arg_label : Arg_label.t -> 'res  =
        fun arg_label ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            self#constr "Nolabel" []
          | Labelled x0 ->
            let x0 = self#string x0 in
            self#constr "Labelled" [x0]
          | Optional x0 ->
            let x0 = self#string x0 in
            self#constr "Optional" [x0]
      method variance : Variance.t -> 'res  =
        fun variance ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            self#constr "Covariant" []
          | Contravariant ->
            self#constr "Contravariant" []
          | Invariant ->
            self#constr "Invariant" []
      method constant : Constant.t -> 'res  =
        fun constant ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            self#constr "Pconst_integer" [x0; x1]
          | Pconst_char x0 ->
            let x0 = self#char x0 in
            self#constr "Pconst_char" [x0]
          | Pconst_string (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#string x1 in
            self#constr "Pconst_string" [x0; x1]
          | Pconst_float (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            self#constr "Pconst_float" [x0; x1]
      method attribute : Attribute.t -> 'res  =
        fun attribute ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string x0 in
          let x1 = self#payload x1 in
          self#tuple [x0; x1]
      method extension : Extension.t -> 'res  =
        fun extension ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string x0 in
          let x1 = self#payload x1 in
          self#tuple [x0; x1]
      method attributes : Attributes.t -> 'res  =
        fun attributes ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let concrete = self#list self#attribute concrete in
          concrete
      method payload : Payload.t -> 'res  =
        fun payload ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let x0 = self#structure x0 in
            self#constr "PStr" [x0]
          | PSig x0 ->
            let x0 = self#signature x0 in
            self#constr "PSig" [x0]
          | PTyp x0 ->
            let x0 = self#core_type x0 in
            self#constr "PTyp" [x0]
          | PPat (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#option self#expression x1 in
            self#constr "PPat" [x0; x1]
      method core_type : Core_type.t -> 'res  =
        fun core_type ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let ptyp_desc = self#core_type_desc ptyp_desc in
          let ptyp_loc = self#location ptyp_loc in
          let ptyp_attributes = self#attributes ptyp_attributes in
          self#record [("ptyp_desc", ptyp_desc); ("ptyp_loc", ptyp_loc); ("ptyp_attributes", ptyp_attributes)]
      method core_type_desc : Core_type_desc.t -> 'res  =
        fun core_type_desc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            self#constr "Ptyp_any" []
          | Ptyp_var x0 ->
            let x0 = self#string x0 in
            self#constr "Ptyp_var" [x0]
          | Ptyp_arrow (x0, x1, x2) ->
            let x0 = self#arg_label x0 in
            let x1 = self#core_type x1 in
            let x2 = self#core_type x2 in
            self#constr "Ptyp_arrow" [x0; x1; x2]
          | Ptyp_tuple x0 ->
            let x0 = self#list self#core_type x0 in
            self#constr "Ptyp_tuple" [x0]
          | Ptyp_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            self#constr "Ptyp_constr" [x0; x1]
          | Ptyp_object (x0, x1) ->
            let x0 = self#list self#object_field x0 in
            let x1 = self#closed_flag x1 in
            self#constr "Ptyp_object" [x0; x1]
          | Ptyp_class (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            self#constr "Ptyp_class" [x0; x1]
          | Ptyp_alias (x0, x1) ->
            let x0 = self#core_type x0 in
            let x1 = self#string x1 in
            self#constr "Ptyp_alias" [x0; x1]
          | Ptyp_variant (x0, x1, x2) ->
            let x0 = self#list self#row_field x0 in
            let x1 = self#closed_flag x1 in
            let x2 = self#option (self#list self#label) x2 in
            self#constr "Ptyp_variant" [x0; x1; x2]
          | Ptyp_poly (x0, x1) ->
            let x0 = self#list (self#loc self#string) x0 in
            let x1 = self#core_type x1 in
            self#constr "Ptyp_poly" [x0; x1]
          | Ptyp_package x0 ->
            let x0 = self#package_type x0 in
            self#constr "Ptyp_package" [x0]
          | Ptyp_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Ptyp_extension" [x0]
      method package_type : Package_type.t -> 'res  =
        fun package_type ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#longident_loc x0 in
          let x1 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#core_type x1 in self#tuple [x0; x1]) x1 in
          self#tuple [x0; x1]
      method row_field : Row_field.t -> 'res  =
        fun row_field ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#attributes x1 in
            let x2 = self#bool x2 in
            let x3 = self#list self#core_type x3 in
            self#constr "Rtag" [x0; x1; x2; x3]
          | Rinherit x0 ->
            let x0 = self#core_type x0 in
            self#constr "Rinherit" [x0]
      method object_field : Object_field.t -> 'res  =
        fun object_field ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#attributes x1 in
            let x2 = self#core_type x2 in
            self#constr "Otag" [x0; x1; x2]
          | Oinherit x0 ->
            let x0 = self#core_type x0 in
            self#constr "Oinherit" [x0]
      method pattern : Pattern.t -> 'res  =
        fun pattern ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let ppat_desc = self#pattern_desc ppat_desc in
          let ppat_loc = self#location ppat_loc in
          let ppat_attributes = self#attributes ppat_attributes in
          self#record [("ppat_desc", ppat_desc); ("ppat_loc", ppat_loc); ("ppat_attributes", ppat_attributes)]
      method pattern_desc : Pattern_desc.t -> 'res  =
        fun pattern_desc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            self#constr "Ppat_any" []
          | Ppat_var x0 ->
            let x0 = self#loc self#string x0 in
            self#constr "Ppat_var" [x0]
          | Ppat_alias (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#loc self#string x1 in
            self#constr "Ppat_alias" [x0; x1]
          | Ppat_constant x0 ->
            let x0 = self#constant x0 in
            self#constr "Ppat_constant" [x0]
          | Ppat_interval (x0, x1) ->
            let x0 = self#constant x0 in
            let x1 = self#constant x1 in
            self#constr "Ppat_interval" [x0; x1]
          | Ppat_tuple x0 ->
            let x0 = self#list self#pattern x0 in
            self#constr "Ppat_tuple" [x0]
          | Ppat_construct (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#option self#pattern x1 in
            self#constr "Ppat_construct" [x0; x1]
          | Ppat_variant (x0, x1) ->
            let x0 = self#label x0 in
            let x1 = self#option self#pattern x1 in
            self#constr "Ppat_variant" [x0; x1]
          | Ppat_record (x0, x1) ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#pattern x1 in self#tuple [x0; x1]) x0 in
            let x1 = self#closed_flag x1 in
            self#constr "Ppat_record" [x0; x1]
          | Ppat_array x0 ->
            let x0 = self#list self#pattern x0 in
            self#constr "Ppat_array" [x0]
          | Ppat_or (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#pattern x1 in
            self#constr "Ppat_or" [x0; x1]
          | Ppat_constraint (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#core_type x1 in
            self#constr "Ppat_constraint" [x0; x1]
          | Ppat_type x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Ppat_type" [x0]
          | Ppat_lazy x0 ->
            let x0 = self#pattern x0 in
            self#constr "Ppat_lazy" [x0]
          | Ppat_unpack x0 ->
            let x0 = self#loc self#string x0 in
            self#constr "Ppat_unpack" [x0]
          | Ppat_exception x0 ->
            let x0 = self#pattern x0 in
            self#constr "Ppat_exception" [x0]
          | Ppat_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Ppat_extension" [x0]
          | Ppat_open (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#pattern x1 in
            self#constr "Ppat_open" [x0; x1]
      method expression : Expression.t -> 'res  =
        fun expression ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let pexp_desc = self#expression_desc pexp_desc in
          let pexp_loc = self#location pexp_loc in
          let pexp_attributes = self#attributes pexp_attributes in
          self#record [("pexp_desc", pexp_desc); ("pexp_loc", pexp_loc); ("pexp_attributes", pexp_attributes)]
      method expression_desc : Expression_desc.t -> 'res  =
        fun expression_desc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pexp_ident" [x0]
          | Pexp_constant x0 ->
            let x0 = self#constant x0 in
            self#constr "Pexp_constant" [x0]
          | Pexp_let (x0, x1, x2) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            let x2 = self#expression x2 in
            self#constr "Pexp_let" [x0; x1; x2]
          | Pexp_function x0 ->
            let x0 = self#list self#case x0 in
            self#constr "Pexp_function" [x0]
          | Pexp_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label x0 in
            let x1 = self#option self#expression x1 in
            let x2 = self#pattern x2 in
            let x3 = self#expression x3 in
            self#constr "Pexp_fun" [x0; x1; x2; x3]
          | Pexp_apply (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list (fun (x0, x1) -> let x0 = self#arg_label x0 in let x1 = self#expression x1 in self#tuple [x0; x1]) x1 in
            self#constr "Pexp_apply" [x0; x1]
          | Pexp_match (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list self#case x1 in
            self#constr "Pexp_match" [x0; x1]
          | Pexp_try (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list self#case x1 in
            self#constr "Pexp_try" [x0; x1]
          | Pexp_tuple x0 ->
            let x0 = self#list self#expression x0 in
            self#constr "Pexp_tuple" [x0]
          | Pexp_construct (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#option self#expression x1 in
            self#constr "Pexp_construct" [x0; x1]
          | Pexp_variant (x0, x1) ->
            let x0 = self#label x0 in
            let x1 = self#option self#expression x1 in
            self#constr "Pexp_variant" [x0; x1]
          | Pexp_record (x0, x1) ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#expression x1 in self#tuple [x0; x1]) x0 in
            let x1 = self#option self#expression x1 in
            self#constr "Pexp_record" [x0; x1]
          | Pexp_field (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#longident_loc x1 in
            self#constr "Pexp_field" [x0; x1]
          | Pexp_setfield (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#expression x2 in
            self#constr "Pexp_setfield" [x0; x1; x2]
          | Pexp_array x0 ->
            let x0 = self#list self#expression x0 in
            self#constr "Pexp_array" [x0]
          | Pexp_ifthenelse (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            let x2 = self#option self#expression x2 in
            self#constr "Pexp_ifthenelse" [x0; x1; x2]
          | Pexp_sequence (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_sequence" [x0; x1]
          | Pexp_while (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_while" [x0; x1]
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let x0 = self#pattern x0 in
            let x1 = self#expression x1 in
            let x2 = self#expression x2 in
            let x3 = self#direction_flag x3 in
            let x4 = self#expression x4 in
            self#constr "Pexp_for" [x0; x1; x2; x3; x4]
          | Pexp_constraint (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#core_type x1 in
            self#constr "Pexp_constraint" [x0; x1]
          | Pexp_coerce (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#option self#core_type x1 in
            let x2 = self#core_type x2 in
            self#constr "Pexp_coerce" [x0; x1; x2]
          | Pexp_send (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#loc self#label x1 in
            self#constr "Pexp_send" [x0; x1]
          | Pexp_new x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pexp_new" [x0]
          | Pexp_setinstvar (x0, x1) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_setinstvar" [x0; x1]
          | Pexp_override x0 ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#loc self#label x0 in let x1 = self#expression x1 in self#tuple [x0; x1]) x0 in
            self#constr "Pexp_override" [x0]
          | Pexp_letmodule (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#module_expr x1 in
            let x2 = self#expression x2 in
            self#constr "Pexp_letmodule" [x0; x1; x2]
          | Pexp_letexception (x0, x1) ->
            let x0 = self#extension_constructor x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_letexception" [x0; x1]
          | Pexp_assert x0 ->
            let x0 = self#expression x0 in
            self#constr "Pexp_assert" [x0]
          | Pexp_lazy x0 ->
            let x0 = self#expression x0 in
            self#constr "Pexp_lazy" [x0]
          | Pexp_poly (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#option self#core_type x1 in
            self#constr "Pexp_poly" [x0; x1]
          | Pexp_object x0 ->
            let x0 = self#class_structure x0 in
            self#constr "Pexp_object" [x0]
          | Pexp_newtype (x0, x1) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_newtype" [x0; x1]
          | Pexp_pack x0 ->
            let x0 = self#module_expr x0 in
            self#constr "Pexp_pack" [x0]
          | Pexp_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#expression x2 in
            self#constr "Pexp_open" [x0; x1; x2]
          | Pexp_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pexp_extension" [x0]
          | Pexp_unreachable ->
            self#constr "Pexp_unreachable" []
      method case : Case.t -> 'res  =
        fun case ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let pc_lhs = self#pattern pc_lhs in
          let pc_guard = self#option self#expression pc_guard in
          let pc_rhs = self#expression pc_rhs in
          self#record [("pc_lhs", pc_lhs); ("pc_guard", pc_guard); ("pc_rhs", pc_rhs)]
      method value_description : Value_description.t -> 'res  =
        fun value_description ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let pval_name = self#loc self#string pval_name in
          let pval_type = self#core_type pval_type in
          let pval_prim = self#list self#string pval_prim in
          let pval_attributes = self#attributes pval_attributes in
          let pval_loc = self#location pval_loc in
          self#record [("pval_name", pval_name); ("pval_type", pval_type); ("pval_prim", pval_prim); ("pval_attributes", pval_attributes); ("pval_loc", pval_loc)]
      method type_declaration : Type_declaration.t -> 'res  =
        fun type_declaration ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let ptype_name = self#loc self#string ptype_name in
          let ptype_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in self#tuple [x0; x1]) ptype_params in
          let ptype_cstrs = self#list (fun (x0, x1, x2) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in let x2 = self#location x2 in self#tuple [x0; x1; x2]) ptype_cstrs in
          let ptype_kind = self#type_kind ptype_kind in
          let ptype_private = self#private_flag ptype_private in
          let ptype_manifest = self#option self#core_type ptype_manifest in
          let ptype_attributes = self#attributes ptype_attributes in
          let ptype_loc = self#location ptype_loc in
          self#record [("ptype_name", ptype_name); ("ptype_params", ptype_params); ("ptype_cstrs", ptype_cstrs); ("ptype_kind", ptype_kind); ("ptype_private", ptype_private); ("ptype_manifest", ptype_manifest); ("ptype_attributes", ptype_attributes); ("ptype_loc", ptype_loc)]
      method type_kind : Type_kind.t -> 'res  =
        fun type_kind ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            self#constr "Ptype_abstract" []
          | Ptype_variant x0 ->
            let x0 = self#list self#constructor_declaration x0 in
            self#constr "Ptype_variant" [x0]
          | Ptype_record x0 ->
            let x0 = self#list self#label_declaration x0 in
            self#constr "Ptype_record" [x0]
          | Ptype_open ->
            self#constr "Ptype_open" []
      method label_declaration : Label_declaration.t -> 'res  =
        fun label_declaration ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let pld_name = self#loc self#string pld_name in
          let pld_mutable = self#mutable_flag pld_mutable in
          let pld_type = self#core_type pld_type in
          let pld_loc = self#location pld_loc in
          let pld_attributes = self#attributes pld_attributes in
          self#record [("pld_name", pld_name); ("pld_mutable", pld_mutable); ("pld_type", pld_type); ("pld_loc", pld_loc); ("pld_attributes", pld_attributes)]
      method constructor_declaration : Constructor_declaration.t -> 'res  =
        fun constructor_declaration ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let pcd_name = self#loc self#string pcd_name in
          let pcd_args = self#constructor_arguments pcd_args in
          let pcd_res = self#option self#core_type pcd_res in
          let pcd_loc = self#location pcd_loc in
          let pcd_attributes = self#attributes pcd_attributes in
          self#record [("pcd_name", pcd_name); ("pcd_args", pcd_args); ("pcd_res", pcd_res); ("pcd_loc", pcd_loc); ("pcd_attributes", pcd_attributes)]
      method constructor_arguments : Constructor_arguments.t -> 'res  =
        fun constructor_arguments ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let x0 = self#list self#core_type x0 in
            self#constr "Pcstr_tuple" [x0]
          | Pcstr_record x0 ->
            let x0 = self#list self#label_declaration x0 in
            self#constr "Pcstr_record" [x0]
      method type_extension : Type_extension.t -> 'res  =
        fun type_extension ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let ptyext_path = self#longident_loc ptyext_path in
          let ptyext_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in self#tuple [x0; x1]) ptyext_params in
          let ptyext_constructors = self#list self#extension_constructor ptyext_constructors in
          let ptyext_private = self#private_flag ptyext_private in
          let ptyext_attributes = self#attributes ptyext_attributes in
          self#record [("ptyext_path", ptyext_path); ("ptyext_params", ptyext_params); ("ptyext_constructors", ptyext_constructors); ("ptyext_private", ptyext_private); ("ptyext_attributes", ptyext_attributes)]
      method extension_constructor : Extension_constructor.t -> 'res  =
        fun extension_constructor ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let pext_name = self#loc self#string pext_name in
          let pext_kind = self#extension_constructor_kind pext_kind in
          let pext_loc = self#location pext_loc in
          let pext_attributes = self#attributes pext_attributes in
          self#record [("pext_name", pext_name); ("pext_kind", pext_kind); ("pext_loc", pext_loc); ("pext_attributes", pext_attributes)]
      method extension_constructor_kind : Extension_constructor_kind.t -> 'res  =
        fun extension_constructor_kind ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let x0 = self#constructor_arguments x0 in
            let x1 = self#option self#core_type x1 in
            self#constr "Pext_decl" [x0; x1]
          | Pext_rebind x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pext_rebind" [x0]
      method class_type : Class_type.t -> 'res  =
        fun class_type ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let pcty_desc = self#class_type_desc pcty_desc in
          let pcty_loc = self#location pcty_loc in
          let pcty_attributes = self#attributes pcty_attributes in
          self#record [("pcty_desc", pcty_desc); ("pcty_loc", pcty_loc); ("pcty_attributes", pcty_attributes)]
      method class_type_desc : Class_type_desc.t -> 'res  =
        fun class_type_desc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            self#constr "Pcty_constr" [x0; x1]
          | Pcty_signature x0 ->
            let x0 = self#class_signature x0 in
            self#constr "Pcty_signature" [x0]
          | Pcty_arrow (x0, x1, x2) ->
            let x0 = self#arg_label x0 in
            let x1 = self#core_type x1 in
            let x2 = self#class_type x2 in
            self#constr "Pcty_arrow" [x0; x1; x2]
          | Pcty_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pcty_extension" [x0]
          | Pcty_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#class_type x2 in
            self#constr "Pcty_open" [x0; x1; x2]
      method class_signature : Class_signature.t -> 'res  =
        fun class_signature ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let pcsig_self = self#core_type pcsig_self in
          let pcsig_fields = self#list self#class_type_field pcsig_fields in
          self#record [("pcsig_self", pcsig_self); ("pcsig_fields", pcsig_fields)]
      method class_type_field : Class_type_field.t -> 'res  =
        fun class_type_field ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let pctf_desc = self#class_type_field_desc pctf_desc in
          let pctf_loc = self#location pctf_loc in
          let pctf_attributes = self#attributes pctf_attributes in
          self#record [("pctf_desc", pctf_desc); ("pctf_loc", pctf_loc); ("pctf_attributes", pctf_attributes)]
      method class_type_field_desc : Class_type_field_desc.t -> 'res  =
        fun class_type_field_desc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let x0 = self#class_type x0 in
            self#constr "Pctf_inherit" [x0]
          | Pctf_val x0 ->
            let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#loc self#label x0 in let x1 = self#mutable_flag x1 in let x2 = self#virtual_flag x2 in let x3 = self#core_type x3 in self#tuple [x0; x1; x2; x3]) x0 in
            self#constr "Pctf_val" [x0]
          | Pctf_method x0 ->
            let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#loc self#label x0 in let x1 = self#private_flag x1 in let x2 = self#virtual_flag x2 in let x3 = self#core_type x3 in self#tuple [x0; x1; x2; x3]) x0 in
            self#constr "Pctf_method" [x0]
          | Pctf_constraint x0 ->
            let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in self#tuple [x0; x1]) x0 in
            self#constr "Pctf_constraint" [x0]
          | Pctf_attribute x0 ->
            let x0 = self#attribute x0 in
            self#constr "Pctf_attribute" [x0]
          | Pctf_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pctf_extension" [x0]
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'res  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag pci_virt in
          let pci_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in self#tuple [x0; x1]) pci_params in
          let pci_name = self#loc self#string pci_name in
          let pci_expr = self#class_expr pci_expr in
          let pci_loc = self#location pci_loc in
          let pci_attributes = self#attributes pci_attributes in
          self#record [("pci_virt", pci_virt); ("pci_params", pci_params); ("pci_name", pci_name); ("pci_expr", pci_expr); ("pci_loc", pci_loc); ("pci_attributes", pci_attributes)]
      method class_infos_class_type : Class_type.t Class_infos.t -> 'res  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag pci_virt in
          let pci_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in self#tuple [x0; x1]) pci_params in
          let pci_name = self#loc self#string pci_name in
          let pci_expr = self#class_type pci_expr in
          let pci_loc = self#location pci_loc in
          let pci_attributes = self#attributes pci_attributes in
          self#record [("pci_virt", pci_virt); ("pci_params", pci_params); ("pci_name", pci_name); ("pci_expr", pci_expr); ("pci_loc", pci_loc); ("pci_attributes", pci_attributes)]
      method class_description : Class_description.t -> 'res  =
        fun class_description ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type concrete in
          concrete
      method class_type_declaration : Class_type_declaration.t -> 'res  =
        fun class_type_declaration ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type concrete in
          concrete
      method class_expr : Class_expr.t -> 'res  =
        fun class_expr ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let pcl_desc = self#class_expr_desc pcl_desc in
          let pcl_loc = self#location pcl_loc in
          let pcl_attributes = self#attributes pcl_attributes in
          self#record [("pcl_desc", pcl_desc); ("pcl_loc", pcl_loc); ("pcl_attributes", pcl_attributes)]
      method class_expr_desc : Class_expr_desc.t -> 'res  =
        fun class_expr_desc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            self#constr "Pcl_constr" [x0; x1]
          | Pcl_structure x0 ->
            let x0 = self#class_structure x0 in
            self#constr "Pcl_structure" [x0]
          | Pcl_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label x0 in
            let x1 = self#option self#expression x1 in
            let x2 = self#pattern x2 in
            let x3 = self#class_expr x3 in
            self#constr "Pcl_fun" [x0; x1; x2; x3]
          | Pcl_apply (x0, x1) ->
            let x0 = self#class_expr x0 in
            let x1 = self#list (fun (x0, x1) -> let x0 = self#arg_label x0 in let x1 = self#expression x1 in self#tuple [x0; x1]) x1 in
            self#constr "Pcl_apply" [x0; x1]
          | Pcl_let (x0, x1, x2) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            let x2 = self#class_expr x2 in
            self#constr "Pcl_let" [x0; x1; x2]
          | Pcl_constraint (x0, x1) ->
            let x0 = self#class_expr x0 in
            let x1 = self#class_type x1 in
            self#constr "Pcl_constraint" [x0; x1]
          | Pcl_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pcl_extension" [x0]
          | Pcl_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#class_expr x2 in
            self#constr "Pcl_open" [x0; x1; x2]
      method class_structure : Class_structure.t -> 'res  =
        fun class_structure ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let pcstr_self = self#pattern pcstr_self in
          let pcstr_fields = self#list self#class_field pcstr_fields in
          self#record [("pcstr_self", pcstr_self); ("pcstr_fields", pcstr_fields)]
      method class_field : Class_field.t -> 'res  =
        fun class_field ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let pcf_desc = self#class_field_desc pcf_desc in
          let pcf_loc = self#location pcf_loc in
          let pcf_attributes = self#attributes pcf_attributes in
          self#record [("pcf_desc", pcf_desc); ("pcf_loc", pcf_loc); ("pcf_attributes", pcf_attributes)]
      method class_field_desc : Class_field_desc.t -> 'res  =
        fun class_field_desc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#class_expr x1 in
            let x2 = self#option (self#loc self#string) x2 in
            self#constr "Pcf_inherit" [x0; x1; x2]
          | Pcf_val x0 ->
            let x0 = (fun (x0, x1, x2) -> let x0 = self#loc self#label x0 in let x1 = self#mutable_flag x1 in let x2 = self#class_field_kind x2 in self#tuple [x0; x1; x2]) x0 in
            self#constr "Pcf_val" [x0]
          | Pcf_method x0 ->
            let x0 = (fun (x0, x1, x2) -> let x0 = self#loc self#label x0 in let x1 = self#private_flag x1 in let x2 = self#class_field_kind x2 in self#tuple [x0; x1; x2]) x0 in
            self#constr "Pcf_method" [x0]
          | Pcf_constraint x0 ->
            let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in self#tuple [x0; x1]) x0 in
            self#constr "Pcf_constraint" [x0]
          | Pcf_initializer x0 ->
            let x0 = self#expression x0 in
            self#constr "Pcf_initializer" [x0]
          | Pcf_attribute x0 ->
            let x0 = self#attribute x0 in
            self#constr "Pcf_attribute" [x0]
          | Pcf_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pcf_extension" [x0]
      method class_field_kind : Class_field_kind.t -> 'res  =
        fun class_field_kind ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let x0 = self#core_type x0 in
            self#constr "Cfk_virtual" [x0]
          | Cfk_concrete (x0, x1) ->
            let x0 = self#override_flag x0 in
            let x1 = self#expression x1 in
            self#constr "Cfk_concrete" [x0; x1]
      method class_declaration : Class_declaration.t -> 'res  =
        fun class_declaration ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_expr concrete in
          concrete
      method module_type : Module_type.t -> 'res  =
        fun module_type ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let pmty_desc = self#module_type_desc pmty_desc in
          let pmty_loc = self#location pmty_loc in
          let pmty_attributes = self#attributes pmty_attributes in
          self#record [("pmty_desc", pmty_desc); ("pmty_loc", pmty_loc); ("pmty_attributes", pmty_attributes)]
      method module_type_desc : Module_type_desc.t -> 'res  =
        fun module_type_desc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pmty_ident" [x0]
          | Pmty_signature x0 ->
            let x0 = self#signature x0 in
            self#constr "Pmty_signature" [x0]
          | Pmty_functor (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#option self#module_type x1 in
            let x2 = self#module_type x2 in
            self#constr "Pmty_functor" [x0; x1; x2]
          | Pmty_with (x0, x1) ->
            let x0 = self#module_type x0 in
            let x1 = self#list self#with_constraint x1 in
            self#constr "Pmty_with" [x0; x1]
          | Pmty_typeof x0 ->
            let x0 = self#module_expr x0 in
            self#constr "Pmty_typeof" [x0]
          | Pmty_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pmty_extension" [x0]
          | Pmty_alias x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pmty_alias" [x0]
      method signature : Signature.t -> 'res  =
        fun signature ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let concrete = self#list self#signature_item concrete in
          concrete
      method signature_item : Signature_item.t -> 'res  =
        fun signature_item ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let psig_desc = self#signature_item_desc psig_desc in
          let psig_loc = self#location psig_loc in
          self#record [("psig_desc", psig_desc); ("psig_loc", psig_loc)]
      method signature_item_desc : Signature_item_desc.t -> 'res  =
        fun signature_item_desc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let x0 = self#value_description x0 in
            self#constr "Psig_value" [x0]
          | Psig_type (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#type_declaration x1 in
            self#constr "Psig_type" [x0; x1]
          | Psig_typext x0 ->
            let x0 = self#type_extension x0 in
            self#constr "Psig_typext" [x0]
          | Psig_exception x0 ->
            let x0 = self#extension_constructor x0 in
            self#constr "Psig_exception" [x0]
          | Psig_module x0 ->
            let x0 = self#module_declaration x0 in
            self#constr "Psig_module" [x0]
          | Psig_recmodule x0 ->
            let x0 = self#list self#module_declaration x0 in
            self#constr "Psig_recmodule" [x0]
          | Psig_modtype x0 ->
            let x0 = self#module_type_declaration x0 in
            self#constr "Psig_modtype" [x0]
          | Psig_open x0 ->
            let x0 = self#open_description x0 in
            self#constr "Psig_open" [x0]
          | Psig_include x0 ->
            let x0 = self#include_description x0 in
            self#constr "Psig_include" [x0]
          | Psig_class x0 ->
            let x0 = self#list self#class_description x0 in
            self#constr "Psig_class" [x0]
          | Psig_class_type x0 ->
            let x0 = self#list self#class_type_declaration x0 in
            self#constr "Psig_class_type" [x0]
          | Psig_attribute x0 ->
            let x0 = self#attribute x0 in
            self#constr "Psig_attribute" [x0]
          | Psig_extension (x0, x1) ->
            let x0 = self#extension x0 in
            let x1 = self#attributes x1 in
            self#constr "Psig_extension" [x0; x1]
      method module_declaration : Module_declaration.t -> 'res  =
        fun module_declaration ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let pmd_name = self#loc self#string pmd_name in
          let pmd_type = self#module_type pmd_type in
          let pmd_attributes = self#attributes pmd_attributes in
          let pmd_loc = self#location pmd_loc in
          self#record [("pmd_name", pmd_name); ("pmd_type", pmd_type); ("pmd_attributes", pmd_attributes); ("pmd_loc", pmd_loc)]
      method module_type_declaration : Module_type_declaration.t -> 'res  =
        fun module_type_declaration ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let pmtd_name = self#loc self#string pmtd_name in
          let pmtd_type = self#option self#module_type pmtd_type in
          let pmtd_attributes = self#attributes pmtd_attributes in
          let pmtd_loc = self#location pmtd_loc in
          self#record [("pmtd_name", pmtd_name); ("pmtd_type", pmtd_type); ("pmtd_attributes", pmtd_attributes); ("pmtd_loc", pmtd_loc)]
      method open_description : Open_description.t -> 'res  =
        fun open_description ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let popen_lid = self#longident_loc popen_lid in
          let popen_override = self#override_flag popen_override in
          let popen_loc = self#location popen_loc in
          let popen_attributes = self#attributes popen_attributes in
          self#record [("popen_lid", popen_lid); ("popen_override", popen_override); ("popen_loc", popen_loc); ("popen_attributes", popen_attributes)]
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'res  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_expr pincl_mod in
          let pincl_loc = self#location pincl_loc in
          let pincl_attributes = self#attributes pincl_attributes in
          self#record [("pincl_mod", pincl_mod); ("pincl_loc", pincl_loc); ("pincl_attributes", pincl_attributes)]
      method include_infos_module_type : Module_type.t Include_infos.t -> 'res  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_type pincl_mod in
          let pincl_loc = self#location pincl_loc in
          let pincl_attributes = self#attributes pincl_attributes in
          self#record [("pincl_mod", pincl_mod); ("pincl_loc", pincl_loc); ("pincl_attributes", pincl_attributes)]
      method include_description : Include_description.t -> 'res  =
        fun include_description ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let concrete = self#include_infos_module_type concrete in
          concrete
      method include_declaration : Include_declaration.t -> 'res  =
        fun include_declaration ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let concrete = self#include_infos_module_expr concrete in
          concrete
      method with_constraint : With_constraint.t -> 'res  =
        fun with_constraint ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#type_declaration x1 in
            self#constr "Pwith_type" [x0; x1]
          | Pwith_module (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#longident_loc x1 in
            self#constr "Pwith_module" [x0; x1]
          | Pwith_typesubst (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#type_declaration x1 in
            self#constr "Pwith_typesubst" [x0; x1]
          | Pwith_modsubst (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#longident_loc x1 in
            self#constr "Pwith_modsubst" [x0; x1]
      method module_expr : Module_expr.t -> 'res  =
        fun module_expr ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let pmod_desc = self#module_expr_desc pmod_desc in
          let pmod_loc = self#location pmod_loc in
          let pmod_attributes = self#attributes pmod_attributes in
          self#record [("pmod_desc", pmod_desc); ("pmod_loc", pmod_loc); ("pmod_attributes", pmod_attributes)]
      method module_expr_desc : Module_expr_desc.t -> 'res  =
        fun module_expr_desc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pmod_ident" [x0]
          | Pmod_structure x0 ->
            let x0 = self#structure x0 in
            self#constr "Pmod_structure" [x0]
          | Pmod_functor (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#option self#module_type x1 in
            let x2 = self#module_expr x2 in
            self#constr "Pmod_functor" [x0; x1; x2]
          | Pmod_apply (x0, x1) ->
            let x0 = self#module_expr x0 in
            let x1 = self#module_expr x1 in
            self#constr "Pmod_apply" [x0; x1]
          | Pmod_constraint (x0, x1) ->
            let x0 = self#module_expr x0 in
            let x1 = self#module_type x1 in
            self#constr "Pmod_constraint" [x0; x1]
          | Pmod_unpack x0 ->
            let x0 = self#expression x0 in
            self#constr "Pmod_unpack" [x0]
          | Pmod_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pmod_extension" [x0]
      method structure : Structure.t -> 'res  =
        fun structure ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let concrete = self#list self#structure_item concrete in
          concrete
      method structure_item : Structure_item.t -> 'res  =
        fun structure_item ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let pstr_desc = self#structure_item_desc pstr_desc in
          let pstr_loc = self#location pstr_loc in
          self#record [("pstr_desc", pstr_desc); ("pstr_loc", pstr_loc)]
      method structure_item_desc : Structure_item_desc.t -> 'res  =
        fun structure_item_desc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#attributes x1 in
            self#constr "Pstr_eval" [x0; x1]
          | Pstr_value (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            self#constr "Pstr_value" [x0; x1]
          | Pstr_primitive x0 ->
            let x0 = self#value_description x0 in
            self#constr "Pstr_primitive" [x0]
          | Pstr_type (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#type_declaration x1 in
            self#constr "Pstr_type" [x0; x1]
          | Pstr_typext x0 ->
            let x0 = self#type_extension x0 in
            self#constr "Pstr_typext" [x0]
          | Pstr_exception x0 ->
            let x0 = self#extension_constructor x0 in
            self#constr "Pstr_exception" [x0]
          | Pstr_module x0 ->
            let x0 = self#module_binding x0 in
            self#constr "Pstr_module" [x0]
          | Pstr_recmodule x0 ->
            let x0 = self#list self#module_binding x0 in
            self#constr "Pstr_recmodule" [x0]
          | Pstr_modtype x0 ->
            let x0 = self#module_type_declaration x0 in
            self#constr "Pstr_modtype" [x0]
          | Pstr_open x0 ->
            let x0 = self#open_description x0 in
            self#constr "Pstr_open" [x0]
          | Pstr_class x0 ->
            let x0 = self#list self#class_declaration x0 in
            self#constr "Pstr_class" [x0]
          | Pstr_class_type x0 ->
            let x0 = self#list self#class_type_declaration x0 in
            self#constr "Pstr_class_type" [x0]
          | Pstr_include x0 ->
            let x0 = self#include_declaration x0 in
            self#constr "Pstr_include" [x0]
          | Pstr_attribute x0 ->
            let x0 = self#attribute x0 in
            self#constr "Pstr_attribute" [x0]
          | Pstr_extension (x0, x1) ->
            let x0 = self#extension x0 in
            let x1 = self#attributes x1 in
            self#constr "Pstr_extension" [x0; x1]
      method value_binding : Value_binding.t -> 'res  =
        fun value_binding ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let pvb_pat = self#pattern pvb_pat in
          let pvb_expr = self#expression pvb_expr in
          let pvb_attributes = self#attributes pvb_attributes in
          let pvb_loc = self#location pvb_loc in
          self#record [("pvb_pat", pvb_pat); ("pvb_expr", pvb_expr); ("pvb_attributes", pvb_attributes); ("pvb_loc", pvb_loc)]
      method module_binding : Module_binding.t -> 'res  =
        fun module_binding ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let pmb_name = self#loc self#string pmb_name in
          let pmb_expr = self#module_expr pmb_expr in
          let pmb_attributes = self#attributes pmb_attributes in
          let pmb_loc = self#location pmb_loc in
          self#record [("pmb_name", pmb_name); ("pmb_expr", pmb_expr); ("pmb_attributes", pmb_attributes); ("pmb_loc", pmb_loc)]
      method toplevel_phrase : Toplevel_phrase.t -> 'res  =
        fun toplevel_phrase ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let x0 = self#structure x0 in
            self#constr "Ptop_def" [x0]
          | Ptop_dir (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#directive_argument x1 in
            self#constr "Ptop_dir" [x0; x1]
      method directive_argument : Directive_argument.t -> 'res  =
        fun directive_argument ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            self#constr "Pdir_none" []
          | Pdir_string x0 ->
            let x0 = self#string x0 in
            self#constr "Pdir_string" [x0]
          | Pdir_int (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            self#constr "Pdir_int" [x0; x1]
          | Pdir_ident x0 ->
            let x0 = self#longident x0 in
            self#constr "Pdir_ident" [x0]
          | Pdir_bool x0 ->
            let x0 = self#bool x0 in
            self#constr "Pdir_bool" [x0]
    end
end

module V4_06 = struct
  open Versions.V4_06

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
      method longident : Longident.t -> Longident.t  =
        fun longident ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let x0 = self#string x0 in
            Longident.of_concrete (Lident x0)
          | Ldot (x0, x1) ->
            let x0 = self#longident x0 in
            let x1 = self#string x1 in
            Longident.of_concrete (Ldot (x0, x1))
          | Lapply (x0, x1) ->
            let x0 = self#longident x0 in
            let x1 = self#longident x1 in
            Longident.of_concrete (Lapply (x0, x1))
      method longident_loc : Longident_loc.t -> Longident_loc.t  =
        fun longident_loc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let concrete = self#loc self#longident concrete in
          Longident_loc.of_concrete concrete
      method rec_flag : Rec_flag.t -> Rec_flag.t  =
        fun rec_flag ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            Rec_flag.of_concrete Nonrecursive
          | Recursive ->
            Rec_flag.of_concrete Recursive
      method direction_flag : Direction_flag.t -> Direction_flag.t  =
        fun direction_flag ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            Direction_flag.of_concrete Upto
          | Downto ->
            Direction_flag.of_concrete Downto
      method private_flag : Private_flag.t -> Private_flag.t  =
        fun private_flag ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            Private_flag.of_concrete Private
          | Public ->
            Private_flag.of_concrete Public
      method mutable_flag : Mutable_flag.t -> Mutable_flag.t  =
        fun mutable_flag ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            Mutable_flag.of_concrete Immutable
          | Mutable ->
            Mutable_flag.of_concrete Mutable
      method virtual_flag : Virtual_flag.t -> Virtual_flag.t  =
        fun virtual_flag ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            Virtual_flag.of_concrete Virtual
          | Concrete ->
            Virtual_flag.of_concrete Concrete
      method override_flag : Override_flag.t -> Override_flag.t  =
        fun override_flag ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            Override_flag.of_concrete Override
          | Fresh ->
            Override_flag.of_concrete Fresh
      method closed_flag : Closed_flag.t -> Closed_flag.t  =
        fun closed_flag ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            Closed_flag.of_concrete Closed
          | Open ->
            Closed_flag.of_concrete Open
      method label : Label.t -> Label.t  =
        fun label ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let concrete = self#string concrete in
          Label.of_concrete concrete
      method arg_label : Arg_label.t -> Arg_label.t  =
        fun arg_label ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            Arg_label.of_concrete Nolabel
          | Labelled x0 ->
            let x0 = self#string x0 in
            Arg_label.of_concrete (Labelled x0)
          | Optional x0 ->
            let x0 = self#string x0 in
            Arg_label.of_concrete (Optional x0)
      method variance : Variance.t -> Variance.t  =
        fun variance ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            Variance.of_concrete Covariant
          | Contravariant ->
            Variance.of_concrete Contravariant
          | Invariant ->
            Variance.of_concrete Invariant
      method constant : Constant.t -> Constant.t  =
        fun constant ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            Constant.of_concrete (Pconst_integer (x0, x1))
          | Pconst_char x0 ->
            let x0 = self#char x0 in
            Constant.of_concrete (Pconst_char x0)
          | Pconst_string (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#string x1 in
            Constant.of_concrete (Pconst_string (x0, x1))
          | Pconst_float (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            Constant.of_concrete (Pconst_float (x0, x1))
      method attribute : Attribute.t -> Attribute.t  =
        fun attribute ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string x0 in
          let x1 = self#payload x1 in
          Attribute.of_concrete (x0, x1)
      method extension : Extension.t -> Extension.t  =
        fun extension ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string x0 in
          let x1 = self#payload x1 in
          Extension.of_concrete (x0, x1)
      method attributes : Attributes.t -> Attributes.t  =
        fun attributes ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let concrete = self#list self#attribute concrete in
          Attributes.of_concrete concrete
      method payload : Payload.t -> Payload.t  =
        fun payload ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let x0 = self#structure x0 in
            Payload.of_concrete (PStr x0)
          | PSig x0 ->
            let x0 = self#signature x0 in
            Payload.of_concrete (PSig x0)
          | PTyp x0 ->
            let x0 = self#core_type x0 in
            Payload.of_concrete (PTyp x0)
          | PPat (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#option self#expression x1 in
            Payload.of_concrete (PPat (x0, x1))
      method core_type : Core_type.t -> Core_type.t  =
        fun core_type ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let ptyp_desc = self#core_type_desc ptyp_desc in
          let ptyp_loc = self#location ptyp_loc in
          let ptyp_attributes = self#attributes ptyp_attributes in
          Core_type.of_concrete { ptyp_desc; ptyp_loc; ptyp_attributes }
      method core_type_desc : Core_type_desc.t -> Core_type_desc.t  =
        fun core_type_desc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            Core_type_desc.of_concrete Ptyp_any
          | Ptyp_var x0 ->
            let x0 = self#string x0 in
            Core_type_desc.of_concrete (Ptyp_var x0)
          | Ptyp_arrow (x0, x1, x2) ->
            let x0 = self#arg_label x0 in
            let x1 = self#core_type x1 in
            let x2 = self#core_type x2 in
            Core_type_desc.of_concrete (Ptyp_arrow (x0, x1, x2))
          | Ptyp_tuple x0 ->
            let x0 = self#list self#core_type x0 in
            Core_type_desc.of_concrete (Ptyp_tuple x0)
          | Ptyp_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            Core_type_desc.of_concrete (Ptyp_constr (x0, x1))
          | Ptyp_object (x0, x1) ->
            let x0 = self#list self#object_field x0 in
            let x1 = self#closed_flag x1 in
            Core_type_desc.of_concrete (Ptyp_object (x0, x1))
          | Ptyp_class (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            Core_type_desc.of_concrete (Ptyp_class (x0, x1))
          | Ptyp_alias (x0, x1) ->
            let x0 = self#core_type x0 in
            let x1 = self#string x1 in
            Core_type_desc.of_concrete (Ptyp_alias (x0, x1))
          | Ptyp_variant (x0, x1, x2) ->
            let x0 = self#list self#row_field x0 in
            let x1 = self#closed_flag x1 in
            let x2 = self#option (self#list self#label) x2 in
            Core_type_desc.of_concrete (Ptyp_variant (x0, x1, x2))
          | Ptyp_poly (x0, x1) ->
            let x0 = self#list (self#loc self#string) x0 in
            let x1 = self#core_type x1 in
            Core_type_desc.of_concrete (Ptyp_poly (x0, x1))
          | Ptyp_package x0 ->
            let x0 = self#package_type x0 in
            Core_type_desc.of_concrete (Ptyp_package x0)
          | Ptyp_extension x0 ->
            let x0 = self#extension x0 in
            Core_type_desc.of_concrete (Ptyp_extension x0)
      method package_type : Package_type.t -> Package_type.t  =
        fun package_type ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#longident_loc x0 in
          let x1 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#core_type x1 in (x0, x1)) x1 in
          Package_type.of_concrete (x0, x1)
      method row_field : Row_field.t -> Row_field.t  =
        fun row_field ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#attributes x1 in
            let x2 = self#bool x2 in
            let x3 = self#list self#core_type x3 in
            Row_field.of_concrete (Rtag (x0, x1, x2, x3))
          | Rinherit x0 ->
            let x0 = self#core_type x0 in
            Row_field.of_concrete (Rinherit x0)
      method object_field : Object_field.t -> Object_field.t  =
        fun object_field ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#attributes x1 in
            let x2 = self#core_type x2 in
            Object_field.of_concrete (Otag (x0, x1, x2))
          | Oinherit x0 ->
            let x0 = self#core_type x0 in
            Object_field.of_concrete (Oinherit x0)
      method pattern : Pattern.t -> Pattern.t  =
        fun pattern ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let ppat_desc = self#pattern_desc ppat_desc in
          let ppat_loc = self#location ppat_loc in
          let ppat_attributes = self#attributes ppat_attributes in
          Pattern.of_concrete { ppat_desc; ppat_loc; ppat_attributes }
      method pattern_desc : Pattern_desc.t -> Pattern_desc.t  =
        fun pattern_desc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            Pattern_desc.of_concrete Ppat_any
          | Ppat_var x0 ->
            let x0 = self#loc self#string x0 in
            Pattern_desc.of_concrete (Ppat_var x0)
          | Ppat_alias (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#loc self#string x1 in
            Pattern_desc.of_concrete (Ppat_alias (x0, x1))
          | Ppat_constant x0 ->
            let x0 = self#constant x0 in
            Pattern_desc.of_concrete (Ppat_constant x0)
          | Ppat_interval (x0, x1) ->
            let x0 = self#constant x0 in
            let x1 = self#constant x1 in
            Pattern_desc.of_concrete (Ppat_interval (x0, x1))
          | Ppat_tuple x0 ->
            let x0 = self#list self#pattern x0 in
            Pattern_desc.of_concrete (Ppat_tuple x0)
          | Ppat_construct (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#option self#pattern x1 in
            Pattern_desc.of_concrete (Ppat_construct (x0, x1))
          | Ppat_variant (x0, x1) ->
            let x0 = self#label x0 in
            let x1 = self#option self#pattern x1 in
            Pattern_desc.of_concrete (Ppat_variant (x0, x1))
          | Ppat_record (x0, x1) ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#pattern x1 in (x0, x1)) x0 in
            let x1 = self#closed_flag x1 in
            Pattern_desc.of_concrete (Ppat_record (x0, x1))
          | Ppat_array x0 ->
            let x0 = self#list self#pattern x0 in
            Pattern_desc.of_concrete (Ppat_array x0)
          | Ppat_or (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#pattern x1 in
            Pattern_desc.of_concrete (Ppat_or (x0, x1))
          | Ppat_constraint (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#core_type x1 in
            Pattern_desc.of_concrete (Ppat_constraint (x0, x1))
          | Ppat_type x0 ->
            let x0 = self#longident_loc x0 in
            Pattern_desc.of_concrete (Ppat_type x0)
          | Ppat_lazy x0 ->
            let x0 = self#pattern x0 in
            Pattern_desc.of_concrete (Ppat_lazy x0)
          | Ppat_unpack x0 ->
            let x0 = self#loc self#string x0 in
            Pattern_desc.of_concrete (Ppat_unpack x0)
          | Ppat_exception x0 ->
            let x0 = self#pattern x0 in
            Pattern_desc.of_concrete (Ppat_exception x0)
          | Ppat_extension x0 ->
            let x0 = self#extension x0 in
            Pattern_desc.of_concrete (Ppat_extension x0)
          | Ppat_open (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#pattern x1 in
            Pattern_desc.of_concrete (Ppat_open (x0, x1))
      method expression : Expression.t -> Expression.t  =
        fun expression ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let pexp_desc = self#expression_desc pexp_desc in
          let pexp_loc = self#location pexp_loc in
          let pexp_attributes = self#attributes pexp_attributes in
          Expression.of_concrete { pexp_desc; pexp_loc; pexp_attributes }
      method expression_desc : Expression_desc.t -> Expression_desc.t  =
        fun expression_desc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let x0 = self#longident_loc x0 in
            Expression_desc.of_concrete (Pexp_ident x0)
          | Pexp_constant x0 ->
            let x0 = self#constant x0 in
            Expression_desc.of_concrete (Pexp_constant x0)
          | Pexp_let (x0, x1, x2) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            let x2 = self#expression x2 in
            Expression_desc.of_concrete (Pexp_let (x0, x1, x2))
          | Pexp_function x0 ->
            let x0 = self#list self#case x0 in
            Expression_desc.of_concrete (Pexp_function x0)
          | Pexp_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label x0 in
            let x1 = self#option self#expression x1 in
            let x2 = self#pattern x2 in
            let x3 = self#expression x3 in
            Expression_desc.of_concrete (Pexp_fun (x0, x1, x2, x3))
          | Pexp_apply (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list (fun (x0, x1) -> let x0 = self#arg_label x0 in let x1 = self#expression x1 in (x0, x1)) x1 in
            Expression_desc.of_concrete (Pexp_apply (x0, x1))
          | Pexp_match (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list self#case x1 in
            Expression_desc.of_concrete (Pexp_match (x0, x1))
          | Pexp_try (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list self#case x1 in
            Expression_desc.of_concrete (Pexp_try (x0, x1))
          | Pexp_tuple x0 ->
            let x0 = self#list self#expression x0 in
            Expression_desc.of_concrete (Pexp_tuple x0)
          | Pexp_construct (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#option self#expression x1 in
            Expression_desc.of_concrete (Pexp_construct (x0, x1))
          | Pexp_variant (x0, x1) ->
            let x0 = self#label x0 in
            let x1 = self#option self#expression x1 in
            Expression_desc.of_concrete (Pexp_variant (x0, x1))
          | Pexp_record (x0, x1) ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#expression x1 in (x0, x1)) x0 in
            let x1 = self#option self#expression x1 in
            Expression_desc.of_concrete (Pexp_record (x0, x1))
          | Pexp_field (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#longident_loc x1 in
            Expression_desc.of_concrete (Pexp_field (x0, x1))
          | Pexp_setfield (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#expression x2 in
            Expression_desc.of_concrete (Pexp_setfield (x0, x1, x2))
          | Pexp_array x0 ->
            let x0 = self#list self#expression x0 in
            Expression_desc.of_concrete (Pexp_array x0)
          | Pexp_ifthenelse (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            let x2 = self#option self#expression x2 in
            Expression_desc.of_concrete (Pexp_ifthenelse (x0, x1, x2))
          | Pexp_sequence (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_sequence (x0, x1))
          | Pexp_while (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_while (x0, x1))
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let x0 = self#pattern x0 in
            let x1 = self#expression x1 in
            let x2 = self#expression x2 in
            let x3 = self#direction_flag x3 in
            let x4 = self#expression x4 in
            Expression_desc.of_concrete (Pexp_for (x0, x1, x2, x3, x4))
          | Pexp_constraint (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#core_type x1 in
            Expression_desc.of_concrete (Pexp_constraint (x0, x1))
          | Pexp_coerce (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#option self#core_type x1 in
            let x2 = self#core_type x2 in
            Expression_desc.of_concrete (Pexp_coerce (x0, x1, x2))
          | Pexp_send (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#loc self#label x1 in
            Expression_desc.of_concrete (Pexp_send (x0, x1))
          | Pexp_new x0 ->
            let x0 = self#longident_loc x0 in
            Expression_desc.of_concrete (Pexp_new x0)
          | Pexp_setinstvar (x0, x1) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_setinstvar (x0, x1))
          | Pexp_override x0 ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#loc self#label x0 in let x1 = self#expression x1 in (x0, x1)) x0 in
            Expression_desc.of_concrete (Pexp_override x0)
          | Pexp_letmodule (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#module_expr x1 in
            let x2 = self#expression x2 in
            Expression_desc.of_concrete (Pexp_letmodule (x0, x1, x2))
          | Pexp_letexception (x0, x1) ->
            let x0 = self#extension_constructor x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_letexception (x0, x1))
          | Pexp_assert x0 ->
            let x0 = self#expression x0 in
            Expression_desc.of_concrete (Pexp_assert x0)
          | Pexp_lazy x0 ->
            let x0 = self#expression x0 in
            Expression_desc.of_concrete (Pexp_lazy x0)
          | Pexp_poly (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#option self#core_type x1 in
            Expression_desc.of_concrete (Pexp_poly (x0, x1))
          | Pexp_object x0 ->
            let x0 = self#class_structure x0 in
            Expression_desc.of_concrete (Pexp_object x0)
          | Pexp_newtype (x0, x1) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#expression x1 in
            Expression_desc.of_concrete (Pexp_newtype (x0, x1))
          | Pexp_pack x0 ->
            let x0 = self#module_expr x0 in
            Expression_desc.of_concrete (Pexp_pack x0)
          | Pexp_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#expression x2 in
            Expression_desc.of_concrete (Pexp_open (x0, x1, x2))
          | Pexp_extension x0 ->
            let x0 = self#extension x0 in
            Expression_desc.of_concrete (Pexp_extension x0)
          | Pexp_unreachable ->
            Expression_desc.of_concrete Pexp_unreachable
      method case : Case.t -> Case.t  =
        fun case ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let pc_lhs = self#pattern pc_lhs in
          let pc_guard = self#option self#expression pc_guard in
          let pc_rhs = self#expression pc_rhs in
          Case.of_concrete { pc_lhs; pc_guard; pc_rhs }
      method value_description : Value_description.t -> Value_description.t  =
        fun value_description ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let pval_name = self#loc self#string pval_name in
          let pval_type = self#core_type pval_type in
          let pval_prim = self#list self#string pval_prim in
          let pval_attributes = self#attributes pval_attributes in
          let pval_loc = self#location pval_loc in
          Value_description.of_concrete { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
      method type_declaration : Type_declaration.t -> Type_declaration.t  =
        fun type_declaration ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let ptype_name = self#loc self#string ptype_name in
          let ptype_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in (x0, x1)) ptype_params in
          let ptype_cstrs = self#list (fun (x0, x1, x2) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in let x2 = self#location x2 in (x0, x1, x2)) ptype_cstrs in
          let ptype_kind = self#type_kind ptype_kind in
          let ptype_private = self#private_flag ptype_private in
          let ptype_manifest = self#option self#core_type ptype_manifest in
          let ptype_attributes = self#attributes ptype_attributes in
          let ptype_loc = self#location ptype_loc in
          Type_declaration.of_concrete { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }
      method type_kind : Type_kind.t -> Type_kind.t  =
        fun type_kind ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            Type_kind.of_concrete Ptype_abstract
          | Ptype_variant x0 ->
            let x0 = self#list self#constructor_declaration x0 in
            Type_kind.of_concrete (Ptype_variant x0)
          | Ptype_record x0 ->
            let x0 = self#list self#label_declaration x0 in
            Type_kind.of_concrete (Ptype_record x0)
          | Ptype_open ->
            Type_kind.of_concrete Ptype_open
      method label_declaration : Label_declaration.t -> Label_declaration.t  =
        fun label_declaration ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let pld_name = self#loc self#string pld_name in
          let pld_mutable = self#mutable_flag pld_mutable in
          let pld_type = self#core_type pld_type in
          let pld_loc = self#location pld_loc in
          let pld_attributes = self#attributes pld_attributes in
          Label_declaration.of_concrete { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
      method constructor_declaration : Constructor_declaration.t -> Constructor_declaration.t  =
        fun constructor_declaration ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let pcd_name = self#loc self#string pcd_name in
          let pcd_args = self#constructor_arguments pcd_args in
          let pcd_res = self#option self#core_type pcd_res in
          let pcd_loc = self#location pcd_loc in
          let pcd_attributes = self#attributes pcd_attributes in
          Constructor_declaration.of_concrete { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
      method constructor_arguments : Constructor_arguments.t -> Constructor_arguments.t  =
        fun constructor_arguments ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let x0 = self#list self#core_type x0 in
            Constructor_arguments.of_concrete (Pcstr_tuple x0)
          | Pcstr_record x0 ->
            let x0 = self#list self#label_declaration x0 in
            Constructor_arguments.of_concrete (Pcstr_record x0)
      method type_extension : Type_extension.t -> Type_extension.t  =
        fun type_extension ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let ptyext_path = self#longident_loc ptyext_path in
          let ptyext_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in (x0, x1)) ptyext_params in
          let ptyext_constructors = self#list self#extension_constructor ptyext_constructors in
          let ptyext_private = self#private_flag ptyext_private in
          let ptyext_attributes = self#attributes ptyext_attributes in
          Type_extension.of_concrete { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }
      method extension_constructor : Extension_constructor.t -> Extension_constructor.t  =
        fun extension_constructor ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let pext_name = self#loc self#string pext_name in
          let pext_kind = self#extension_constructor_kind pext_kind in
          let pext_loc = self#location pext_loc in
          let pext_attributes = self#attributes pext_attributes in
          Extension_constructor.of_concrete { pext_name; pext_kind; pext_loc; pext_attributes }
      method extension_constructor_kind : Extension_constructor_kind.t -> Extension_constructor_kind.t  =
        fun extension_constructor_kind ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let x0 = self#constructor_arguments x0 in
            let x1 = self#option self#core_type x1 in
            Extension_constructor_kind.of_concrete (Pext_decl (x0, x1))
          | Pext_rebind x0 ->
            let x0 = self#longident_loc x0 in
            Extension_constructor_kind.of_concrete (Pext_rebind x0)
      method class_type : Class_type.t -> Class_type.t  =
        fun class_type ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let pcty_desc = self#class_type_desc pcty_desc in
          let pcty_loc = self#location pcty_loc in
          let pcty_attributes = self#attributes pcty_attributes in
          Class_type.of_concrete { pcty_desc; pcty_loc; pcty_attributes }
      method class_type_desc : Class_type_desc.t -> Class_type_desc.t  =
        fun class_type_desc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            Class_type_desc.of_concrete (Pcty_constr (x0, x1))
          | Pcty_signature x0 ->
            let x0 = self#class_signature x0 in
            Class_type_desc.of_concrete (Pcty_signature x0)
          | Pcty_arrow (x0, x1, x2) ->
            let x0 = self#arg_label x0 in
            let x1 = self#core_type x1 in
            let x2 = self#class_type x2 in
            Class_type_desc.of_concrete (Pcty_arrow (x0, x1, x2))
          | Pcty_extension x0 ->
            let x0 = self#extension x0 in
            Class_type_desc.of_concrete (Pcty_extension x0)
          | Pcty_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#class_type x2 in
            Class_type_desc.of_concrete (Pcty_open (x0, x1, x2))
      method class_signature : Class_signature.t -> Class_signature.t  =
        fun class_signature ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let pcsig_self = self#core_type pcsig_self in
          let pcsig_fields = self#list self#class_type_field pcsig_fields in
          Class_signature.of_concrete { pcsig_self; pcsig_fields }
      method class_type_field : Class_type_field.t -> Class_type_field.t  =
        fun class_type_field ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let pctf_desc = self#class_type_field_desc pctf_desc in
          let pctf_loc = self#location pctf_loc in
          let pctf_attributes = self#attributes pctf_attributes in
          Class_type_field.of_concrete { pctf_desc; pctf_loc; pctf_attributes }
      method class_type_field_desc : Class_type_field_desc.t -> Class_type_field_desc.t  =
        fun class_type_field_desc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let x0 = self#class_type x0 in
            Class_type_field_desc.of_concrete (Pctf_inherit x0)
          | Pctf_val x0 ->
            let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#loc self#label x0 in let x1 = self#mutable_flag x1 in let x2 = self#virtual_flag x2 in let x3 = self#core_type x3 in (x0, x1, x2, x3)) x0 in
            Class_type_field_desc.of_concrete (Pctf_val x0)
          | Pctf_method x0 ->
            let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#loc self#label x0 in let x1 = self#private_flag x1 in let x2 = self#virtual_flag x2 in let x3 = self#core_type x3 in (x0, x1, x2, x3)) x0 in
            Class_type_field_desc.of_concrete (Pctf_method x0)
          | Pctf_constraint x0 ->
            let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in (x0, x1)) x0 in
            Class_type_field_desc.of_concrete (Pctf_constraint x0)
          | Pctf_attribute x0 ->
            let x0 = self#attribute x0 in
            Class_type_field_desc.of_concrete (Pctf_attribute x0)
          | Pctf_extension x0 ->
            let x0 = self#extension x0 in
            Class_type_field_desc.of_concrete (Pctf_extension x0)
      method class_infos_class_expr : Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag pci_virt in
          let pci_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in (x0, x1)) pci_params in
          let pci_name = self#loc self#string pci_name in
          let pci_expr = self#class_expr pci_expr in
          let pci_loc = self#location pci_loc in
          let pci_attributes = self#attributes pci_attributes in
          Class_infos.of_concrete_class_expr { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
      method class_infos_class_type : Class_type.t Class_infos.t -> Class_type.t Class_infos.t  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag pci_virt in
          let pci_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in (x0, x1)) pci_params in
          let pci_name = self#loc self#string pci_name in
          let pci_expr = self#class_type pci_expr in
          let pci_loc = self#location pci_loc in
          let pci_attributes = self#attributes pci_attributes in
          Class_infos.of_concrete_class_type { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
      method class_description : Class_description.t -> Class_description.t  =
        fun class_description ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type concrete in
          Class_description.of_concrete concrete
      method class_type_declaration : Class_type_declaration.t -> Class_type_declaration.t  =
        fun class_type_declaration ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type concrete in
          Class_type_declaration.of_concrete concrete
      method class_expr : Class_expr.t -> Class_expr.t  =
        fun class_expr ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let pcl_desc = self#class_expr_desc pcl_desc in
          let pcl_loc = self#location pcl_loc in
          let pcl_attributes = self#attributes pcl_attributes in
          Class_expr.of_concrete { pcl_desc; pcl_loc; pcl_attributes }
      method class_expr_desc : Class_expr_desc.t -> Class_expr_desc.t  =
        fun class_expr_desc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            Class_expr_desc.of_concrete (Pcl_constr (x0, x1))
          | Pcl_structure x0 ->
            let x0 = self#class_structure x0 in
            Class_expr_desc.of_concrete (Pcl_structure x0)
          | Pcl_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label x0 in
            let x1 = self#option self#expression x1 in
            let x2 = self#pattern x2 in
            let x3 = self#class_expr x3 in
            Class_expr_desc.of_concrete (Pcl_fun (x0, x1, x2, x3))
          | Pcl_apply (x0, x1) ->
            let x0 = self#class_expr x0 in
            let x1 = self#list (fun (x0, x1) -> let x0 = self#arg_label x0 in let x1 = self#expression x1 in (x0, x1)) x1 in
            Class_expr_desc.of_concrete (Pcl_apply (x0, x1))
          | Pcl_let (x0, x1, x2) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            let x2 = self#class_expr x2 in
            Class_expr_desc.of_concrete (Pcl_let (x0, x1, x2))
          | Pcl_constraint (x0, x1) ->
            let x0 = self#class_expr x0 in
            let x1 = self#class_type x1 in
            Class_expr_desc.of_concrete (Pcl_constraint (x0, x1))
          | Pcl_extension x0 ->
            let x0 = self#extension x0 in
            Class_expr_desc.of_concrete (Pcl_extension x0)
          | Pcl_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#class_expr x2 in
            Class_expr_desc.of_concrete (Pcl_open (x0, x1, x2))
      method class_structure : Class_structure.t -> Class_structure.t  =
        fun class_structure ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let pcstr_self = self#pattern pcstr_self in
          let pcstr_fields = self#list self#class_field pcstr_fields in
          Class_structure.of_concrete { pcstr_self; pcstr_fields }
      method class_field : Class_field.t -> Class_field.t  =
        fun class_field ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let pcf_desc = self#class_field_desc pcf_desc in
          let pcf_loc = self#location pcf_loc in
          let pcf_attributes = self#attributes pcf_attributes in
          Class_field.of_concrete { pcf_desc; pcf_loc; pcf_attributes }
      method class_field_desc : Class_field_desc.t -> Class_field_desc.t  =
        fun class_field_desc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#class_expr x1 in
            let x2 = self#option (self#loc self#string) x2 in
            Class_field_desc.of_concrete (Pcf_inherit (x0, x1, x2))
          | Pcf_val x0 ->
            let x0 = (fun (x0, x1, x2) -> let x0 = self#loc self#label x0 in let x1 = self#mutable_flag x1 in let x2 = self#class_field_kind x2 in (x0, x1, x2)) x0 in
            Class_field_desc.of_concrete (Pcf_val x0)
          | Pcf_method x0 ->
            let x0 = (fun (x0, x1, x2) -> let x0 = self#loc self#label x0 in let x1 = self#private_flag x1 in let x2 = self#class_field_kind x2 in (x0, x1, x2)) x0 in
            Class_field_desc.of_concrete (Pcf_method x0)
          | Pcf_constraint x0 ->
            let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in (x0, x1)) x0 in
            Class_field_desc.of_concrete (Pcf_constraint x0)
          | Pcf_initializer x0 ->
            let x0 = self#expression x0 in
            Class_field_desc.of_concrete (Pcf_initializer x0)
          | Pcf_attribute x0 ->
            let x0 = self#attribute x0 in
            Class_field_desc.of_concrete (Pcf_attribute x0)
          | Pcf_extension x0 ->
            let x0 = self#extension x0 in
            Class_field_desc.of_concrete (Pcf_extension x0)
      method class_field_kind : Class_field_kind.t -> Class_field_kind.t  =
        fun class_field_kind ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let x0 = self#core_type x0 in
            Class_field_kind.of_concrete (Cfk_virtual x0)
          | Cfk_concrete (x0, x1) ->
            let x0 = self#override_flag x0 in
            let x1 = self#expression x1 in
            Class_field_kind.of_concrete (Cfk_concrete (x0, x1))
      method class_declaration : Class_declaration.t -> Class_declaration.t  =
        fun class_declaration ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_expr concrete in
          Class_declaration.of_concrete concrete
      method module_type : Module_type.t -> Module_type.t  =
        fun module_type ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let pmty_desc = self#module_type_desc pmty_desc in
          let pmty_loc = self#location pmty_loc in
          let pmty_attributes = self#attributes pmty_attributes in
          Module_type.of_concrete { pmty_desc; pmty_loc; pmty_attributes }
      method module_type_desc : Module_type_desc.t -> Module_type_desc.t  =
        fun module_type_desc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let x0 = self#longident_loc x0 in
            Module_type_desc.of_concrete (Pmty_ident x0)
          | Pmty_signature x0 ->
            let x0 = self#signature x0 in
            Module_type_desc.of_concrete (Pmty_signature x0)
          | Pmty_functor (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#option self#module_type x1 in
            let x2 = self#module_type x2 in
            Module_type_desc.of_concrete (Pmty_functor (x0, x1, x2))
          | Pmty_with (x0, x1) ->
            let x0 = self#module_type x0 in
            let x1 = self#list self#with_constraint x1 in
            Module_type_desc.of_concrete (Pmty_with (x0, x1))
          | Pmty_typeof x0 ->
            let x0 = self#module_expr x0 in
            Module_type_desc.of_concrete (Pmty_typeof x0)
          | Pmty_extension x0 ->
            let x0 = self#extension x0 in
            Module_type_desc.of_concrete (Pmty_extension x0)
          | Pmty_alias x0 ->
            let x0 = self#longident_loc x0 in
            Module_type_desc.of_concrete (Pmty_alias x0)
      method signature : Signature.t -> Signature.t  =
        fun signature ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let concrete = self#list self#signature_item concrete in
          Signature.of_concrete concrete
      method signature_item : Signature_item.t -> Signature_item.t  =
        fun signature_item ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let psig_desc = self#signature_item_desc psig_desc in
          let psig_loc = self#location psig_loc in
          Signature_item.of_concrete { psig_desc; psig_loc }
      method signature_item_desc : Signature_item_desc.t -> Signature_item_desc.t  =
        fun signature_item_desc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let x0 = self#value_description x0 in
            Signature_item_desc.of_concrete (Psig_value x0)
          | Psig_type (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#type_declaration x1 in
            Signature_item_desc.of_concrete (Psig_type (x0, x1))
          | Psig_typext x0 ->
            let x0 = self#type_extension x0 in
            Signature_item_desc.of_concrete (Psig_typext x0)
          | Psig_exception x0 ->
            let x0 = self#extension_constructor x0 in
            Signature_item_desc.of_concrete (Psig_exception x0)
          | Psig_module x0 ->
            let x0 = self#module_declaration x0 in
            Signature_item_desc.of_concrete (Psig_module x0)
          | Psig_recmodule x0 ->
            let x0 = self#list self#module_declaration x0 in
            Signature_item_desc.of_concrete (Psig_recmodule x0)
          | Psig_modtype x0 ->
            let x0 = self#module_type_declaration x0 in
            Signature_item_desc.of_concrete (Psig_modtype x0)
          | Psig_open x0 ->
            let x0 = self#open_description x0 in
            Signature_item_desc.of_concrete (Psig_open x0)
          | Psig_include x0 ->
            let x0 = self#include_description x0 in
            Signature_item_desc.of_concrete (Psig_include x0)
          | Psig_class x0 ->
            let x0 = self#list self#class_description x0 in
            Signature_item_desc.of_concrete (Psig_class x0)
          | Psig_class_type x0 ->
            let x0 = self#list self#class_type_declaration x0 in
            Signature_item_desc.of_concrete (Psig_class_type x0)
          | Psig_attribute x0 ->
            let x0 = self#attribute x0 in
            Signature_item_desc.of_concrete (Psig_attribute x0)
          | Psig_extension (x0, x1) ->
            let x0 = self#extension x0 in
            let x1 = self#attributes x1 in
            Signature_item_desc.of_concrete (Psig_extension (x0, x1))
      method module_declaration : Module_declaration.t -> Module_declaration.t  =
        fun module_declaration ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let pmd_name = self#loc self#string pmd_name in
          let pmd_type = self#module_type pmd_type in
          let pmd_attributes = self#attributes pmd_attributes in
          let pmd_loc = self#location pmd_loc in
          Module_declaration.of_concrete { pmd_name; pmd_type; pmd_attributes; pmd_loc }
      method module_type_declaration : Module_type_declaration.t -> Module_type_declaration.t  =
        fun module_type_declaration ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let pmtd_name = self#loc self#string pmtd_name in
          let pmtd_type = self#option self#module_type pmtd_type in
          let pmtd_attributes = self#attributes pmtd_attributes in
          let pmtd_loc = self#location pmtd_loc in
          Module_type_declaration.of_concrete { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
      method open_description : Open_description.t -> Open_description.t  =
        fun open_description ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let popen_lid = self#longident_loc popen_lid in
          let popen_override = self#override_flag popen_override in
          let popen_loc = self#location popen_loc in
          let popen_attributes = self#attributes popen_attributes in
          Open_description.of_concrete { popen_lid; popen_override; popen_loc; popen_attributes }
      method include_infos_module_expr : Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_expr pincl_mod in
          let pincl_loc = self#location pincl_loc in
          let pincl_attributes = self#attributes pincl_attributes in
          Include_infos.of_concrete_module_expr { pincl_mod; pincl_loc; pincl_attributes }
      method include_infos_module_type : Module_type.t Include_infos.t -> Module_type.t Include_infos.t  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_type pincl_mod in
          let pincl_loc = self#location pincl_loc in
          let pincl_attributes = self#attributes pincl_attributes in
          Include_infos.of_concrete_module_type { pincl_mod; pincl_loc; pincl_attributes }
      method include_description : Include_description.t -> Include_description.t  =
        fun include_description ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let concrete = self#include_infos_module_type concrete in
          Include_description.of_concrete concrete
      method include_declaration : Include_declaration.t -> Include_declaration.t  =
        fun include_declaration ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let concrete = self#include_infos_module_expr concrete in
          Include_declaration.of_concrete concrete
      method with_constraint : With_constraint.t -> With_constraint.t  =
        fun with_constraint ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#type_declaration x1 in
            With_constraint.of_concrete (Pwith_type (x0, x1))
          | Pwith_module (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#longident_loc x1 in
            With_constraint.of_concrete (Pwith_module (x0, x1))
          | Pwith_typesubst (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#type_declaration x1 in
            With_constraint.of_concrete (Pwith_typesubst (x0, x1))
          | Pwith_modsubst (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#longident_loc x1 in
            With_constraint.of_concrete (Pwith_modsubst (x0, x1))
      method module_expr : Module_expr.t -> Module_expr.t  =
        fun module_expr ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let pmod_desc = self#module_expr_desc pmod_desc in
          let pmod_loc = self#location pmod_loc in
          let pmod_attributes = self#attributes pmod_attributes in
          Module_expr.of_concrete { pmod_desc; pmod_loc; pmod_attributes }
      method module_expr_desc : Module_expr_desc.t -> Module_expr_desc.t  =
        fun module_expr_desc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let x0 = self#longident_loc x0 in
            Module_expr_desc.of_concrete (Pmod_ident x0)
          | Pmod_structure x0 ->
            let x0 = self#structure x0 in
            Module_expr_desc.of_concrete (Pmod_structure x0)
          | Pmod_functor (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#option self#module_type x1 in
            let x2 = self#module_expr x2 in
            Module_expr_desc.of_concrete (Pmod_functor (x0, x1, x2))
          | Pmod_apply (x0, x1) ->
            let x0 = self#module_expr x0 in
            let x1 = self#module_expr x1 in
            Module_expr_desc.of_concrete (Pmod_apply (x0, x1))
          | Pmod_constraint (x0, x1) ->
            let x0 = self#module_expr x0 in
            let x1 = self#module_type x1 in
            Module_expr_desc.of_concrete (Pmod_constraint (x0, x1))
          | Pmod_unpack x0 ->
            let x0 = self#expression x0 in
            Module_expr_desc.of_concrete (Pmod_unpack x0)
          | Pmod_extension x0 ->
            let x0 = self#extension x0 in
            Module_expr_desc.of_concrete (Pmod_extension x0)
      method structure : Structure.t -> Structure.t  =
        fun structure ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let concrete = self#list self#structure_item concrete in
          Structure.of_concrete concrete
      method structure_item : Structure_item.t -> Structure_item.t  =
        fun structure_item ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let pstr_desc = self#structure_item_desc pstr_desc in
          let pstr_loc = self#location pstr_loc in
          Structure_item.of_concrete { pstr_desc; pstr_loc }
      method structure_item_desc : Structure_item_desc.t -> Structure_item_desc.t  =
        fun structure_item_desc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#attributes x1 in
            Structure_item_desc.of_concrete (Pstr_eval (x0, x1))
          | Pstr_value (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            Structure_item_desc.of_concrete (Pstr_value (x0, x1))
          | Pstr_primitive x0 ->
            let x0 = self#value_description x0 in
            Structure_item_desc.of_concrete (Pstr_primitive x0)
          | Pstr_type (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#type_declaration x1 in
            Structure_item_desc.of_concrete (Pstr_type (x0, x1))
          | Pstr_typext x0 ->
            let x0 = self#type_extension x0 in
            Structure_item_desc.of_concrete (Pstr_typext x0)
          | Pstr_exception x0 ->
            let x0 = self#extension_constructor x0 in
            Structure_item_desc.of_concrete (Pstr_exception x0)
          | Pstr_module x0 ->
            let x0 = self#module_binding x0 in
            Structure_item_desc.of_concrete (Pstr_module x0)
          | Pstr_recmodule x0 ->
            let x0 = self#list self#module_binding x0 in
            Structure_item_desc.of_concrete (Pstr_recmodule x0)
          | Pstr_modtype x0 ->
            let x0 = self#module_type_declaration x0 in
            Structure_item_desc.of_concrete (Pstr_modtype x0)
          | Pstr_open x0 ->
            let x0 = self#open_description x0 in
            Structure_item_desc.of_concrete (Pstr_open x0)
          | Pstr_class x0 ->
            let x0 = self#list self#class_declaration x0 in
            Structure_item_desc.of_concrete (Pstr_class x0)
          | Pstr_class_type x0 ->
            let x0 = self#list self#class_type_declaration x0 in
            Structure_item_desc.of_concrete (Pstr_class_type x0)
          | Pstr_include x0 ->
            let x0 = self#include_declaration x0 in
            Structure_item_desc.of_concrete (Pstr_include x0)
          | Pstr_attribute x0 ->
            let x0 = self#attribute x0 in
            Structure_item_desc.of_concrete (Pstr_attribute x0)
          | Pstr_extension (x0, x1) ->
            let x0 = self#extension x0 in
            let x1 = self#attributes x1 in
            Structure_item_desc.of_concrete (Pstr_extension (x0, x1))
      method value_binding : Value_binding.t -> Value_binding.t  =
        fun value_binding ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let pvb_pat = self#pattern pvb_pat in
          let pvb_expr = self#expression pvb_expr in
          let pvb_attributes = self#attributes pvb_attributes in
          let pvb_loc = self#location pvb_loc in
          Value_binding.of_concrete { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
      method module_binding : Module_binding.t -> Module_binding.t  =
        fun module_binding ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let pmb_name = self#loc self#string pmb_name in
          let pmb_expr = self#module_expr pmb_expr in
          let pmb_attributes = self#attributes pmb_attributes in
          let pmb_loc = self#location pmb_loc in
          Module_binding.of_concrete { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
      method toplevel_phrase : Toplevel_phrase.t -> Toplevel_phrase.t  =
        fun toplevel_phrase ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let x0 = self#structure x0 in
            Toplevel_phrase.of_concrete (Ptop_def x0)
          | Ptop_dir (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#directive_argument x1 in
            Toplevel_phrase.of_concrete (Ptop_dir (x0, x1))
      method directive_argument : Directive_argument.t -> Directive_argument.t  =
        fun directive_argument ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            Directive_argument.of_concrete Pdir_none
          | Pdir_string x0 ->
            let x0 = self#string x0 in
            Directive_argument.of_concrete (Pdir_string x0)
          | Pdir_int (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            Directive_argument.of_concrete (Pdir_int (x0, x1))
          | Pdir_ident x0 ->
            let x0 = self#longident x0 in
            Directive_argument.of_concrete (Pdir_ident x0)
          | Pdir_bool x0 ->
            let x0 = self#bool x0 in
            Directive_argument.of_concrete (Pdir_bool x0)
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
      method longident : Longident.t -> unit  =
        fun longident ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            self#string x0
          | Ldot (x0, x1) ->
            self#longident x0;
            self#string x1
          | Lapply (x0, x1) ->
            self#longident x0;
            self#longident x1
      method longident_loc : Longident_loc.t -> unit  =
        fun longident_loc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          self#loc self#longident concrete
      method rec_flag : Rec_flag.t -> unit  =
        fun rec_flag ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            ()
          | Recursive ->
            ()
      method direction_flag : Direction_flag.t -> unit  =
        fun direction_flag ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            ()
          | Downto ->
            ()
      method private_flag : Private_flag.t -> unit  =
        fun private_flag ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            ()
          | Public ->
            ()
      method mutable_flag : Mutable_flag.t -> unit  =
        fun mutable_flag ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            ()
          | Mutable ->
            ()
      method virtual_flag : Virtual_flag.t -> unit  =
        fun virtual_flag ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            ()
          | Concrete ->
            ()
      method override_flag : Override_flag.t -> unit  =
        fun override_flag ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            ()
          | Fresh ->
            ()
      method closed_flag : Closed_flag.t -> unit  =
        fun closed_flag ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            ()
          | Open ->
            ()
      method label : Label.t -> unit  =
        fun label ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          self#string concrete
      method arg_label : Arg_label.t -> unit  =
        fun arg_label ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            ()
          | Labelled x0 ->
            self#string x0
          | Optional x0 ->
            self#string x0
      method variance : Variance.t -> unit  =
        fun variance ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            ()
          | Contravariant ->
            ()
          | Invariant ->
            ()
      method constant : Constant.t -> unit  =
        fun constant ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            self#string x0;
            self#option self#char x1
          | Pconst_char x0 ->
            self#char x0
          | Pconst_string (x0, x1) ->
            self#string x0;
            self#option self#string x1
          | Pconst_float (x0, x1) ->
            self#string x0;
            self#option self#char x1
      method attribute : Attribute.t -> unit  =
        fun attribute ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          self#loc self#string x0;
          self#payload x1
      method extension : Extension.t -> unit  =
        fun extension ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          self#loc self#string x0;
          self#payload x1
      method attributes : Attributes.t -> unit  =
        fun attributes ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          self#list self#attribute concrete
      method payload : Payload.t -> unit  =
        fun payload ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            self#structure x0
          | PSig x0 ->
            self#signature x0
          | PTyp x0 ->
            self#core_type x0
          | PPat (x0, x1) ->
            self#pattern x0;
            self#option self#expression x1
      method core_type : Core_type.t -> unit  =
        fun core_type ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          self#core_type_desc ptyp_desc;
          self#location ptyp_loc;
          self#attributes ptyp_attributes
      method core_type_desc : Core_type_desc.t -> unit  =
        fun core_type_desc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            ()
          | Ptyp_var x0 ->
            self#string x0
          | Ptyp_arrow (x0, x1, x2) ->
            self#arg_label x0;
            self#core_type x1;
            self#core_type x2
          | Ptyp_tuple x0 ->
            self#list self#core_type x0
          | Ptyp_constr (x0, x1) ->
            self#longident_loc x0;
            self#list self#core_type x1
          | Ptyp_object (x0, x1) ->
            self#list self#object_field x0;
            self#closed_flag x1
          | Ptyp_class (x0, x1) ->
            self#longident_loc x0;
            self#list self#core_type x1
          | Ptyp_alias (x0, x1) ->
            self#core_type x0;
            self#string x1
          | Ptyp_variant (x0, x1, x2) ->
            self#list self#row_field x0;
            self#closed_flag x1;
            self#option (self#list self#label) x2
          | Ptyp_poly (x0, x1) ->
            self#list (self#loc self#string) x0;
            self#core_type x1
          | Ptyp_package x0 ->
            self#package_type x0
          | Ptyp_extension x0 ->
            self#extension x0
      method package_type : Package_type.t -> unit  =
        fun package_type ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          self#longident_loc x0;
          self#list (fun (x0, x1) -> self#longident_loc x0; self#core_type x1) x1
      method row_field : Row_field.t -> unit  =
        fun row_field ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            self#loc self#label x0;
            self#attributes x1;
            self#bool x2;
            self#list self#core_type x3
          | Rinherit x0 ->
            self#core_type x0
      method object_field : Object_field.t -> unit  =
        fun object_field ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            self#loc self#label x0;
            self#attributes x1;
            self#core_type x2
          | Oinherit x0 ->
            self#core_type x0
      method pattern : Pattern.t -> unit  =
        fun pattern ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          self#pattern_desc ppat_desc;
          self#location ppat_loc;
          self#attributes ppat_attributes
      method pattern_desc : Pattern_desc.t -> unit  =
        fun pattern_desc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            ()
          | Ppat_var x0 ->
            self#loc self#string x0
          | Ppat_alias (x0, x1) ->
            self#pattern x0;
            self#loc self#string x1
          | Ppat_constant x0 ->
            self#constant x0
          | Ppat_interval (x0, x1) ->
            self#constant x0;
            self#constant x1
          | Ppat_tuple x0 ->
            self#list self#pattern x0
          | Ppat_construct (x0, x1) ->
            self#longident_loc x0;
            self#option self#pattern x1
          | Ppat_variant (x0, x1) ->
            self#label x0;
            self#option self#pattern x1
          | Ppat_record (x0, x1) ->
            self#list (fun (x0, x1) -> self#longident_loc x0; self#pattern x1) x0;
            self#closed_flag x1
          | Ppat_array x0 ->
            self#list self#pattern x0
          | Ppat_or (x0, x1) ->
            self#pattern x0;
            self#pattern x1
          | Ppat_constraint (x0, x1) ->
            self#pattern x0;
            self#core_type x1
          | Ppat_type x0 ->
            self#longident_loc x0
          | Ppat_lazy x0 ->
            self#pattern x0
          | Ppat_unpack x0 ->
            self#loc self#string x0
          | Ppat_exception x0 ->
            self#pattern x0
          | Ppat_extension x0 ->
            self#extension x0
          | Ppat_open (x0, x1) ->
            self#longident_loc x0;
            self#pattern x1
      method expression : Expression.t -> unit  =
        fun expression ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          self#expression_desc pexp_desc;
          self#location pexp_loc;
          self#attributes pexp_attributes
      method expression_desc : Expression_desc.t -> unit  =
        fun expression_desc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            self#longident_loc x0
          | Pexp_constant x0 ->
            self#constant x0
          | Pexp_let (x0, x1, x2) ->
            self#rec_flag x0;
            self#list self#value_binding x1;
            self#expression x2
          | Pexp_function x0 ->
            self#list self#case x0
          | Pexp_fun (x0, x1, x2, x3) ->
            self#arg_label x0;
            self#option self#expression x1;
            self#pattern x2;
            self#expression x3
          | Pexp_apply (x0, x1) ->
            self#expression x0;
            self#list (fun (x0, x1) -> self#arg_label x0; self#expression x1) x1
          | Pexp_match (x0, x1) ->
            self#expression x0;
            self#list self#case x1
          | Pexp_try (x0, x1) ->
            self#expression x0;
            self#list self#case x1
          | Pexp_tuple x0 ->
            self#list self#expression x0
          | Pexp_construct (x0, x1) ->
            self#longident_loc x0;
            self#option self#expression x1
          | Pexp_variant (x0, x1) ->
            self#label x0;
            self#option self#expression x1
          | Pexp_record (x0, x1) ->
            self#list (fun (x0, x1) -> self#longident_loc x0; self#expression x1) x0;
            self#option self#expression x1
          | Pexp_field (x0, x1) ->
            self#expression x0;
            self#longident_loc x1
          | Pexp_setfield (x0, x1, x2) ->
            self#expression x0;
            self#longident_loc x1;
            self#expression x2
          | Pexp_array x0 ->
            self#list self#expression x0
          | Pexp_ifthenelse (x0, x1, x2) ->
            self#expression x0;
            self#expression x1;
            self#option self#expression x2
          | Pexp_sequence (x0, x1) ->
            self#expression x0;
            self#expression x1
          | Pexp_while (x0, x1) ->
            self#expression x0;
            self#expression x1
          | Pexp_for (x0, x1, x2, x3, x4) ->
            self#pattern x0;
            self#expression x1;
            self#expression x2;
            self#direction_flag x3;
            self#expression x4
          | Pexp_constraint (x0, x1) ->
            self#expression x0;
            self#core_type x1
          | Pexp_coerce (x0, x1, x2) ->
            self#expression x0;
            self#option self#core_type x1;
            self#core_type x2
          | Pexp_send (x0, x1) ->
            self#expression x0;
            self#loc self#label x1
          | Pexp_new x0 ->
            self#longident_loc x0
          | Pexp_setinstvar (x0, x1) ->
            self#loc self#label x0;
            self#expression x1
          | Pexp_override x0 ->
            self#list (fun (x0, x1) -> self#loc self#label x0; self#expression x1) x0
          | Pexp_letmodule (x0, x1, x2) ->
            self#loc self#string x0;
            self#module_expr x1;
            self#expression x2
          | Pexp_letexception (x0, x1) ->
            self#extension_constructor x0;
            self#expression x1
          | Pexp_assert x0 ->
            self#expression x0
          | Pexp_lazy x0 ->
            self#expression x0
          | Pexp_poly (x0, x1) ->
            self#expression x0;
            self#option self#core_type x1
          | Pexp_object x0 ->
            self#class_structure x0
          | Pexp_newtype (x0, x1) ->
            self#loc self#string x0;
            self#expression x1
          | Pexp_pack x0 ->
            self#module_expr x0
          | Pexp_open (x0, x1, x2) ->
            self#override_flag x0;
            self#longident_loc x1;
            self#expression x2
          | Pexp_extension x0 ->
            self#extension x0
          | Pexp_unreachable ->
            ()
      method case : Case.t -> unit  =
        fun case ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          self#pattern pc_lhs;
          self#option self#expression pc_guard;
          self#expression pc_rhs
      method value_description : Value_description.t -> unit  =
        fun value_description ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          self#loc self#string pval_name;
          self#core_type pval_type;
          self#list self#string pval_prim;
          self#attributes pval_attributes;
          self#location pval_loc
      method type_declaration : Type_declaration.t -> unit  =
        fun type_declaration ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          self#loc self#string ptype_name;
          self#list (fun (x0, x1) -> self#core_type x0; self#variance x1) ptype_params;
          self#list (fun (x0, x1, x2) -> self#core_type x0; self#core_type x1; self#location x2) ptype_cstrs;
          self#type_kind ptype_kind;
          self#private_flag ptype_private;
          self#option self#core_type ptype_manifest;
          self#attributes ptype_attributes;
          self#location ptype_loc
      method type_kind : Type_kind.t -> unit  =
        fun type_kind ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            ()
          | Ptype_variant x0 ->
            self#list self#constructor_declaration x0
          | Ptype_record x0 ->
            self#list self#label_declaration x0
          | Ptype_open ->
            ()
      method label_declaration : Label_declaration.t -> unit  =
        fun label_declaration ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          self#loc self#string pld_name;
          self#mutable_flag pld_mutable;
          self#core_type pld_type;
          self#location pld_loc;
          self#attributes pld_attributes
      method constructor_declaration : Constructor_declaration.t -> unit  =
        fun constructor_declaration ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          self#loc self#string pcd_name;
          self#constructor_arguments pcd_args;
          self#option self#core_type pcd_res;
          self#location pcd_loc;
          self#attributes pcd_attributes
      method constructor_arguments : Constructor_arguments.t -> unit  =
        fun constructor_arguments ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            self#list self#core_type x0
          | Pcstr_record x0 ->
            self#list self#label_declaration x0
      method type_extension : Type_extension.t -> unit  =
        fun type_extension ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          self#longident_loc ptyext_path;
          self#list (fun (x0, x1) -> self#core_type x0; self#variance x1) ptyext_params;
          self#list self#extension_constructor ptyext_constructors;
          self#private_flag ptyext_private;
          self#attributes ptyext_attributes
      method extension_constructor : Extension_constructor.t -> unit  =
        fun extension_constructor ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          self#loc self#string pext_name;
          self#extension_constructor_kind pext_kind;
          self#location pext_loc;
          self#attributes pext_attributes
      method extension_constructor_kind : Extension_constructor_kind.t -> unit  =
        fun extension_constructor_kind ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            self#constructor_arguments x0;
            self#option self#core_type x1
          | Pext_rebind x0 ->
            self#longident_loc x0
      method class_type : Class_type.t -> unit  =
        fun class_type ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          self#class_type_desc pcty_desc;
          self#location pcty_loc;
          self#attributes pcty_attributes
      method class_type_desc : Class_type_desc.t -> unit  =
        fun class_type_desc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            self#longident_loc x0;
            self#list self#core_type x1
          | Pcty_signature x0 ->
            self#class_signature x0
          | Pcty_arrow (x0, x1, x2) ->
            self#arg_label x0;
            self#core_type x1;
            self#class_type x2
          | Pcty_extension x0 ->
            self#extension x0
          | Pcty_open (x0, x1, x2) ->
            self#override_flag x0;
            self#longident_loc x1;
            self#class_type x2
      method class_signature : Class_signature.t -> unit  =
        fun class_signature ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          self#core_type pcsig_self;
          self#list self#class_type_field pcsig_fields
      method class_type_field : Class_type_field.t -> unit  =
        fun class_type_field ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          self#class_type_field_desc pctf_desc;
          self#location pctf_loc;
          self#attributes pctf_attributes
      method class_type_field_desc : Class_type_field_desc.t -> unit  =
        fun class_type_field_desc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            self#class_type x0
          | Pctf_val x0 ->
            (fun (x0, x1, x2, x3) -> self#loc self#label x0; self#mutable_flag x1; self#virtual_flag x2; self#core_type x3) x0
          | Pctf_method x0 ->
            (fun (x0, x1, x2, x3) -> self#loc self#label x0; self#private_flag x1; self#virtual_flag x2; self#core_type x3) x0
          | Pctf_constraint x0 ->
            (fun (x0, x1) -> self#core_type x0; self#core_type x1) x0
          | Pctf_attribute x0 ->
            self#attribute x0
          | Pctf_extension x0 ->
            self#extension x0
      method class_infos_class_expr : Class_expr.t Class_infos.t -> unit  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          self#virtual_flag pci_virt;
          self#list (fun (x0, x1) -> self#core_type x0; self#variance x1) pci_params;
          self#loc self#string pci_name;
          self#class_expr pci_expr;
          self#location pci_loc;
          self#attributes pci_attributes
      method class_infos_class_type : Class_type.t Class_infos.t -> unit  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          self#virtual_flag pci_virt;
          self#list (fun (x0, x1) -> self#core_type x0; self#variance x1) pci_params;
          self#loc self#string pci_name;
          self#class_type pci_expr;
          self#location pci_loc;
          self#attributes pci_attributes
      method class_description : Class_description.t -> unit  =
        fun class_description ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          self#class_infos_class_type concrete
      method class_type_declaration : Class_type_declaration.t -> unit  =
        fun class_type_declaration ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          self#class_infos_class_type concrete
      method class_expr : Class_expr.t -> unit  =
        fun class_expr ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          self#class_expr_desc pcl_desc;
          self#location pcl_loc;
          self#attributes pcl_attributes
      method class_expr_desc : Class_expr_desc.t -> unit  =
        fun class_expr_desc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            self#longident_loc x0;
            self#list self#core_type x1
          | Pcl_structure x0 ->
            self#class_structure x0
          | Pcl_fun (x0, x1, x2, x3) ->
            self#arg_label x0;
            self#option self#expression x1;
            self#pattern x2;
            self#class_expr x3
          | Pcl_apply (x0, x1) ->
            self#class_expr x0;
            self#list (fun (x0, x1) -> self#arg_label x0; self#expression x1) x1
          | Pcl_let (x0, x1, x2) ->
            self#rec_flag x0;
            self#list self#value_binding x1;
            self#class_expr x2
          | Pcl_constraint (x0, x1) ->
            self#class_expr x0;
            self#class_type x1
          | Pcl_extension x0 ->
            self#extension x0
          | Pcl_open (x0, x1, x2) ->
            self#override_flag x0;
            self#longident_loc x1;
            self#class_expr x2
      method class_structure : Class_structure.t -> unit  =
        fun class_structure ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          self#pattern pcstr_self;
          self#list self#class_field pcstr_fields
      method class_field : Class_field.t -> unit  =
        fun class_field ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          self#class_field_desc pcf_desc;
          self#location pcf_loc;
          self#attributes pcf_attributes
      method class_field_desc : Class_field_desc.t -> unit  =
        fun class_field_desc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            self#override_flag x0;
            self#class_expr x1;
            self#option (self#loc self#string) x2
          | Pcf_val x0 ->
            (fun (x0, x1, x2) -> self#loc self#label x0; self#mutable_flag x1; self#class_field_kind x2) x0
          | Pcf_method x0 ->
            (fun (x0, x1, x2) -> self#loc self#label x0; self#private_flag x1; self#class_field_kind x2) x0
          | Pcf_constraint x0 ->
            (fun (x0, x1) -> self#core_type x0; self#core_type x1) x0
          | Pcf_initializer x0 ->
            self#expression x0
          | Pcf_attribute x0 ->
            self#attribute x0
          | Pcf_extension x0 ->
            self#extension x0
      method class_field_kind : Class_field_kind.t -> unit  =
        fun class_field_kind ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            self#core_type x0
          | Cfk_concrete (x0, x1) ->
            self#override_flag x0;
            self#expression x1
      method class_declaration : Class_declaration.t -> unit  =
        fun class_declaration ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          self#class_infos_class_expr concrete
      method module_type : Module_type.t -> unit  =
        fun module_type ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          self#module_type_desc pmty_desc;
          self#location pmty_loc;
          self#attributes pmty_attributes
      method module_type_desc : Module_type_desc.t -> unit  =
        fun module_type_desc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            self#longident_loc x0
          | Pmty_signature x0 ->
            self#signature x0
          | Pmty_functor (x0, x1, x2) ->
            self#loc self#string x0;
            self#option self#module_type x1;
            self#module_type x2
          | Pmty_with (x0, x1) ->
            self#module_type x0;
            self#list self#with_constraint x1
          | Pmty_typeof x0 ->
            self#module_expr x0
          | Pmty_extension x0 ->
            self#extension x0
          | Pmty_alias x0 ->
            self#longident_loc x0
      method signature : Signature.t -> unit  =
        fun signature ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          self#list self#signature_item concrete
      method signature_item : Signature_item.t -> unit  =
        fun signature_item ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          self#signature_item_desc psig_desc;
          self#location psig_loc
      method signature_item_desc : Signature_item_desc.t -> unit  =
        fun signature_item_desc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            self#value_description x0
          | Psig_type (x0, x1) ->
            self#rec_flag x0;
            self#list self#type_declaration x1
          | Psig_typext x0 ->
            self#type_extension x0
          | Psig_exception x0 ->
            self#extension_constructor x0
          | Psig_module x0 ->
            self#module_declaration x0
          | Psig_recmodule x0 ->
            self#list self#module_declaration x0
          | Psig_modtype x0 ->
            self#module_type_declaration x0
          | Psig_open x0 ->
            self#open_description x0
          | Psig_include x0 ->
            self#include_description x0
          | Psig_class x0 ->
            self#list self#class_description x0
          | Psig_class_type x0 ->
            self#list self#class_type_declaration x0
          | Psig_attribute x0 ->
            self#attribute x0
          | Psig_extension (x0, x1) ->
            self#extension x0;
            self#attributes x1
      method module_declaration : Module_declaration.t -> unit  =
        fun module_declaration ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          self#loc self#string pmd_name;
          self#module_type pmd_type;
          self#attributes pmd_attributes;
          self#location pmd_loc
      method module_type_declaration : Module_type_declaration.t -> unit  =
        fun module_type_declaration ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          self#loc self#string pmtd_name;
          self#option self#module_type pmtd_type;
          self#attributes pmtd_attributes;
          self#location pmtd_loc
      method open_description : Open_description.t -> unit  =
        fun open_description ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          self#longident_loc popen_lid;
          self#override_flag popen_override;
          self#location popen_loc;
          self#attributes popen_attributes
      method include_infos_module_expr : Module_expr.t Include_infos.t -> unit  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          self#module_expr pincl_mod;
          self#location pincl_loc;
          self#attributes pincl_attributes
      method include_infos_module_type : Module_type.t Include_infos.t -> unit  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          self#module_type pincl_mod;
          self#location pincl_loc;
          self#attributes pincl_attributes
      method include_description : Include_description.t -> unit  =
        fun include_description ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          self#include_infos_module_type concrete
      method include_declaration : Include_declaration.t -> unit  =
        fun include_declaration ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          self#include_infos_module_expr concrete
      method with_constraint : With_constraint.t -> unit  =
        fun with_constraint ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            self#longident_loc x0;
            self#type_declaration x1
          | Pwith_module (x0, x1) ->
            self#longident_loc x0;
            self#longident_loc x1
          | Pwith_typesubst (x0, x1) ->
            self#longident_loc x0;
            self#type_declaration x1
          | Pwith_modsubst (x0, x1) ->
            self#longident_loc x0;
            self#longident_loc x1
      method module_expr : Module_expr.t -> unit  =
        fun module_expr ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          self#module_expr_desc pmod_desc;
          self#location pmod_loc;
          self#attributes pmod_attributes
      method module_expr_desc : Module_expr_desc.t -> unit  =
        fun module_expr_desc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            self#longident_loc x0
          | Pmod_structure x0 ->
            self#structure x0
          | Pmod_functor (x0, x1, x2) ->
            self#loc self#string x0;
            self#option self#module_type x1;
            self#module_expr x2
          | Pmod_apply (x0, x1) ->
            self#module_expr x0;
            self#module_expr x1
          | Pmod_constraint (x0, x1) ->
            self#module_expr x0;
            self#module_type x1
          | Pmod_unpack x0 ->
            self#expression x0
          | Pmod_extension x0 ->
            self#extension x0
      method structure : Structure.t -> unit  =
        fun structure ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          self#list self#structure_item concrete
      method structure_item : Structure_item.t -> unit  =
        fun structure_item ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          self#structure_item_desc pstr_desc;
          self#location pstr_loc
      method structure_item_desc : Structure_item_desc.t -> unit  =
        fun structure_item_desc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            self#expression x0;
            self#attributes x1
          | Pstr_value (x0, x1) ->
            self#rec_flag x0;
            self#list self#value_binding x1
          | Pstr_primitive x0 ->
            self#value_description x0
          | Pstr_type (x0, x1) ->
            self#rec_flag x0;
            self#list self#type_declaration x1
          | Pstr_typext x0 ->
            self#type_extension x0
          | Pstr_exception x0 ->
            self#extension_constructor x0
          | Pstr_module x0 ->
            self#module_binding x0
          | Pstr_recmodule x0 ->
            self#list self#module_binding x0
          | Pstr_modtype x0 ->
            self#module_type_declaration x0
          | Pstr_open x0 ->
            self#open_description x0
          | Pstr_class x0 ->
            self#list self#class_declaration x0
          | Pstr_class_type x0 ->
            self#list self#class_type_declaration x0
          | Pstr_include x0 ->
            self#include_declaration x0
          | Pstr_attribute x0 ->
            self#attribute x0
          | Pstr_extension (x0, x1) ->
            self#extension x0;
            self#attributes x1
      method value_binding : Value_binding.t -> unit  =
        fun value_binding ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          self#pattern pvb_pat;
          self#expression pvb_expr;
          self#attributes pvb_attributes;
          self#location pvb_loc
      method module_binding : Module_binding.t -> unit  =
        fun module_binding ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          self#loc self#string pmb_name;
          self#module_expr pmb_expr;
          self#attributes pmb_attributes;
          self#location pmb_loc
      method toplevel_phrase : Toplevel_phrase.t -> unit  =
        fun toplevel_phrase ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            self#structure x0
          | Ptop_dir (x0, x1) ->
            self#string x0;
            self#directive_argument x1
      method directive_argument : Directive_argument.t -> unit  =
        fun directive_argument ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            ()
          | Pdir_string x0 ->
            self#string x0
          | Pdir_int (x0, x1) ->
            self#string x0;
            self#option self#char x1
          | Pdir_ident x0 ->
            self#longident x0
          | Pdir_bool x0 ->
            self#bool x0
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
      method longident : Longident.t -> 'acc -> 'acc  =
        fun longident acc ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let acc = self#string x0 acc in
            acc
          | Ldot (x0, x1) ->
            let acc = self#longident x0 acc in
            let acc = self#string x1 acc in
            acc
          | Lapply (x0, x1) ->
            let acc = self#longident x0 acc in
            let acc = self#longident x1 acc in
            acc
      method longident_loc : Longident_loc.t -> 'acc -> 'acc  =
        fun longident_loc acc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let acc = self#loc self#longident concrete acc in
          acc
      method rec_flag : Rec_flag.t -> 'acc -> 'acc  =
        fun rec_flag acc ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            acc
          | Recursive ->
            acc
      method direction_flag : Direction_flag.t -> 'acc -> 'acc  =
        fun direction_flag acc ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            acc
          | Downto ->
            acc
      method private_flag : Private_flag.t -> 'acc -> 'acc  =
        fun private_flag acc ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            acc
          | Public ->
            acc
      method mutable_flag : Mutable_flag.t -> 'acc -> 'acc  =
        fun mutable_flag acc ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            acc
          | Mutable ->
            acc
      method virtual_flag : Virtual_flag.t -> 'acc -> 'acc  =
        fun virtual_flag acc ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            acc
          | Concrete ->
            acc
      method override_flag : Override_flag.t -> 'acc -> 'acc  =
        fun override_flag acc ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            acc
          | Fresh ->
            acc
      method closed_flag : Closed_flag.t -> 'acc -> 'acc  =
        fun closed_flag acc ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            acc
          | Open ->
            acc
      method label : Label.t -> 'acc -> 'acc  =
        fun label acc ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let acc = self#string concrete acc in
          acc
      method arg_label : Arg_label.t -> 'acc -> 'acc  =
        fun arg_label acc ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            acc
          | Labelled x0 ->
            let acc = self#string x0 acc in
            acc
          | Optional x0 ->
            let acc = self#string x0 acc in
            acc
      method variance : Variance.t -> 'acc -> 'acc  =
        fun variance acc ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            acc
          | Contravariant ->
            acc
          | Invariant ->
            acc
      method constant : Constant.t -> 'acc -> 'acc  =
        fun constant acc ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#option self#char x1 acc in
            acc
          | Pconst_char x0 ->
            let acc = self#char x0 acc in
            acc
          | Pconst_string (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#option self#string x1 acc in
            acc
          | Pconst_float (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#option self#char x1 acc in
            acc
      method attribute : Attribute.t -> 'acc -> 'acc  =
        fun attribute acc ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let acc = self#loc self#string x0 acc in
          let acc = self#payload x1 acc in
          acc
      method extension : Extension.t -> 'acc -> 'acc  =
        fun extension acc ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let acc = self#loc self#string x0 acc in
          let acc = self#payload x1 acc in
          acc
      method attributes : Attributes.t -> 'acc -> 'acc  =
        fun attributes acc ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let acc = self#list self#attribute concrete acc in
          acc
      method payload : Payload.t -> 'acc -> 'acc  =
        fun payload acc ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let acc = self#structure x0 acc in
            acc
          | PSig x0 ->
            let acc = self#signature x0 acc in
            acc
          | PTyp x0 ->
            let acc = self#core_type x0 acc in
            acc
          | PPat (x0, x1) ->
            let acc = self#pattern x0 acc in
            let acc = self#option self#expression x1 acc in
            acc
      method core_type : Core_type.t -> 'acc -> 'acc  =
        fun core_type acc ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let acc = self#core_type_desc ptyp_desc acc in
          let acc = self#location ptyp_loc acc in
          let acc = self#attributes ptyp_attributes acc in
          acc
      method core_type_desc : Core_type_desc.t -> 'acc -> 'acc  =
        fun core_type_desc acc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            acc
          | Ptyp_var x0 ->
            let acc = self#string x0 acc in
            acc
          | Ptyp_arrow (x0, x1, x2) ->
            let acc = self#arg_label x0 acc in
            let acc = self#core_type x1 acc in
            let acc = self#core_type x2 acc in
            acc
          | Ptyp_tuple x0 ->
            let acc = self#list self#core_type x0 acc in
            acc
          | Ptyp_constr (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#list self#core_type x1 acc in
            acc
          | Ptyp_object (x0, x1) ->
            let acc = self#list self#object_field x0 acc in
            let acc = self#closed_flag x1 acc in
            acc
          | Ptyp_class (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#list self#core_type x1 acc in
            acc
          | Ptyp_alias (x0, x1) ->
            let acc = self#core_type x0 acc in
            let acc = self#string x1 acc in
            acc
          | Ptyp_variant (x0, x1, x2) ->
            let acc = self#list self#row_field x0 acc in
            let acc = self#closed_flag x1 acc in
            let acc = self#option (self#list self#label) x2 acc in
            acc
          | Ptyp_poly (x0, x1) ->
            let acc = self#list (self#loc self#string) x0 acc in
            let acc = self#core_type x1 acc in
            acc
          | Ptyp_package x0 ->
            let acc = self#package_type x0 acc in
            acc
          | Ptyp_extension x0 ->
            let acc = self#extension x0 acc in
            acc
      method package_type : Package_type.t -> 'acc -> 'acc  =
        fun package_type acc ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let acc = self#longident_loc x0 acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#longident_loc x0 acc in let acc = self#core_type x1 acc in acc) x1 acc in
          acc
      method row_field : Row_field.t -> 'acc -> 'acc  =
        fun row_field acc ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let acc = self#loc self#label x0 acc in
            let acc = self#attributes x1 acc in
            let acc = self#bool x2 acc in
            let acc = self#list self#core_type x3 acc in
            acc
          | Rinherit x0 ->
            let acc = self#core_type x0 acc in
            acc
      method object_field : Object_field.t -> 'acc -> 'acc  =
        fun object_field acc ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let acc = self#loc self#label x0 acc in
            let acc = self#attributes x1 acc in
            let acc = self#core_type x2 acc in
            acc
          | Oinherit x0 ->
            let acc = self#core_type x0 acc in
            acc
      method pattern : Pattern.t -> 'acc -> 'acc  =
        fun pattern acc ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let acc = self#pattern_desc ppat_desc acc in
          let acc = self#location ppat_loc acc in
          let acc = self#attributes ppat_attributes acc in
          acc
      method pattern_desc : Pattern_desc.t -> 'acc -> 'acc  =
        fun pattern_desc acc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            acc
          | Ppat_var x0 ->
            let acc = self#loc self#string x0 acc in
            acc
          | Ppat_alias (x0, x1) ->
            let acc = self#pattern x0 acc in
            let acc = self#loc self#string x1 acc in
            acc
          | Ppat_constant x0 ->
            let acc = self#constant x0 acc in
            acc
          | Ppat_interval (x0, x1) ->
            let acc = self#constant x0 acc in
            let acc = self#constant x1 acc in
            acc
          | Ppat_tuple x0 ->
            let acc = self#list self#pattern x0 acc in
            acc
          | Ppat_construct (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#option self#pattern x1 acc in
            acc
          | Ppat_variant (x0, x1) ->
            let acc = self#label x0 acc in
            let acc = self#option self#pattern x1 acc in
            acc
          | Ppat_record (x0, x1) ->
            let acc = self#list (fun (x0, x1) acc -> let acc = self#longident_loc x0 acc in let acc = self#pattern x1 acc in acc) x0 acc in
            let acc = self#closed_flag x1 acc in
            acc
          | Ppat_array x0 ->
            let acc = self#list self#pattern x0 acc in
            acc
          | Ppat_or (x0, x1) ->
            let acc = self#pattern x0 acc in
            let acc = self#pattern x1 acc in
            acc
          | Ppat_constraint (x0, x1) ->
            let acc = self#pattern x0 acc in
            let acc = self#core_type x1 acc in
            acc
          | Ppat_type x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Ppat_lazy x0 ->
            let acc = self#pattern x0 acc in
            acc
          | Ppat_unpack x0 ->
            let acc = self#loc self#string x0 acc in
            acc
          | Ppat_exception x0 ->
            let acc = self#pattern x0 acc in
            acc
          | Ppat_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Ppat_open (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#pattern x1 acc in
            acc
      method expression : Expression.t -> 'acc -> 'acc  =
        fun expression acc ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let acc = self#expression_desc pexp_desc acc in
          let acc = self#location pexp_loc acc in
          let acc = self#attributes pexp_attributes acc in
          acc
      method expression_desc : Expression_desc.t -> 'acc -> 'acc  =
        fun expression_desc acc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Pexp_constant x0 ->
            let acc = self#constant x0 acc in
            acc
          | Pexp_let (x0, x1, x2) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#value_binding x1 acc in
            let acc = self#expression x2 acc in
            acc
          | Pexp_function x0 ->
            let acc = self#list self#case x0 acc in
            acc
          | Pexp_fun (x0, x1, x2, x3) ->
            let acc = self#arg_label x0 acc in
            let acc = self#option self#expression x1 acc in
            let acc = self#pattern x2 acc in
            let acc = self#expression x3 acc in
            acc
          | Pexp_apply (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#list (fun (x0, x1) acc -> let acc = self#arg_label x0 acc in let acc = self#expression x1 acc in acc) x1 acc in
            acc
          | Pexp_match (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#list self#case x1 acc in
            acc
          | Pexp_try (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#list self#case x1 acc in
            acc
          | Pexp_tuple x0 ->
            let acc = self#list self#expression x0 acc in
            acc
          | Pexp_construct (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#option self#expression x1 acc in
            acc
          | Pexp_variant (x0, x1) ->
            let acc = self#label x0 acc in
            let acc = self#option self#expression x1 acc in
            acc
          | Pexp_record (x0, x1) ->
            let acc = self#list (fun (x0, x1) acc -> let acc = self#longident_loc x0 acc in let acc = self#expression x1 acc in acc) x0 acc in
            let acc = self#option self#expression x1 acc in
            acc
          | Pexp_field (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#longident_loc x1 acc in
            acc
          | Pexp_setfield (x0, x1, x2) ->
            let acc = self#expression x0 acc in
            let acc = self#longident_loc x1 acc in
            let acc = self#expression x2 acc in
            acc
          | Pexp_array x0 ->
            let acc = self#list self#expression x0 acc in
            acc
          | Pexp_ifthenelse (x0, x1, x2) ->
            let acc = self#expression x0 acc in
            let acc = self#expression x1 acc in
            let acc = self#option self#expression x2 acc in
            acc
          | Pexp_sequence (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_while (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let acc = self#pattern x0 acc in
            let acc = self#expression x1 acc in
            let acc = self#expression x2 acc in
            let acc = self#direction_flag x3 acc in
            let acc = self#expression x4 acc in
            acc
          | Pexp_constraint (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#core_type x1 acc in
            acc
          | Pexp_coerce (x0, x1, x2) ->
            let acc = self#expression x0 acc in
            let acc = self#option self#core_type x1 acc in
            let acc = self#core_type x2 acc in
            acc
          | Pexp_send (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#loc self#label x1 acc in
            acc
          | Pexp_new x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Pexp_setinstvar (x0, x1) ->
            let acc = self#loc self#label x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_override x0 ->
            let acc = self#list (fun (x0, x1) acc -> let acc = self#loc self#label x0 acc in let acc = self#expression x1 acc in acc) x0 acc in
            acc
          | Pexp_letmodule (x0, x1, x2) ->
            let acc = self#loc self#string x0 acc in
            let acc = self#module_expr x1 acc in
            let acc = self#expression x2 acc in
            acc
          | Pexp_letexception (x0, x1) ->
            let acc = self#extension_constructor x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_assert x0 ->
            let acc = self#expression x0 acc in
            acc
          | Pexp_lazy x0 ->
            let acc = self#expression x0 acc in
            acc
          | Pexp_poly (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#option self#core_type x1 acc in
            acc
          | Pexp_object x0 ->
            let acc = self#class_structure x0 acc in
            acc
          | Pexp_newtype (x0, x1) ->
            let acc = self#loc self#string x0 acc in
            let acc = self#expression x1 acc in
            acc
          | Pexp_pack x0 ->
            let acc = self#module_expr x0 acc in
            acc
          | Pexp_open (x0, x1, x2) ->
            let acc = self#override_flag x0 acc in
            let acc = self#longident_loc x1 acc in
            let acc = self#expression x2 acc in
            acc
          | Pexp_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Pexp_unreachable ->
            acc
      method case : Case.t -> 'acc -> 'acc  =
        fun case acc ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let acc = self#pattern pc_lhs acc in
          let acc = self#option self#expression pc_guard acc in
          let acc = self#expression pc_rhs acc in
          acc
      method value_description : Value_description.t -> 'acc -> 'acc  =
        fun value_description acc ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let acc = self#loc self#string pval_name acc in
          let acc = self#core_type pval_type acc in
          let acc = self#list self#string pval_prim acc in
          let acc = self#attributes pval_attributes acc in
          let acc = self#location pval_loc acc in
          acc
      method type_declaration : Type_declaration.t -> 'acc -> 'acc  =
        fun type_declaration acc ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let acc = self#loc self#string ptype_name acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#variance x1 acc in acc) ptype_params acc in
          let acc = self#list (fun (x0, x1, x2) acc -> let acc = self#core_type x0 acc in let acc = self#core_type x1 acc in let acc = self#location x2 acc in acc) ptype_cstrs acc in
          let acc = self#type_kind ptype_kind acc in
          let acc = self#private_flag ptype_private acc in
          let acc = self#option self#core_type ptype_manifest acc in
          let acc = self#attributes ptype_attributes acc in
          let acc = self#location ptype_loc acc in
          acc
      method type_kind : Type_kind.t -> 'acc -> 'acc  =
        fun type_kind acc ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            acc
          | Ptype_variant x0 ->
            let acc = self#list self#constructor_declaration x0 acc in
            acc
          | Ptype_record x0 ->
            let acc = self#list self#label_declaration x0 acc in
            acc
          | Ptype_open ->
            acc
      method label_declaration : Label_declaration.t -> 'acc -> 'acc  =
        fun label_declaration acc ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let acc = self#loc self#string pld_name acc in
          let acc = self#mutable_flag pld_mutable acc in
          let acc = self#core_type pld_type acc in
          let acc = self#location pld_loc acc in
          let acc = self#attributes pld_attributes acc in
          acc
      method constructor_declaration : Constructor_declaration.t -> 'acc -> 'acc  =
        fun constructor_declaration acc ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let acc = self#loc self#string pcd_name acc in
          let acc = self#constructor_arguments pcd_args acc in
          let acc = self#option self#core_type pcd_res acc in
          let acc = self#location pcd_loc acc in
          let acc = self#attributes pcd_attributes acc in
          acc
      method constructor_arguments : Constructor_arguments.t -> 'acc -> 'acc  =
        fun constructor_arguments acc ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let acc = self#list self#core_type x0 acc in
            acc
          | Pcstr_record x0 ->
            let acc = self#list self#label_declaration x0 acc in
            acc
      method type_extension : Type_extension.t -> 'acc -> 'acc  =
        fun type_extension acc ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let acc = self#longident_loc ptyext_path acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#variance x1 acc in acc) ptyext_params acc in
          let acc = self#list self#extension_constructor ptyext_constructors acc in
          let acc = self#private_flag ptyext_private acc in
          let acc = self#attributes ptyext_attributes acc in
          acc
      method extension_constructor : Extension_constructor.t -> 'acc -> 'acc  =
        fun extension_constructor acc ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let acc = self#loc self#string pext_name acc in
          let acc = self#extension_constructor_kind pext_kind acc in
          let acc = self#location pext_loc acc in
          let acc = self#attributes pext_attributes acc in
          acc
      method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> 'acc  =
        fun extension_constructor_kind acc ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let acc = self#constructor_arguments x0 acc in
            let acc = self#option self#core_type x1 acc in
            acc
          | Pext_rebind x0 ->
            let acc = self#longident_loc x0 acc in
            acc
      method class_type : Class_type.t -> 'acc -> 'acc  =
        fun class_type acc ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let acc = self#class_type_desc pcty_desc acc in
          let acc = self#location pcty_loc acc in
          let acc = self#attributes pcty_attributes acc in
          acc
      method class_type_desc : Class_type_desc.t -> 'acc -> 'acc  =
        fun class_type_desc acc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#list self#core_type x1 acc in
            acc
          | Pcty_signature x0 ->
            let acc = self#class_signature x0 acc in
            acc
          | Pcty_arrow (x0, x1, x2) ->
            let acc = self#arg_label x0 acc in
            let acc = self#core_type x1 acc in
            let acc = self#class_type x2 acc in
            acc
          | Pcty_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Pcty_open (x0, x1, x2) ->
            let acc = self#override_flag x0 acc in
            let acc = self#longident_loc x1 acc in
            let acc = self#class_type x2 acc in
            acc
      method class_signature : Class_signature.t -> 'acc -> 'acc  =
        fun class_signature acc ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let acc = self#core_type pcsig_self acc in
          let acc = self#list self#class_type_field pcsig_fields acc in
          acc
      method class_type_field : Class_type_field.t -> 'acc -> 'acc  =
        fun class_type_field acc ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let acc = self#class_type_field_desc pctf_desc acc in
          let acc = self#location pctf_loc acc in
          let acc = self#attributes pctf_attributes acc in
          acc
      method class_type_field_desc : Class_type_field_desc.t -> 'acc -> 'acc  =
        fun class_type_field_desc acc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let acc = self#class_type x0 acc in
            acc
          | Pctf_val x0 ->
            let acc = (fun (x0, x1, x2, x3) acc -> let acc = self#loc self#label x0 acc in let acc = self#mutable_flag x1 acc in let acc = self#virtual_flag x2 acc in let acc = self#core_type x3 acc in acc) x0 acc in
            acc
          | Pctf_method x0 ->
            let acc = (fun (x0, x1, x2, x3) acc -> let acc = self#loc self#label x0 acc in let acc = self#private_flag x1 acc in let acc = self#virtual_flag x2 acc in let acc = self#core_type x3 acc in acc) x0 acc in
            acc
          | Pctf_constraint x0 ->
            let acc = (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#core_type x1 acc in acc) x0 acc in
            acc
          | Pctf_attribute x0 ->
            let acc = self#attribute x0 acc in
            acc
          | Pctf_extension x0 ->
            let acc = self#extension x0 acc in
            acc
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> 'acc  =
        fun class_infos acc ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let acc = self#virtual_flag pci_virt acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#variance x1 acc in acc) pci_params acc in
          let acc = self#loc self#string pci_name acc in
          let acc = self#class_expr pci_expr acc in
          let acc = self#location pci_loc acc in
          let acc = self#attributes pci_attributes acc in
          acc
      method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> 'acc  =
        fun class_infos acc ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let acc = self#virtual_flag pci_virt acc in
          let acc = self#list (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#variance x1 acc in acc) pci_params acc in
          let acc = self#loc self#string pci_name acc in
          let acc = self#class_type pci_expr acc in
          let acc = self#location pci_loc acc in
          let acc = self#attributes pci_attributes acc in
          acc
      method class_description : Class_description.t -> 'acc -> 'acc  =
        fun class_description acc ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let acc = self#class_infos_class_type concrete acc in
          acc
      method class_type_declaration : Class_type_declaration.t -> 'acc -> 'acc  =
        fun class_type_declaration acc ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let acc = self#class_infos_class_type concrete acc in
          acc
      method class_expr : Class_expr.t -> 'acc -> 'acc  =
        fun class_expr acc ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let acc = self#class_expr_desc pcl_desc acc in
          let acc = self#location pcl_loc acc in
          let acc = self#attributes pcl_attributes acc in
          acc
      method class_expr_desc : Class_expr_desc.t -> 'acc -> 'acc  =
        fun class_expr_desc acc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#list self#core_type x1 acc in
            acc
          | Pcl_structure x0 ->
            let acc = self#class_structure x0 acc in
            acc
          | Pcl_fun (x0, x1, x2, x3) ->
            let acc = self#arg_label x0 acc in
            let acc = self#option self#expression x1 acc in
            let acc = self#pattern x2 acc in
            let acc = self#class_expr x3 acc in
            acc
          | Pcl_apply (x0, x1) ->
            let acc = self#class_expr x0 acc in
            let acc = self#list (fun (x0, x1) acc -> let acc = self#arg_label x0 acc in let acc = self#expression x1 acc in acc) x1 acc in
            acc
          | Pcl_let (x0, x1, x2) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#value_binding x1 acc in
            let acc = self#class_expr x2 acc in
            acc
          | Pcl_constraint (x0, x1) ->
            let acc = self#class_expr x0 acc in
            let acc = self#class_type x1 acc in
            acc
          | Pcl_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Pcl_open (x0, x1, x2) ->
            let acc = self#override_flag x0 acc in
            let acc = self#longident_loc x1 acc in
            let acc = self#class_expr x2 acc in
            acc
      method class_structure : Class_structure.t -> 'acc -> 'acc  =
        fun class_structure acc ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let acc = self#pattern pcstr_self acc in
          let acc = self#list self#class_field pcstr_fields acc in
          acc
      method class_field : Class_field.t -> 'acc -> 'acc  =
        fun class_field acc ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let acc = self#class_field_desc pcf_desc acc in
          let acc = self#location pcf_loc acc in
          let acc = self#attributes pcf_attributes acc in
          acc
      method class_field_desc : Class_field_desc.t -> 'acc -> 'acc  =
        fun class_field_desc acc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let acc = self#override_flag x0 acc in
            let acc = self#class_expr x1 acc in
            let acc = self#option (self#loc self#string) x2 acc in
            acc
          | Pcf_val x0 ->
            let acc = (fun (x0, x1, x2) acc -> let acc = self#loc self#label x0 acc in let acc = self#mutable_flag x1 acc in let acc = self#class_field_kind x2 acc in acc) x0 acc in
            acc
          | Pcf_method x0 ->
            let acc = (fun (x0, x1, x2) acc -> let acc = self#loc self#label x0 acc in let acc = self#private_flag x1 acc in let acc = self#class_field_kind x2 acc in acc) x0 acc in
            acc
          | Pcf_constraint x0 ->
            let acc = (fun (x0, x1) acc -> let acc = self#core_type x0 acc in let acc = self#core_type x1 acc in acc) x0 acc in
            acc
          | Pcf_initializer x0 ->
            let acc = self#expression x0 acc in
            acc
          | Pcf_attribute x0 ->
            let acc = self#attribute x0 acc in
            acc
          | Pcf_extension x0 ->
            let acc = self#extension x0 acc in
            acc
      method class_field_kind : Class_field_kind.t -> 'acc -> 'acc  =
        fun class_field_kind acc ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let acc = self#core_type x0 acc in
            acc
          | Cfk_concrete (x0, x1) ->
            let acc = self#override_flag x0 acc in
            let acc = self#expression x1 acc in
            acc
      method class_declaration : Class_declaration.t -> 'acc -> 'acc  =
        fun class_declaration acc ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let acc = self#class_infos_class_expr concrete acc in
          acc
      method module_type : Module_type.t -> 'acc -> 'acc  =
        fun module_type acc ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let acc = self#module_type_desc pmty_desc acc in
          let acc = self#location pmty_loc acc in
          let acc = self#attributes pmty_attributes acc in
          acc
      method module_type_desc : Module_type_desc.t -> 'acc -> 'acc  =
        fun module_type_desc acc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Pmty_signature x0 ->
            let acc = self#signature x0 acc in
            acc
          | Pmty_functor (x0, x1, x2) ->
            let acc = self#loc self#string x0 acc in
            let acc = self#option self#module_type x1 acc in
            let acc = self#module_type x2 acc in
            acc
          | Pmty_with (x0, x1) ->
            let acc = self#module_type x0 acc in
            let acc = self#list self#with_constraint x1 acc in
            acc
          | Pmty_typeof x0 ->
            let acc = self#module_expr x0 acc in
            acc
          | Pmty_extension x0 ->
            let acc = self#extension x0 acc in
            acc
          | Pmty_alias x0 ->
            let acc = self#longident_loc x0 acc in
            acc
      method signature : Signature.t -> 'acc -> 'acc  =
        fun signature acc ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let acc = self#list self#signature_item concrete acc in
          acc
      method signature_item : Signature_item.t -> 'acc -> 'acc  =
        fun signature_item acc ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let acc = self#signature_item_desc psig_desc acc in
          let acc = self#location psig_loc acc in
          acc
      method signature_item_desc : Signature_item_desc.t -> 'acc -> 'acc  =
        fun signature_item_desc acc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let acc = self#value_description x0 acc in
            acc
          | Psig_type (x0, x1) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#type_declaration x1 acc in
            acc
          | Psig_typext x0 ->
            let acc = self#type_extension x0 acc in
            acc
          | Psig_exception x0 ->
            let acc = self#extension_constructor x0 acc in
            acc
          | Psig_module x0 ->
            let acc = self#module_declaration x0 acc in
            acc
          | Psig_recmodule x0 ->
            let acc = self#list self#module_declaration x0 acc in
            acc
          | Psig_modtype x0 ->
            let acc = self#module_type_declaration x0 acc in
            acc
          | Psig_open x0 ->
            let acc = self#open_description x0 acc in
            acc
          | Psig_include x0 ->
            let acc = self#include_description x0 acc in
            acc
          | Psig_class x0 ->
            let acc = self#list self#class_description x0 acc in
            acc
          | Psig_class_type x0 ->
            let acc = self#list self#class_type_declaration x0 acc in
            acc
          | Psig_attribute x0 ->
            let acc = self#attribute x0 acc in
            acc
          | Psig_extension (x0, x1) ->
            let acc = self#extension x0 acc in
            let acc = self#attributes x1 acc in
            acc
      method module_declaration : Module_declaration.t -> 'acc -> 'acc  =
        fun module_declaration acc ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let acc = self#loc self#string pmd_name acc in
          let acc = self#module_type pmd_type acc in
          let acc = self#attributes pmd_attributes acc in
          let acc = self#location pmd_loc acc in
          acc
      method module_type_declaration : Module_type_declaration.t -> 'acc -> 'acc  =
        fun module_type_declaration acc ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let acc = self#loc self#string pmtd_name acc in
          let acc = self#option self#module_type pmtd_type acc in
          let acc = self#attributes pmtd_attributes acc in
          let acc = self#location pmtd_loc acc in
          acc
      method open_description : Open_description.t -> 'acc -> 'acc  =
        fun open_description acc ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let acc = self#longident_loc popen_lid acc in
          let acc = self#override_flag popen_override acc in
          let acc = self#location popen_loc acc in
          let acc = self#attributes popen_attributes acc in
          acc
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> 'acc  =
        fun include_infos acc ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let acc = self#module_expr pincl_mod acc in
          let acc = self#location pincl_loc acc in
          let acc = self#attributes pincl_attributes acc in
          acc
      method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> 'acc  =
        fun include_infos acc ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let acc = self#module_type pincl_mod acc in
          let acc = self#location pincl_loc acc in
          let acc = self#attributes pincl_attributes acc in
          acc
      method include_description : Include_description.t -> 'acc -> 'acc  =
        fun include_description acc ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let acc = self#include_infos_module_type concrete acc in
          acc
      method include_declaration : Include_declaration.t -> 'acc -> 'acc  =
        fun include_declaration acc ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let acc = self#include_infos_module_expr concrete acc in
          acc
      method with_constraint : With_constraint.t -> 'acc -> 'acc  =
        fun with_constraint acc ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#type_declaration x1 acc in
            acc
          | Pwith_module (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#longident_loc x1 acc in
            acc
          | Pwith_typesubst (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#type_declaration x1 acc in
            acc
          | Pwith_modsubst (x0, x1) ->
            let acc = self#longident_loc x0 acc in
            let acc = self#longident_loc x1 acc in
            acc
      method module_expr : Module_expr.t -> 'acc -> 'acc  =
        fun module_expr acc ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let acc = self#module_expr_desc pmod_desc acc in
          let acc = self#location pmod_loc acc in
          let acc = self#attributes pmod_attributes acc in
          acc
      method module_expr_desc : Module_expr_desc.t -> 'acc -> 'acc  =
        fun module_expr_desc acc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let acc = self#longident_loc x0 acc in
            acc
          | Pmod_structure x0 ->
            let acc = self#structure x0 acc in
            acc
          | Pmod_functor (x0, x1, x2) ->
            let acc = self#loc self#string x0 acc in
            let acc = self#option self#module_type x1 acc in
            let acc = self#module_expr x2 acc in
            acc
          | Pmod_apply (x0, x1) ->
            let acc = self#module_expr x0 acc in
            let acc = self#module_expr x1 acc in
            acc
          | Pmod_constraint (x0, x1) ->
            let acc = self#module_expr x0 acc in
            let acc = self#module_type x1 acc in
            acc
          | Pmod_unpack x0 ->
            let acc = self#expression x0 acc in
            acc
          | Pmod_extension x0 ->
            let acc = self#extension x0 acc in
            acc
      method structure : Structure.t -> 'acc -> 'acc  =
        fun structure acc ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let acc = self#list self#structure_item concrete acc in
          acc
      method structure_item : Structure_item.t -> 'acc -> 'acc  =
        fun structure_item acc ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let acc = self#structure_item_desc pstr_desc acc in
          let acc = self#location pstr_loc acc in
          acc
      method structure_item_desc : Structure_item_desc.t -> 'acc -> 'acc  =
        fun structure_item_desc acc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let acc = self#expression x0 acc in
            let acc = self#attributes x1 acc in
            acc
          | Pstr_value (x0, x1) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#value_binding x1 acc in
            acc
          | Pstr_primitive x0 ->
            let acc = self#value_description x0 acc in
            acc
          | Pstr_type (x0, x1) ->
            let acc = self#rec_flag x0 acc in
            let acc = self#list self#type_declaration x1 acc in
            acc
          | Pstr_typext x0 ->
            let acc = self#type_extension x0 acc in
            acc
          | Pstr_exception x0 ->
            let acc = self#extension_constructor x0 acc in
            acc
          | Pstr_module x0 ->
            let acc = self#module_binding x0 acc in
            acc
          | Pstr_recmodule x0 ->
            let acc = self#list self#module_binding x0 acc in
            acc
          | Pstr_modtype x0 ->
            let acc = self#module_type_declaration x0 acc in
            acc
          | Pstr_open x0 ->
            let acc = self#open_description x0 acc in
            acc
          | Pstr_class x0 ->
            let acc = self#list self#class_declaration x0 acc in
            acc
          | Pstr_class_type x0 ->
            let acc = self#list self#class_type_declaration x0 acc in
            acc
          | Pstr_include x0 ->
            let acc = self#include_declaration x0 acc in
            acc
          | Pstr_attribute x0 ->
            let acc = self#attribute x0 acc in
            acc
          | Pstr_extension (x0, x1) ->
            let acc = self#extension x0 acc in
            let acc = self#attributes x1 acc in
            acc
      method value_binding : Value_binding.t -> 'acc -> 'acc  =
        fun value_binding acc ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let acc = self#pattern pvb_pat acc in
          let acc = self#expression pvb_expr acc in
          let acc = self#attributes pvb_attributes acc in
          let acc = self#location pvb_loc acc in
          acc
      method module_binding : Module_binding.t -> 'acc -> 'acc  =
        fun module_binding acc ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let acc = self#loc self#string pmb_name acc in
          let acc = self#module_expr pmb_expr acc in
          let acc = self#attributes pmb_attributes acc in
          let acc = self#location pmb_loc acc in
          acc
      method toplevel_phrase : Toplevel_phrase.t -> 'acc -> 'acc  =
        fun toplevel_phrase acc ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let acc = self#structure x0 acc in
            acc
          | Ptop_dir (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#directive_argument x1 acc in
            acc
      method directive_argument : Directive_argument.t -> 'acc -> 'acc  =
        fun directive_argument acc ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            acc
          | Pdir_string x0 ->
            let acc = self#string x0 acc in
            acc
          | Pdir_int (x0, x1) ->
            let acc = self#string x0 acc in
            let acc = self#option self#char x1 acc in
            acc
          | Pdir_ident x0 ->
            let acc = self#longident x0 acc in
            acc
          | Pdir_bool x0 ->
            let acc = self#bool x0 acc in
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
      method longident : Longident.t -> 'acc -> (Longident.t * 'acc)  =
        fun longident acc ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let (x0, acc) = self#string x0 acc in
            (Longident.of_concrete (Lident x0), acc)
          | Ldot (x0, x1) ->
            let (x0, acc) = self#longident x0 acc in
            let (x1, acc) = self#string x1 acc in
            (Longident.of_concrete (Ldot (x0, x1)), acc)
          | Lapply (x0, x1) ->
            let (x0, acc) = self#longident x0 acc in
            let (x1, acc) = self#longident x1 acc in
            (Longident.of_concrete (Lapply (x0, x1)), acc)
      method longident_loc : Longident_loc.t -> 'acc -> (Longident_loc.t * 'acc)  =
        fun longident_loc acc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let (concrete, acc) = self#loc self#longident concrete acc in
          (Longident_loc.of_concrete concrete, acc)
      method rec_flag : Rec_flag.t -> 'acc -> (Rec_flag.t * 'acc)  =
        fun rec_flag acc ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            (Rec_flag.of_concrete Nonrecursive, acc)
          | Recursive ->
            (Rec_flag.of_concrete Recursive, acc)
      method direction_flag : Direction_flag.t -> 'acc -> (Direction_flag.t * 'acc)  =
        fun direction_flag acc ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            (Direction_flag.of_concrete Upto, acc)
          | Downto ->
            (Direction_flag.of_concrete Downto, acc)
      method private_flag : Private_flag.t -> 'acc -> (Private_flag.t * 'acc)  =
        fun private_flag acc ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            (Private_flag.of_concrete Private, acc)
          | Public ->
            (Private_flag.of_concrete Public, acc)
      method mutable_flag : Mutable_flag.t -> 'acc -> (Mutable_flag.t * 'acc)  =
        fun mutable_flag acc ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            (Mutable_flag.of_concrete Immutable, acc)
          | Mutable ->
            (Mutable_flag.of_concrete Mutable, acc)
      method virtual_flag : Virtual_flag.t -> 'acc -> (Virtual_flag.t * 'acc)  =
        fun virtual_flag acc ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            (Virtual_flag.of_concrete Virtual, acc)
          | Concrete ->
            (Virtual_flag.of_concrete Concrete, acc)
      method override_flag : Override_flag.t -> 'acc -> (Override_flag.t * 'acc)  =
        fun override_flag acc ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            (Override_flag.of_concrete Override, acc)
          | Fresh ->
            (Override_flag.of_concrete Fresh, acc)
      method closed_flag : Closed_flag.t -> 'acc -> (Closed_flag.t * 'acc)  =
        fun closed_flag acc ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            (Closed_flag.of_concrete Closed, acc)
          | Open ->
            (Closed_flag.of_concrete Open, acc)
      method label : Label.t -> 'acc -> (Label.t * 'acc)  =
        fun label acc ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let (concrete, acc) = self#string concrete acc in
          (Label.of_concrete concrete, acc)
      method arg_label : Arg_label.t -> 'acc -> (Arg_label.t * 'acc)  =
        fun arg_label acc ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            (Arg_label.of_concrete Nolabel, acc)
          | Labelled x0 ->
            let (x0, acc) = self#string x0 acc in
            (Arg_label.of_concrete (Labelled x0), acc)
          | Optional x0 ->
            let (x0, acc) = self#string x0 acc in
            (Arg_label.of_concrete (Optional x0), acc)
      method variance : Variance.t -> 'acc -> (Variance.t * 'acc)  =
        fun variance acc ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            (Variance.of_concrete Covariant, acc)
          | Contravariant ->
            (Variance.of_concrete Contravariant, acc)
          | Invariant ->
            (Variance.of_concrete Invariant, acc)
      method constant : Constant.t -> 'acc -> (Constant.t * 'acc)  =
        fun constant acc ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#option self#char x1 acc in
            (Constant.of_concrete (Pconst_integer (x0, x1)), acc)
          | Pconst_char x0 ->
            let (x0, acc) = self#char x0 acc in
            (Constant.of_concrete (Pconst_char x0), acc)
          | Pconst_string (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#option self#string x1 acc in
            (Constant.of_concrete (Pconst_string (x0, x1)), acc)
          | Pconst_float (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#option self#char x1 acc in
            (Constant.of_concrete (Pconst_float (x0, x1)), acc)
      method attribute : Attribute.t -> 'acc -> (Attribute.t * 'acc)  =
        fun attribute acc ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let (x0, acc) = self#loc self#string x0 acc in
          let (x1, acc) = self#payload x1 acc in
          (Attribute.of_concrete (x0, x1), acc)
      method extension : Extension.t -> 'acc -> (Extension.t * 'acc)  =
        fun extension acc ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let (x0, acc) = self#loc self#string x0 acc in
          let (x1, acc) = self#payload x1 acc in
          (Extension.of_concrete (x0, x1), acc)
      method attributes : Attributes.t -> 'acc -> (Attributes.t * 'acc)  =
        fun attributes acc ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let (concrete, acc) = self#list self#attribute concrete acc in
          (Attributes.of_concrete concrete, acc)
      method payload : Payload.t -> 'acc -> (Payload.t * 'acc)  =
        fun payload acc ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let (x0, acc) = self#structure x0 acc in
            (Payload.of_concrete (PStr x0), acc)
          | PSig x0 ->
            let (x0, acc) = self#signature x0 acc in
            (Payload.of_concrete (PSig x0), acc)
          | PTyp x0 ->
            let (x0, acc) = self#core_type x0 acc in
            (Payload.of_concrete (PTyp x0), acc)
          | PPat (x0, x1) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            (Payload.of_concrete (PPat (x0, x1)), acc)
      method core_type : Core_type.t -> 'acc -> (Core_type.t * 'acc)  =
        fun core_type acc ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let (ptyp_desc, acc) = self#core_type_desc ptyp_desc acc in
          let (ptyp_loc, acc) = self#location ptyp_loc acc in
          let (ptyp_attributes, acc) = self#attributes ptyp_attributes acc in
          (Core_type.of_concrete { ptyp_desc; ptyp_loc; ptyp_attributes }, acc)
      method core_type_desc : Core_type_desc.t -> 'acc -> (Core_type_desc.t * 'acc)  =
        fun core_type_desc acc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            (Core_type_desc.of_concrete Ptyp_any, acc)
          | Ptyp_var x0 ->
            let (x0, acc) = self#string x0 acc in
            (Core_type_desc.of_concrete (Ptyp_var x0), acc)
          | Ptyp_arrow (x0, x1, x2) ->
            let (x0, acc) = self#arg_label x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            let (x2, acc) = self#core_type x2 acc in
            (Core_type_desc.of_concrete (Ptyp_arrow (x0, x1, x2)), acc)
          | Ptyp_tuple x0 ->
            let (x0, acc) = self#list self#core_type x0 acc in
            (Core_type_desc.of_concrete (Ptyp_tuple x0), acc)
          | Ptyp_constr (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#list self#core_type x1 acc in
            (Core_type_desc.of_concrete (Ptyp_constr (x0, x1)), acc)
          | Ptyp_object (x0, x1) ->
            let (x0, acc) = self#list self#object_field x0 acc in
            let (x1, acc) = self#closed_flag x1 acc in
            (Core_type_desc.of_concrete (Ptyp_object (x0, x1)), acc)
          | Ptyp_class (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#list self#core_type x1 acc in
            (Core_type_desc.of_concrete (Ptyp_class (x0, x1)), acc)
          | Ptyp_alias (x0, x1) ->
            let (x0, acc) = self#core_type x0 acc in
            let (x1, acc) = self#string x1 acc in
            (Core_type_desc.of_concrete (Ptyp_alias (x0, x1)), acc)
          | Ptyp_variant (x0, x1, x2) ->
            let (x0, acc) = self#list self#row_field x0 acc in
            let (x1, acc) = self#closed_flag x1 acc in
            let (x2, acc) = self#option (self#list self#label) x2 acc in
            (Core_type_desc.of_concrete (Ptyp_variant (x0, x1, x2)), acc)
          | Ptyp_poly (x0, x1) ->
            let (x0, acc) = self#list (self#loc self#string) x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            (Core_type_desc.of_concrete (Ptyp_poly (x0, x1)), acc)
          | Ptyp_package x0 ->
            let (x0, acc) = self#package_type x0 acc in
            (Core_type_desc.of_concrete (Ptyp_package x0), acc)
          | Ptyp_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Core_type_desc.of_concrete (Ptyp_extension x0), acc)
      method package_type : Package_type.t -> 'acc -> (Package_type.t * 'acc)  =
        fun package_type acc ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let (x0, acc) = self#longident_loc x0 acc in
          let (x1, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#longident_loc x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) x1 acc in
          (Package_type.of_concrete (x0, x1), acc)
      method row_field : Row_field.t -> 'acc -> (Row_field.t * 'acc)  =
        fun row_field acc ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let (x0, acc) = self#loc self#label x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            let (x2, acc) = self#bool x2 acc in
            let (x3, acc) = self#list self#core_type x3 acc in
            (Row_field.of_concrete (Rtag (x0, x1, x2, x3)), acc)
          | Rinherit x0 ->
            let (x0, acc) = self#core_type x0 acc in
            (Row_field.of_concrete (Rinherit x0), acc)
      method object_field : Object_field.t -> 'acc -> (Object_field.t * 'acc)  =
        fun object_field acc ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let (x0, acc) = self#loc self#label x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            let (x2, acc) = self#core_type x2 acc in
            (Object_field.of_concrete (Otag (x0, x1, x2)), acc)
          | Oinherit x0 ->
            let (x0, acc) = self#core_type x0 acc in
            (Object_field.of_concrete (Oinherit x0), acc)
      method pattern : Pattern.t -> 'acc -> (Pattern.t * 'acc)  =
        fun pattern acc ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let (ppat_desc, acc) = self#pattern_desc ppat_desc acc in
          let (ppat_loc, acc) = self#location ppat_loc acc in
          let (ppat_attributes, acc) = self#attributes ppat_attributes acc in
          (Pattern.of_concrete { ppat_desc; ppat_loc; ppat_attributes }, acc)
      method pattern_desc : Pattern_desc.t -> 'acc -> (Pattern_desc.t * 'acc)  =
        fun pattern_desc acc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            (Pattern_desc.of_concrete Ppat_any, acc)
          | Ppat_var x0 ->
            let (x0, acc) = self#loc self#string x0 acc in
            (Pattern_desc.of_concrete (Ppat_var x0), acc)
          | Ppat_alias (x0, x1) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#loc self#string x1 acc in
            (Pattern_desc.of_concrete (Ppat_alias (x0, x1)), acc)
          | Ppat_constant x0 ->
            let (x0, acc) = self#constant x0 acc in
            (Pattern_desc.of_concrete (Ppat_constant x0), acc)
          | Ppat_interval (x0, x1) ->
            let (x0, acc) = self#constant x0 acc in
            let (x1, acc) = self#constant x1 acc in
            (Pattern_desc.of_concrete (Ppat_interval (x0, x1)), acc)
          | Ppat_tuple x0 ->
            let (x0, acc) = self#list self#pattern x0 acc in
            (Pattern_desc.of_concrete (Ppat_tuple x0), acc)
          | Ppat_construct (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#option self#pattern x1 acc in
            (Pattern_desc.of_concrete (Ppat_construct (x0, x1)), acc)
          | Ppat_variant (x0, x1) ->
            let (x0, acc) = self#label x0 acc in
            let (x1, acc) = self#option self#pattern x1 acc in
            (Pattern_desc.of_concrete (Ppat_variant (x0, x1)), acc)
          | Ppat_record (x0, x1) ->
            let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#longident_loc x0 acc in let (x1, acc) = self#pattern x1 acc in ((x0, x1), acc)) x0 acc in
            let (x1, acc) = self#closed_flag x1 acc in
            (Pattern_desc.of_concrete (Ppat_record (x0, x1)), acc)
          | Ppat_array x0 ->
            let (x0, acc) = self#list self#pattern x0 acc in
            (Pattern_desc.of_concrete (Ppat_array x0), acc)
          | Ppat_or (x0, x1) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#pattern x1 acc in
            (Pattern_desc.of_concrete (Ppat_or (x0, x1)), acc)
          | Ppat_constraint (x0, x1) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            (Pattern_desc.of_concrete (Ppat_constraint (x0, x1)), acc)
          | Ppat_type x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Pattern_desc.of_concrete (Ppat_type x0), acc)
          | Ppat_lazy x0 ->
            let (x0, acc) = self#pattern x0 acc in
            (Pattern_desc.of_concrete (Ppat_lazy x0), acc)
          | Ppat_unpack x0 ->
            let (x0, acc) = self#loc self#string x0 acc in
            (Pattern_desc.of_concrete (Ppat_unpack x0), acc)
          | Ppat_exception x0 ->
            let (x0, acc) = self#pattern x0 acc in
            (Pattern_desc.of_concrete (Ppat_exception x0), acc)
          | Ppat_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Pattern_desc.of_concrete (Ppat_extension x0), acc)
          | Ppat_open (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#pattern x1 acc in
            (Pattern_desc.of_concrete (Ppat_open (x0, x1)), acc)
      method expression : Expression.t -> 'acc -> (Expression.t * 'acc)  =
        fun expression acc ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let (pexp_desc, acc) = self#expression_desc pexp_desc acc in
          let (pexp_loc, acc) = self#location pexp_loc acc in
          let (pexp_attributes, acc) = self#attributes pexp_attributes acc in
          (Expression.of_concrete { pexp_desc; pexp_loc; pexp_attributes }, acc)
      method expression_desc : Expression_desc.t -> 'acc -> (Expression_desc.t * 'acc)  =
        fun expression_desc acc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Expression_desc.of_concrete (Pexp_ident x0), acc)
          | Pexp_constant x0 ->
            let (x0, acc) = self#constant x0 acc in
            (Expression_desc.of_concrete (Pexp_constant x0), acc)
          | Pexp_let (x0, x1, x2) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#value_binding x1 acc in
            let (x2, acc) = self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_let (x0, x1, x2)), acc)
          | Pexp_function x0 ->
            let (x0, acc) = self#list self#case x0 acc in
            (Expression_desc.of_concrete (Pexp_function x0), acc)
          | Pexp_fun (x0, x1, x2, x3) ->
            let (x0, acc) = self#arg_label x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            let (x2, acc) = self#pattern x2 acc in
            let (x3, acc) = self#expression x3 acc in
            (Expression_desc.of_concrete (Pexp_fun (x0, x1, x2, x3)), acc)
          | Pexp_apply (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#arg_label x0 acc in let (x1, acc) = self#expression x1 acc in ((x0, x1), acc)) x1 acc in
            (Expression_desc.of_concrete (Pexp_apply (x0, x1)), acc)
          | Pexp_match (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#list self#case x1 acc in
            (Expression_desc.of_concrete (Pexp_match (x0, x1)), acc)
          | Pexp_try (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#list self#case x1 acc in
            (Expression_desc.of_concrete (Pexp_try (x0, x1)), acc)
          | Pexp_tuple x0 ->
            let (x0, acc) = self#list self#expression x0 acc in
            (Expression_desc.of_concrete (Pexp_tuple x0), acc)
          | Pexp_construct (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_construct (x0, x1)), acc)
          | Pexp_variant (x0, x1) ->
            let (x0, acc) = self#label x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_variant (x0, x1)), acc)
          | Pexp_record (x0, x1) ->
            let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#longident_loc x0 acc in let (x1, acc) = self#expression x1 acc in ((x0, x1), acc)) x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_record (x0, x1)), acc)
          | Pexp_field (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            (Expression_desc.of_concrete (Pexp_field (x0, x1)), acc)
          | Pexp_setfield (x0, x1, x2) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            let (x2, acc) = self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_setfield (x0, x1, x2)), acc)
          | Pexp_array x0 ->
            let (x0, acc) = self#list self#expression x0 acc in
            (Expression_desc.of_concrete (Pexp_array x0), acc)
          | Pexp_ifthenelse (x0, x1, x2) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#expression x1 acc in
            let (x2, acc) = self#option self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_ifthenelse (x0, x1, x2)), acc)
          | Pexp_sequence (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_sequence (x0, x1)), acc)
          | Pexp_while (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_while (x0, x1)), acc)
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let (x0, acc) = self#pattern x0 acc in
            let (x1, acc) = self#expression x1 acc in
            let (x2, acc) = self#expression x2 acc in
            let (x3, acc) = self#direction_flag x3 acc in
            let (x4, acc) = self#expression x4 acc in
            (Expression_desc.of_concrete (Pexp_for (x0, x1, x2, x3, x4)), acc)
          | Pexp_constraint (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            (Expression_desc.of_concrete (Pexp_constraint (x0, x1)), acc)
          | Pexp_coerce (x0, x1, x2) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#option self#core_type x1 acc in
            let (x2, acc) = self#core_type x2 acc in
            (Expression_desc.of_concrete (Pexp_coerce (x0, x1, x2)), acc)
          | Pexp_send (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#loc self#label x1 acc in
            (Expression_desc.of_concrete (Pexp_send (x0, x1)), acc)
          | Pexp_new x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Expression_desc.of_concrete (Pexp_new x0), acc)
          | Pexp_setinstvar (x0, x1) ->
            let (x0, acc) = self#loc self#label x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_setinstvar (x0, x1)), acc)
          | Pexp_override x0 ->
            let (x0, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#expression x1 acc in ((x0, x1), acc)) x0 acc in
            (Expression_desc.of_concrete (Pexp_override x0), acc)
          | Pexp_letmodule (x0, x1, x2) ->
            let (x0, acc) = self#loc self#string x0 acc in
            let (x1, acc) = self#module_expr x1 acc in
            let (x2, acc) = self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_letmodule (x0, x1, x2)), acc)
          | Pexp_letexception (x0, x1) ->
            let (x0, acc) = self#extension_constructor x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_letexception (x0, x1)), acc)
          | Pexp_assert x0 ->
            let (x0, acc) = self#expression x0 acc in
            (Expression_desc.of_concrete (Pexp_assert x0), acc)
          | Pexp_lazy x0 ->
            let (x0, acc) = self#expression x0 acc in
            (Expression_desc.of_concrete (Pexp_lazy x0), acc)
          | Pexp_poly (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#option self#core_type x1 acc in
            (Expression_desc.of_concrete (Pexp_poly (x0, x1)), acc)
          | Pexp_object x0 ->
            let (x0, acc) = self#class_structure x0 acc in
            (Expression_desc.of_concrete (Pexp_object x0), acc)
          | Pexp_newtype (x0, x1) ->
            let (x0, acc) = self#loc self#string x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Expression_desc.of_concrete (Pexp_newtype (x0, x1)), acc)
          | Pexp_pack x0 ->
            let (x0, acc) = self#module_expr x0 acc in
            (Expression_desc.of_concrete (Pexp_pack x0), acc)
          | Pexp_open (x0, x1, x2) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            let (x2, acc) = self#expression x2 acc in
            (Expression_desc.of_concrete (Pexp_open (x0, x1, x2)), acc)
          | Pexp_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Expression_desc.of_concrete (Pexp_extension x0), acc)
          | Pexp_unreachable ->
            (Expression_desc.of_concrete Pexp_unreachable, acc)
      method case : Case.t -> 'acc -> (Case.t * 'acc)  =
        fun case acc ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let (pc_lhs, acc) = self#pattern pc_lhs acc in
          let (pc_guard, acc) = self#option self#expression pc_guard acc in
          let (pc_rhs, acc) = self#expression pc_rhs acc in
          (Case.of_concrete { pc_lhs; pc_guard; pc_rhs }, acc)
      method value_description : Value_description.t -> 'acc -> (Value_description.t * 'acc)  =
        fun value_description acc ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let (pval_name, acc) = self#loc self#string pval_name acc in
          let (pval_type, acc) = self#core_type pval_type acc in
          let (pval_prim, acc) = self#list self#string pval_prim acc in
          let (pval_attributes, acc) = self#attributes pval_attributes acc in
          let (pval_loc, acc) = self#location pval_loc acc in
          (Value_description.of_concrete { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }, acc)
      method type_declaration : Type_declaration.t -> 'acc -> (Type_declaration.t * 'acc)  =
        fun type_declaration acc ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let (ptype_name, acc) = self#loc self#string ptype_name acc in
          let (ptype_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#variance x1 acc in ((x0, x1), acc)) ptype_params acc in
          let (ptype_cstrs, acc) = self#list (fun (x0, x1, x2) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#core_type x1 acc in let (x2, acc) = self#location x2 acc in ((x0, x1, x2), acc)) ptype_cstrs acc in
          let (ptype_kind, acc) = self#type_kind ptype_kind acc in
          let (ptype_private, acc) = self#private_flag ptype_private acc in
          let (ptype_manifest, acc) = self#option self#core_type ptype_manifest acc in
          let (ptype_attributes, acc) = self#attributes ptype_attributes acc in
          let (ptype_loc, acc) = self#location ptype_loc acc in
          (Type_declaration.of_concrete { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }, acc)
      method type_kind : Type_kind.t -> 'acc -> (Type_kind.t * 'acc)  =
        fun type_kind acc ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            (Type_kind.of_concrete Ptype_abstract, acc)
          | Ptype_variant x0 ->
            let (x0, acc) = self#list self#constructor_declaration x0 acc in
            (Type_kind.of_concrete (Ptype_variant x0), acc)
          | Ptype_record x0 ->
            let (x0, acc) = self#list self#label_declaration x0 acc in
            (Type_kind.of_concrete (Ptype_record x0), acc)
          | Ptype_open ->
            (Type_kind.of_concrete Ptype_open, acc)
      method label_declaration : Label_declaration.t -> 'acc -> (Label_declaration.t * 'acc)  =
        fun label_declaration acc ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let (pld_name, acc) = self#loc self#string pld_name acc in
          let (pld_mutable, acc) = self#mutable_flag pld_mutable acc in
          let (pld_type, acc) = self#core_type pld_type acc in
          let (pld_loc, acc) = self#location pld_loc acc in
          let (pld_attributes, acc) = self#attributes pld_attributes acc in
          (Label_declaration.of_concrete { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }, acc)
      method constructor_declaration : Constructor_declaration.t -> 'acc -> (Constructor_declaration.t * 'acc)  =
        fun constructor_declaration acc ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let (pcd_name, acc) = self#loc self#string pcd_name acc in
          let (pcd_args, acc) = self#constructor_arguments pcd_args acc in
          let (pcd_res, acc) = self#option self#core_type pcd_res acc in
          let (pcd_loc, acc) = self#location pcd_loc acc in
          let (pcd_attributes, acc) = self#attributes pcd_attributes acc in
          (Constructor_declaration.of_concrete { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }, acc)
      method constructor_arguments : Constructor_arguments.t -> 'acc -> (Constructor_arguments.t * 'acc)  =
        fun constructor_arguments acc ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let (x0, acc) = self#list self#core_type x0 acc in
            (Constructor_arguments.of_concrete (Pcstr_tuple x0), acc)
          | Pcstr_record x0 ->
            let (x0, acc) = self#list self#label_declaration x0 acc in
            (Constructor_arguments.of_concrete (Pcstr_record x0), acc)
      method type_extension : Type_extension.t -> 'acc -> (Type_extension.t * 'acc)  =
        fun type_extension acc ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let (ptyext_path, acc) = self#longident_loc ptyext_path acc in
          let (ptyext_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#variance x1 acc in ((x0, x1), acc)) ptyext_params acc in
          let (ptyext_constructors, acc) = self#list self#extension_constructor ptyext_constructors acc in
          let (ptyext_private, acc) = self#private_flag ptyext_private acc in
          let (ptyext_attributes, acc) = self#attributes ptyext_attributes acc in
          (Type_extension.of_concrete { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }, acc)
      method extension_constructor : Extension_constructor.t -> 'acc -> (Extension_constructor.t * 'acc)  =
        fun extension_constructor acc ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let (pext_name, acc) = self#loc self#string pext_name acc in
          let (pext_kind, acc) = self#extension_constructor_kind pext_kind acc in
          let (pext_loc, acc) = self#location pext_loc acc in
          let (pext_attributes, acc) = self#attributes pext_attributes acc in
          (Extension_constructor.of_concrete { pext_name; pext_kind; pext_loc; pext_attributes }, acc)
      method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> (Extension_constructor_kind.t * 'acc)  =
        fun extension_constructor_kind acc ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let (x0, acc) = self#constructor_arguments x0 acc in
            let (x1, acc) = self#option self#core_type x1 acc in
            (Extension_constructor_kind.of_concrete (Pext_decl (x0, x1)), acc)
          | Pext_rebind x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Extension_constructor_kind.of_concrete (Pext_rebind x0), acc)
      method class_type : Class_type.t -> 'acc -> (Class_type.t * 'acc)  =
        fun class_type acc ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let (pcty_desc, acc) = self#class_type_desc pcty_desc acc in
          let (pcty_loc, acc) = self#location pcty_loc acc in
          let (pcty_attributes, acc) = self#attributes pcty_attributes acc in
          (Class_type.of_concrete { pcty_desc; pcty_loc; pcty_attributes }, acc)
      method class_type_desc : Class_type_desc.t -> 'acc -> (Class_type_desc.t * 'acc)  =
        fun class_type_desc acc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#list self#core_type x1 acc in
            (Class_type_desc.of_concrete (Pcty_constr (x0, x1)), acc)
          | Pcty_signature x0 ->
            let (x0, acc) = self#class_signature x0 acc in
            (Class_type_desc.of_concrete (Pcty_signature x0), acc)
          | Pcty_arrow (x0, x1, x2) ->
            let (x0, acc) = self#arg_label x0 acc in
            let (x1, acc) = self#core_type x1 acc in
            let (x2, acc) = self#class_type x2 acc in
            (Class_type_desc.of_concrete (Pcty_arrow (x0, x1, x2)), acc)
          | Pcty_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Class_type_desc.of_concrete (Pcty_extension x0), acc)
          | Pcty_open (x0, x1, x2) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            let (x2, acc) = self#class_type x2 acc in
            (Class_type_desc.of_concrete (Pcty_open (x0, x1, x2)), acc)
      method class_signature : Class_signature.t -> 'acc -> (Class_signature.t * 'acc)  =
        fun class_signature acc ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let (pcsig_self, acc) = self#core_type pcsig_self acc in
          let (pcsig_fields, acc) = self#list self#class_type_field pcsig_fields acc in
          (Class_signature.of_concrete { pcsig_self; pcsig_fields }, acc)
      method class_type_field : Class_type_field.t -> 'acc -> (Class_type_field.t * 'acc)  =
        fun class_type_field acc ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let (pctf_desc, acc) = self#class_type_field_desc pctf_desc acc in
          let (pctf_loc, acc) = self#location pctf_loc acc in
          let (pctf_attributes, acc) = self#attributes pctf_attributes acc in
          (Class_type_field.of_concrete { pctf_desc; pctf_loc; pctf_attributes }, acc)
      method class_type_field_desc : Class_type_field_desc.t -> 'acc -> (Class_type_field_desc.t * 'acc)  =
        fun class_type_field_desc acc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let (x0, acc) = self#class_type x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_inherit x0), acc)
          | Pctf_val x0 ->
            let (x0, acc) = (fun (x0, x1, x2, x3) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#mutable_flag x1 acc in let (x2, acc) = self#virtual_flag x2 acc in let (x3, acc) = self#core_type x3 acc in ((x0, x1, x2, x3), acc)) x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_val x0), acc)
          | Pctf_method x0 ->
            let (x0, acc) = (fun (x0, x1, x2, x3) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#private_flag x1 acc in let (x2, acc) = self#virtual_flag x2 acc in let (x3, acc) = self#core_type x3 acc in ((x0, x1, x2, x3), acc)) x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_method x0), acc)
          | Pctf_constraint x0 ->
            let (x0, acc) = (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_constraint x0), acc)
          | Pctf_attribute x0 ->
            let (x0, acc) = self#attribute x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_attribute x0), acc)
          | Pctf_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Class_type_field_desc.of_concrete (Pctf_extension x0), acc)
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> (Class_expr.t Class_infos.t * 'acc)  =
        fun class_infos acc ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let (pci_virt, acc) = self#virtual_flag pci_virt acc in
          let (pci_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#variance x1 acc in ((x0, x1), acc)) pci_params acc in
          let (pci_name, acc) = self#loc self#string pci_name acc in
          let (pci_expr, acc) = self#class_expr pci_expr acc in
          let (pci_loc, acc) = self#location pci_loc acc in
          let (pci_attributes, acc) = self#attributes pci_attributes acc in
          (Class_infos.of_concrete_class_expr { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }, acc)
      method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> (Class_type.t Class_infos.t * 'acc)  =
        fun class_infos acc ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let (pci_virt, acc) = self#virtual_flag pci_virt acc in
          let (pci_params, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#variance x1 acc in ((x0, x1), acc)) pci_params acc in
          let (pci_name, acc) = self#loc self#string pci_name acc in
          let (pci_expr, acc) = self#class_type pci_expr acc in
          let (pci_loc, acc) = self#location pci_loc acc in
          let (pci_attributes, acc) = self#attributes pci_attributes acc in
          (Class_infos.of_concrete_class_type { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }, acc)
      method class_description : Class_description.t -> 'acc -> (Class_description.t * 'acc)  =
        fun class_description acc ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let (concrete, acc) = self#class_infos_class_type concrete acc in
          (Class_description.of_concrete concrete, acc)
      method class_type_declaration : Class_type_declaration.t -> 'acc -> (Class_type_declaration.t * 'acc)  =
        fun class_type_declaration acc ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let (concrete, acc) = self#class_infos_class_type concrete acc in
          (Class_type_declaration.of_concrete concrete, acc)
      method class_expr : Class_expr.t -> 'acc -> (Class_expr.t * 'acc)  =
        fun class_expr acc ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let (pcl_desc, acc) = self#class_expr_desc pcl_desc acc in
          let (pcl_loc, acc) = self#location pcl_loc acc in
          let (pcl_attributes, acc) = self#attributes pcl_attributes acc in
          (Class_expr.of_concrete { pcl_desc; pcl_loc; pcl_attributes }, acc)
      method class_expr_desc : Class_expr_desc.t -> 'acc -> (Class_expr_desc.t * 'acc)  =
        fun class_expr_desc acc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#list self#core_type x1 acc in
            (Class_expr_desc.of_concrete (Pcl_constr (x0, x1)), acc)
          | Pcl_structure x0 ->
            let (x0, acc) = self#class_structure x0 acc in
            (Class_expr_desc.of_concrete (Pcl_structure x0), acc)
          | Pcl_fun (x0, x1, x2, x3) ->
            let (x0, acc) = self#arg_label x0 acc in
            let (x1, acc) = self#option self#expression x1 acc in
            let (x2, acc) = self#pattern x2 acc in
            let (x3, acc) = self#class_expr x3 acc in
            (Class_expr_desc.of_concrete (Pcl_fun (x0, x1, x2, x3)), acc)
          | Pcl_apply (x0, x1) ->
            let (x0, acc) = self#class_expr x0 acc in
            let (x1, acc) = self#list (fun (x0, x1) acc -> let (x0, acc) = self#arg_label x0 acc in let (x1, acc) = self#expression x1 acc in ((x0, x1), acc)) x1 acc in
            (Class_expr_desc.of_concrete (Pcl_apply (x0, x1)), acc)
          | Pcl_let (x0, x1, x2) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#value_binding x1 acc in
            let (x2, acc) = self#class_expr x2 acc in
            (Class_expr_desc.of_concrete (Pcl_let (x0, x1, x2)), acc)
          | Pcl_constraint (x0, x1) ->
            let (x0, acc) = self#class_expr x0 acc in
            let (x1, acc) = self#class_type x1 acc in
            (Class_expr_desc.of_concrete (Pcl_constraint (x0, x1)), acc)
          | Pcl_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Class_expr_desc.of_concrete (Pcl_extension x0), acc)
          | Pcl_open (x0, x1, x2) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            let (x2, acc) = self#class_expr x2 acc in
            (Class_expr_desc.of_concrete (Pcl_open (x0, x1, x2)), acc)
      method class_structure : Class_structure.t -> 'acc -> (Class_structure.t * 'acc)  =
        fun class_structure acc ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let (pcstr_self, acc) = self#pattern pcstr_self acc in
          let (pcstr_fields, acc) = self#list self#class_field pcstr_fields acc in
          (Class_structure.of_concrete { pcstr_self; pcstr_fields }, acc)
      method class_field : Class_field.t -> 'acc -> (Class_field.t * 'acc)  =
        fun class_field acc ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let (pcf_desc, acc) = self#class_field_desc pcf_desc acc in
          let (pcf_loc, acc) = self#location pcf_loc acc in
          let (pcf_attributes, acc) = self#attributes pcf_attributes acc in
          (Class_field.of_concrete { pcf_desc; pcf_loc; pcf_attributes }, acc)
      method class_field_desc : Class_field_desc.t -> 'acc -> (Class_field_desc.t * 'acc)  =
        fun class_field_desc acc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#class_expr x1 acc in
            let (x2, acc) = self#option (self#loc self#string) x2 acc in
            (Class_field_desc.of_concrete (Pcf_inherit (x0, x1, x2)), acc)
          | Pcf_val x0 ->
            let (x0, acc) = (fun (x0, x1, x2) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#mutable_flag x1 acc in let (x2, acc) = self#class_field_kind x2 acc in ((x0, x1, x2), acc)) x0 acc in
            (Class_field_desc.of_concrete (Pcf_val x0), acc)
          | Pcf_method x0 ->
            let (x0, acc) = (fun (x0, x1, x2) acc -> let (x0, acc) = self#loc self#label x0 acc in let (x1, acc) = self#private_flag x1 acc in let (x2, acc) = self#class_field_kind x2 acc in ((x0, x1, x2), acc)) x0 acc in
            (Class_field_desc.of_concrete (Pcf_method x0), acc)
          | Pcf_constraint x0 ->
            let (x0, acc) = (fun (x0, x1) acc -> let (x0, acc) = self#core_type x0 acc in let (x1, acc) = self#core_type x1 acc in ((x0, x1), acc)) x0 acc in
            (Class_field_desc.of_concrete (Pcf_constraint x0), acc)
          | Pcf_initializer x0 ->
            let (x0, acc) = self#expression x0 acc in
            (Class_field_desc.of_concrete (Pcf_initializer x0), acc)
          | Pcf_attribute x0 ->
            let (x0, acc) = self#attribute x0 acc in
            (Class_field_desc.of_concrete (Pcf_attribute x0), acc)
          | Pcf_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Class_field_desc.of_concrete (Pcf_extension x0), acc)
      method class_field_kind : Class_field_kind.t -> 'acc -> (Class_field_kind.t * 'acc)  =
        fun class_field_kind acc ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let (x0, acc) = self#core_type x0 acc in
            (Class_field_kind.of_concrete (Cfk_virtual x0), acc)
          | Cfk_concrete (x0, x1) ->
            let (x0, acc) = self#override_flag x0 acc in
            let (x1, acc) = self#expression x1 acc in
            (Class_field_kind.of_concrete (Cfk_concrete (x0, x1)), acc)
      method class_declaration : Class_declaration.t -> 'acc -> (Class_declaration.t * 'acc)  =
        fun class_declaration acc ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let (concrete, acc) = self#class_infos_class_expr concrete acc in
          (Class_declaration.of_concrete concrete, acc)
      method module_type : Module_type.t -> 'acc -> (Module_type.t * 'acc)  =
        fun module_type acc ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let (pmty_desc, acc) = self#module_type_desc pmty_desc acc in
          let (pmty_loc, acc) = self#location pmty_loc acc in
          let (pmty_attributes, acc) = self#attributes pmty_attributes acc in
          (Module_type.of_concrete { pmty_desc; pmty_loc; pmty_attributes }, acc)
      method module_type_desc : Module_type_desc.t -> 'acc -> (Module_type_desc.t * 'acc)  =
        fun module_type_desc acc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Module_type_desc.of_concrete (Pmty_ident x0), acc)
          | Pmty_signature x0 ->
            let (x0, acc) = self#signature x0 acc in
            (Module_type_desc.of_concrete (Pmty_signature x0), acc)
          | Pmty_functor (x0, x1, x2) ->
            let (x0, acc) = self#loc self#string x0 acc in
            let (x1, acc) = self#option self#module_type x1 acc in
            let (x2, acc) = self#module_type x2 acc in
            (Module_type_desc.of_concrete (Pmty_functor (x0, x1, x2)), acc)
          | Pmty_with (x0, x1) ->
            let (x0, acc) = self#module_type x0 acc in
            let (x1, acc) = self#list self#with_constraint x1 acc in
            (Module_type_desc.of_concrete (Pmty_with (x0, x1)), acc)
          | Pmty_typeof x0 ->
            let (x0, acc) = self#module_expr x0 acc in
            (Module_type_desc.of_concrete (Pmty_typeof x0), acc)
          | Pmty_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Module_type_desc.of_concrete (Pmty_extension x0), acc)
          | Pmty_alias x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Module_type_desc.of_concrete (Pmty_alias x0), acc)
      method signature : Signature.t -> 'acc -> (Signature.t * 'acc)  =
        fun signature acc ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let (concrete, acc) = self#list self#signature_item concrete acc in
          (Signature.of_concrete concrete, acc)
      method signature_item : Signature_item.t -> 'acc -> (Signature_item.t * 'acc)  =
        fun signature_item acc ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let (psig_desc, acc) = self#signature_item_desc psig_desc acc in
          let (psig_loc, acc) = self#location psig_loc acc in
          (Signature_item.of_concrete { psig_desc; psig_loc }, acc)
      method signature_item_desc : Signature_item_desc.t -> 'acc -> (Signature_item_desc.t * 'acc)  =
        fun signature_item_desc acc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let (x0, acc) = self#value_description x0 acc in
            (Signature_item_desc.of_concrete (Psig_value x0), acc)
          | Psig_type (x0, x1) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#type_declaration x1 acc in
            (Signature_item_desc.of_concrete (Psig_type (x0, x1)), acc)
          | Psig_typext x0 ->
            let (x0, acc) = self#type_extension x0 acc in
            (Signature_item_desc.of_concrete (Psig_typext x0), acc)
          | Psig_exception x0 ->
            let (x0, acc) = self#extension_constructor x0 acc in
            (Signature_item_desc.of_concrete (Psig_exception x0), acc)
          | Psig_module x0 ->
            let (x0, acc) = self#module_declaration x0 acc in
            (Signature_item_desc.of_concrete (Psig_module x0), acc)
          | Psig_recmodule x0 ->
            let (x0, acc) = self#list self#module_declaration x0 acc in
            (Signature_item_desc.of_concrete (Psig_recmodule x0), acc)
          | Psig_modtype x0 ->
            let (x0, acc) = self#module_type_declaration x0 acc in
            (Signature_item_desc.of_concrete (Psig_modtype x0), acc)
          | Psig_open x0 ->
            let (x0, acc) = self#open_description x0 acc in
            (Signature_item_desc.of_concrete (Psig_open x0), acc)
          | Psig_include x0 ->
            let (x0, acc) = self#include_description x0 acc in
            (Signature_item_desc.of_concrete (Psig_include x0), acc)
          | Psig_class x0 ->
            let (x0, acc) = self#list self#class_description x0 acc in
            (Signature_item_desc.of_concrete (Psig_class x0), acc)
          | Psig_class_type x0 ->
            let (x0, acc) = self#list self#class_type_declaration x0 acc in
            (Signature_item_desc.of_concrete (Psig_class_type x0), acc)
          | Psig_attribute x0 ->
            let (x0, acc) = self#attribute x0 acc in
            (Signature_item_desc.of_concrete (Psig_attribute x0), acc)
          | Psig_extension (x0, x1) ->
            let (x0, acc) = self#extension x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            (Signature_item_desc.of_concrete (Psig_extension (x0, x1)), acc)
      method module_declaration : Module_declaration.t -> 'acc -> (Module_declaration.t * 'acc)  =
        fun module_declaration acc ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let (pmd_name, acc) = self#loc self#string pmd_name acc in
          let (pmd_type, acc) = self#module_type pmd_type acc in
          let (pmd_attributes, acc) = self#attributes pmd_attributes acc in
          let (pmd_loc, acc) = self#location pmd_loc acc in
          (Module_declaration.of_concrete { pmd_name; pmd_type; pmd_attributes; pmd_loc }, acc)
      method module_type_declaration : Module_type_declaration.t -> 'acc -> (Module_type_declaration.t * 'acc)  =
        fun module_type_declaration acc ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let (pmtd_name, acc) = self#loc self#string pmtd_name acc in
          let (pmtd_type, acc) = self#option self#module_type pmtd_type acc in
          let (pmtd_attributes, acc) = self#attributes pmtd_attributes acc in
          let (pmtd_loc, acc) = self#location pmtd_loc acc in
          (Module_type_declaration.of_concrete { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }, acc)
      method open_description : Open_description.t -> 'acc -> (Open_description.t * 'acc)  =
        fun open_description acc ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let (popen_lid, acc) = self#longident_loc popen_lid acc in
          let (popen_override, acc) = self#override_flag popen_override acc in
          let (popen_loc, acc) = self#location popen_loc acc in
          let (popen_attributes, acc) = self#attributes popen_attributes acc in
          (Open_description.of_concrete { popen_lid; popen_override; popen_loc; popen_attributes }, acc)
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> (Module_expr.t Include_infos.t * 'acc)  =
        fun include_infos acc ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let (pincl_mod, acc) = self#module_expr pincl_mod acc in
          let (pincl_loc, acc) = self#location pincl_loc acc in
          let (pincl_attributes, acc) = self#attributes pincl_attributes acc in
          (Include_infos.of_concrete_module_expr { pincl_mod; pincl_loc; pincl_attributes }, acc)
      method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> (Module_type.t Include_infos.t * 'acc)  =
        fun include_infos acc ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let (pincl_mod, acc) = self#module_type pincl_mod acc in
          let (pincl_loc, acc) = self#location pincl_loc acc in
          let (pincl_attributes, acc) = self#attributes pincl_attributes acc in
          (Include_infos.of_concrete_module_type { pincl_mod; pincl_loc; pincl_attributes }, acc)
      method include_description : Include_description.t -> 'acc -> (Include_description.t * 'acc)  =
        fun include_description acc ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let (concrete, acc) = self#include_infos_module_type concrete acc in
          (Include_description.of_concrete concrete, acc)
      method include_declaration : Include_declaration.t -> 'acc -> (Include_declaration.t * 'acc)  =
        fun include_declaration acc ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let (concrete, acc) = self#include_infos_module_expr concrete acc in
          (Include_declaration.of_concrete concrete, acc)
      method with_constraint : With_constraint.t -> 'acc -> (With_constraint.t * 'acc)  =
        fun with_constraint acc ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#type_declaration x1 acc in
            (With_constraint.of_concrete (Pwith_type (x0, x1)), acc)
          | Pwith_module (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            (With_constraint.of_concrete (Pwith_module (x0, x1)), acc)
          | Pwith_typesubst (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#type_declaration x1 acc in
            (With_constraint.of_concrete (Pwith_typesubst (x0, x1)), acc)
          | Pwith_modsubst (x0, x1) ->
            let (x0, acc) = self#longident_loc x0 acc in
            let (x1, acc) = self#longident_loc x1 acc in
            (With_constraint.of_concrete (Pwith_modsubst (x0, x1)), acc)
      method module_expr : Module_expr.t -> 'acc -> (Module_expr.t * 'acc)  =
        fun module_expr acc ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let (pmod_desc, acc) = self#module_expr_desc pmod_desc acc in
          let (pmod_loc, acc) = self#location pmod_loc acc in
          let (pmod_attributes, acc) = self#attributes pmod_attributes acc in
          (Module_expr.of_concrete { pmod_desc; pmod_loc; pmod_attributes }, acc)
      method module_expr_desc : Module_expr_desc.t -> 'acc -> (Module_expr_desc.t * 'acc)  =
        fun module_expr_desc acc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let (x0, acc) = self#longident_loc x0 acc in
            (Module_expr_desc.of_concrete (Pmod_ident x0), acc)
          | Pmod_structure x0 ->
            let (x0, acc) = self#structure x0 acc in
            (Module_expr_desc.of_concrete (Pmod_structure x0), acc)
          | Pmod_functor (x0, x1, x2) ->
            let (x0, acc) = self#loc self#string x0 acc in
            let (x1, acc) = self#option self#module_type x1 acc in
            let (x2, acc) = self#module_expr x2 acc in
            (Module_expr_desc.of_concrete (Pmod_functor (x0, x1, x2)), acc)
          | Pmod_apply (x0, x1) ->
            let (x0, acc) = self#module_expr x0 acc in
            let (x1, acc) = self#module_expr x1 acc in
            (Module_expr_desc.of_concrete (Pmod_apply (x0, x1)), acc)
          | Pmod_constraint (x0, x1) ->
            let (x0, acc) = self#module_expr x0 acc in
            let (x1, acc) = self#module_type x1 acc in
            (Module_expr_desc.of_concrete (Pmod_constraint (x0, x1)), acc)
          | Pmod_unpack x0 ->
            let (x0, acc) = self#expression x0 acc in
            (Module_expr_desc.of_concrete (Pmod_unpack x0), acc)
          | Pmod_extension x0 ->
            let (x0, acc) = self#extension x0 acc in
            (Module_expr_desc.of_concrete (Pmod_extension x0), acc)
      method structure : Structure.t -> 'acc -> (Structure.t * 'acc)  =
        fun structure acc ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let (concrete, acc) = self#list self#structure_item concrete acc in
          (Structure.of_concrete concrete, acc)
      method structure_item : Structure_item.t -> 'acc -> (Structure_item.t * 'acc)  =
        fun structure_item acc ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let (pstr_desc, acc) = self#structure_item_desc pstr_desc acc in
          let (pstr_loc, acc) = self#location pstr_loc acc in
          (Structure_item.of_concrete { pstr_desc; pstr_loc }, acc)
      method structure_item_desc : Structure_item_desc.t -> 'acc -> (Structure_item_desc.t * 'acc)  =
        fun structure_item_desc acc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let (x0, acc) = self#expression x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            (Structure_item_desc.of_concrete (Pstr_eval (x0, x1)), acc)
          | Pstr_value (x0, x1) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#value_binding x1 acc in
            (Structure_item_desc.of_concrete (Pstr_value (x0, x1)), acc)
          | Pstr_primitive x0 ->
            let (x0, acc) = self#value_description x0 acc in
            (Structure_item_desc.of_concrete (Pstr_primitive x0), acc)
          | Pstr_type (x0, x1) ->
            let (x0, acc) = self#rec_flag x0 acc in
            let (x1, acc) = self#list self#type_declaration x1 acc in
            (Structure_item_desc.of_concrete (Pstr_type (x0, x1)), acc)
          | Pstr_typext x0 ->
            let (x0, acc) = self#type_extension x0 acc in
            (Structure_item_desc.of_concrete (Pstr_typext x0), acc)
          | Pstr_exception x0 ->
            let (x0, acc) = self#extension_constructor x0 acc in
            (Structure_item_desc.of_concrete (Pstr_exception x0), acc)
          | Pstr_module x0 ->
            let (x0, acc) = self#module_binding x0 acc in
            (Structure_item_desc.of_concrete (Pstr_module x0), acc)
          | Pstr_recmodule x0 ->
            let (x0, acc) = self#list self#module_binding x0 acc in
            (Structure_item_desc.of_concrete (Pstr_recmodule x0), acc)
          | Pstr_modtype x0 ->
            let (x0, acc) = self#module_type_declaration x0 acc in
            (Structure_item_desc.of_concrete (Pstr_modtype x0), acc)
          | Pstr_open x0 ->
            let (x0, acc) = self#open_description x0 acc in
            (Structure_item_desc.of_concrete (Pstr_open x0), acc)
          | Pstr_class x0 ->
            let (x0, acc) = self#list self#class_declaration x0 acc in
            (Structure_item_desc.of_concrete (Pstr_class x0), acc)
          | Pstr_class_type x0 ->
            let (x0, acc) = self#list self#class_type_declaration x0 acc in
            (Structure_item_desc.of_concrete (Pstr_class_type x0), acc)
          | Pstr_include x0 ->
            let (x0, acc) = self#include_declaration x0 acc in
            (Structure_item_desc.of_concrete (Pstr_include x0), acc)
          | Pstr_attribute x0 ->
            let (x0, acc) = self#attribute x0 acc in
            (Structure_item_desc.of_concrete (Pstr_attribute x0), acc)
          | Pstr_extension (x0, x1) ->
            let (x0, acc) = self#extension x0 acc in
            let (x1, acc) = self#attributes x1 acc in
            (Structure_item_desc.of_concrete (Pstr_extension (x0, x1)), acc)
      method value_binding : Value_binding.t -> 'acc -> (Value_binding.t * 'acc)  =
        fun value_binding acc ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let (pvb_pat, acc) = self#pattern pvb_pat acc in
          let (pvb_expr, acc) = self#expression pvb_expr acc in
          let (pvb_attributes, acc) = self#attributes pvb_attributes acc in
          let (pvb_loc, acc) = self#location pvb_loc acc in
          (Value_binding.of_concrete { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }, acc)
      method module_binding : Module_binding.t -> 'acc -> (Module_binding.t * 'acc)  =
        fun module_binding acc ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let (pmb_name, acc) = self#loc self#string pmb_name acc in
          let (pmb_expr, acc) = self#module_expr pmb_expr acc in
          let (pmb_attributes, acc) = self#attributes pmb_attributes acc in
          let (pmb_loc, acc) = self#location pmb_loc acc in
          (Module_binding.of_concrete { pmb_name; pmb_expr; pmb_attributes; pmb_loc }, acc)
      method toplevel_phrase : Toplevel_phrase.t -> 'acc -> (Toplevel_phrase.t * 'acc)  =
        fun toplevel_phrase acc ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let (x0, acc) = self#structure x0 acc in
            (Toplevel_phrase.of_concrete (Ptop_def x0), acc)
          | Ptop_dir (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#directive_argument x1 acc in
            (Toplevel_phrase.of_concrete (Ptop_dir (x0, x1)), acc)
      method directive_argument : Directive_argument.t -> 'acc -> (Directive_argument.t * 'acc)  =
        fun directive_argument acc ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            (Directive_argument.of_concrete Pdir_none, acc)
          | Pdir_string x0 ->
            let (x0, acc) = self#string x0 acc in
            (Directive_argument.of_concrete (Pdir_string x0), acc)
          | Pdir_int (x0, x1) ->
            let (x0, acc) = self#string x0 acc in
            let (x1, acc) = self#option self#char x1 acc in
            (Directive_argument.of_concrete (Pdir_int (x0, x1)), acc)
          | Pdir_ident x0 ->
            let (x0, acc) = self#longident x0 acc in
            (Directive_argument.of_concrete (Pdir_ident x0), acc)
          | Pdir_bool x0 ->
            let (x0, acc) = self#bool x0 acc in
            (Directive_argument.of_concrete (Pdir_bool x0), acc)
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
      method longident : 'ctx -> Longident.t -> Longident.t  =
        fun _ctx longident ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let x0 = self#string _ctx x0 in
            Longident.of_concrete (Lident x0)
          | Ldot (x0, x1) ->
            let x0 = self#longident _ctx x0 in
            let x1 = self#string _ctx x1 in
            Longident.of_concrete (Ldot (x0, x1))
          | Lapply (x0, x1) ->
            let x0 = self#longident _ctx x0 in
            let x1 = self#longident _ctx x1 in
            Longident.of_concrete (Lapply (x0, x1))
      method longident_loc : 'ctx -> Longident_loc.t -> Longident_loc.t  =
        fun _ctx longident_loc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let concrete = self#loc self#longident _ctx concrete in
          Longident_loc.of_concrete concrete
      method rec_flag : 'ctx -> Rec_flag.t -> Rec_flag.t  =
        fun _ctx rec_flag ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            Rec_flag.of_concrete Nonrecursive
          | Recursive ->
            Rec_flag.of_concrete Recursive
      method direction_flag : 'ctx -> Direction_flag.t -> Direction_flag.t  =
        fun _ctx direction_flag ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            Direction_flag.of_concrete Upto
          | Downto ->
            Direction_flag.of_concrete Downto
      method private_flag : 'ctx -> Private_flag.t -> Private_flag.t  =
        fun _ctx private_flag ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            Private_flag.of_concrete Private
          | Public ->
            Private_flag.of_concrete Public
      method mutable_flag : 'ctx -> Mutable_flag.t -> Mutable_flag.t  =
        fun _ctx mutable_flag ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            Mutable_flag.of_concrete Immutable
          | Mutable ->
            Mutable_flag.of_concrete Mutable
      method virtual_flag : 'ctx -> Virtual_flag.t -> Virtual_flag.t  =
        fun _ctx virtual_flag ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            Virtual_flag.of_concrete Virtual
          | Concrete ->
            Virtual_flag.of_concrete Concrete
      method override_flag : 'ctx -> Override_flag.t -> Override_flag.t  =
        fun _ctx override_flag ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            Override_flag.of_concrete Override
          | Fresh ->
            Override_flag.of_concrete Fresh
      method closed_flag : 'ctx -> Closed_flag.t -> Closed_flag.t  =
        fun _ctx closed_flag ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            Closed_flag.of_concrete Closed
          | Open ->
            Closed_flag.of_concrete Open
      method label : 'ctx -> Label.t -> Label.t  =
        fun _ctx label ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let concrete = self#string _ctx concrete in
          Label.of_concrete concrete
      method arg_label : 'ctx -> Arg_label.t -> Arg_label.t  =
        fun _ctx arg_label ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            Arg_label.of_concrete Nolabel
          | Labelled x0 ->
            let x0 = self#string _ctx x0 in
            Arg_label.of_concrete (Labelled x0)
          | Optional x0 ->
            let x0 = self#string _ctx x0 in
            Arg_label.of_concrete (Optional x0)
      method variance : 'ctx -> Variance.t -> Variance.t  =
        fun _ctx variance ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            Variance.of_concrete Covariant
          | Contravariant ->
            Variance.of_concrete Contravariant
          | Invariant ->
            Variance.of_concrete Invariant
      method constant : 'ctx -> Constant.t -> Constant.t  =
        fun _ctx constant ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#option self#char _ctx x1 in
            Constant.of_concrete (Pconst_integer (x0, x1))
          | Pconst_char x0 ->
            let x0 = self#char _ctx x0 in
            Constant.of_concrete (Pconst_char x0)
          | Pconst_string (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#option self#string _ctx x1 in
            Constant.of_concrete (Pconst_string (x0, x1))
          | Pconst_float (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#option self#char _ctx x1 in
            Constant.of_concrete (Pconst_float (x0, x1))
      method attribute : 'ctx -> Attribute.t -> Attribute.t  =
        fun _ctx attribute ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string _ctx x0 in
          let x1 = self#payload _ctx x1 in
          Attribute.of_concrete (x0, x1)
      method extension : 'ctx -> Extension.t -> Extension.t  =
        fun _ctx extension ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string _ctx x0 in
          let x1 = self#payload _ctx x1 in
          Extension.of_concrete (x0, x1)
      method attributes : 'ctx -> Attributes.t -> Attributes.t  =
        fun _ctx attributes ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let concrete = self#list self#attribute _ctx concrete in
          Attributes.of_concrete concrete
      method payload : 'ctx -> Payload.t -> Payload.t  =
        fun _ctx payload ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let x0 = self#structure _ctx x0 in
            Payload.of_concrete (PStr x0)
          | PSig x0 ->
            let x0 = self#signature _ctx x0 in
            Payload.of_concrete (PSig x0)
          | PTyp x0 ->
            let x0 = self#core_type _ctx x0 in
            Payload.of_concrete (PTyp x0)
          | PPat (x0, x1) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            Payload.of_concrete (PPat (x0, x1))
      method core_type : 'ctx -> Core_type.t -> Core_type.t  =
        fun _ctx core_type ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let ptyp_desc = self#core_type_desc _ctx ptyp_desc in
          let ptyp_loc = self#location _ctx ptyp_loc in
          let ptyp_attributes = self#attributes _ctx ptyp_attributes in
          Core_type.of_concrete { ptyp_desc; ptyp_loc; ptyp_attributes }
      method core_type_desc : 'ctx -> Core_type_desc.t -> Core_type_desc.t  =
        fun _ctx core_type_desc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            Core_type_desc.of_concrete Ptyp_any
          | Ptyp_var x0 ->
            let x0 = self#string _ctx x0 in
            Core_type_desc.of_concrete (Ptyp_var x0)
          | Ptyp_arrow (x0, x1, x2) ->
            let x0 = self#arg_label _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            let x2 = self#core_type _ctx x2 in
            Core_type_desc.of_concrete (Ptyp_arrow (x0, x1, x2))
          | Ptyp_tuple x0 ->
            let x0 = self#list self#core_type _ctx x0 in
            Core_type_desc.of_concrete (Ptyp_tuple x0)
          | Ptyp_constr (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#list self#core_type _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_constr (x0, x1))
          | Ptyp_object (x0, x1) ->
            let x0 = self#list self#object_field _ctx x0 in
            let x1 = self#closed_flag _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_object (x0, x1))
          | Ptyp_class (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#list self#core_type _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_class (x0, x1))
          | Ptyp_alias (x0, x1) ->
            let x0 = self#core_type _ctx x0 in
            let x1 = self#string _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_alias (x0, x1))
          | Ptyp_variant (x0, x1, x2) ->
            let x0 = self#list self#row_field _ctx x0 in
            let x1 = self#closed_flag _ctx x1 in
            let x2 = self#option (self#list self#label) _ctx x2 in
            Core_type_desc.of_concrete (Ptyp_variant (x0, x1, x2))
          | Ptyp_poly (x0, x1) ->
            let x0 = self#list (self#loc self#string) _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            Core_type_desc.of_concrete (Ptyp_poly (x0, x1))
          | Ptyp_package x0 ->
            let x0 = self#package_type _ctx x0 in
            Core_type_desc.of_concrete (Ptyp_package x0)
          | Ptyp_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Core_type_desc.of_concrete (Ptyp_extension x0)
      method package_type : 'ctx -> Package_type.t -> Package_type.t  =
        fun _ctx package_type ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#longident_loc _ctx x0 in
          let x1 = self#list (fun _ctx (x0, x1) -> let x0 = self#longident_loc _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx x1 in
          Package_type.of_concrete (x0, x1)
      method row_field : 'ctx -> Row_field.t -> Row_field.t  =
        fun _ctx row_field ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let x0 = self#loc self#label _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            let x2 = self#bool _ctx x2 in
            let x3 = self#list self#core_type _ctx x3 in
            Row_field.of_concrete (Rtag (x0, x1, x2, x3))
          | Rinherit x0 ->
            let x0 = self#core_type _ctx x0 in
            Row_field.of_concrete (Rinherit x0)
      method object_field : 'ctx -> Object_field.t -> Object_field.t  =
        fun _ctx object_field ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let x0 = self#loc self#label _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            let x2 = self#core_type _ctx x2 in
            Object_field.of_concrete (Otag (x0, x1, x2))
          | Oinherit x0 ->
            let x0 = self#core_type _ctx x0 in
            Object_field.of_concrete (Oinherit x0)
      method pattern : 'ctx -> Pattern.t -> Pattern.t  =
        fun _ctx pattern ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let ppat_desc = self#pattern_desc _ctx ppat_desc in
          let ppat_loc = self#location _ctx ppat_loc in
          let ppat_attributes = self#attributes _ctx ppat_attributes in
          Pattern.of_concrete { ppat_desc; ppat_loc; ppat_attributes }
      method pattern_desc : 'ctx -> Pattern_desc.t -> Pattern_desc.t  =
        fun _ctx pattern_desc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            Pattern_desc.of_concrete Ppat_any
          | Ppat_var x0 ->
            let x0 = self#loc self#string _ctx x0 in
            Pattern_desc.of_concrete (Ppat_var x0)
          | Ppat_alias (x0, x1) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#loc self#string _ctx x1 in
            Pattern_desc.of_concrete (Ppat_alias (x0, x1))
          | Ppat_constant x0 ->
            let x0 = self#constant _ctx x0 in
            Pattern_desc.of_concrete (Ppat_constant x0)
          | Ppat_interval (x0, x1) ->
            let x0 = self#constant _ctx x0 in
            let x1 = self#constant _ctx x1 in
            Pattern_desc.of_concrete (Ppat_interval (x0, x1))
          | Ppat_tuple x0 ->
            let x0 = self#list self#pattern _ctx x0 in
            Pattern_desc.of_concrete (Ppat_tuple x0)
          | Ppat_construct (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#option self#pattern _ctx x1 in
            Pattern_desc.of_concrete (Ppat_construct (x0, x1))
          | Ppat_variant (x0, x1) ->
            let x0 = self#label _ctx x0 in
            let x1 = self#option self#pattern _ctx x1 in
            Pattern_desc.of_concrete (Ppat_variant (x0, x1))
          | Ppat_record (x0, x1) ->
            let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#longident_loc _ctx x0 in let x1 = self#pattern _ctx x1 in (x0, x1)) _ctx x0 in
            let x1 = self#closed_flag _ctx x1 in
            Pattern_desc.of_concrete (Ppat_record (x0, x1))
          | Ppat_array x0 ->
            let x0 = self#list self#pattern _ctx x0 in
            Pattern_desc.of_concrete (Ppat_array x0)
          | Ppat_or (x0, x1) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#pattern _ctx x1 in
            Pattern_desc.of_concrete (Ppat_or (x0, x1))
          | Ppat_constraint (x0, x1) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            Pattern_desc.of_concrete (Ppat_constraint (x0, x1))
          | Ppat_type x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Pattern_desc.of_concrete (Ppat_type x0)
          | Ppat_lazy x0 ->
            let x0 = self#pattern _ctx x0 in
            Pattern_desc.of_concrete (Ppat_lazy x0)
          | Ppat_unpack x0 ->
            let x0 = self#loc self#string _ctx x0 in
            Pattern_desc.of_concrete (Ppat_unpack x0)
          | Ppat_exception x0 ->
            let x0 = self#pattern _ctx x0 in
            Pattern_desc.of_concrete (Ppat_exception x0)
          | Ppat_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Pattern_desc.of_concrete (Ppat_extension x0)
          | Ppat_open (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#pattern _ctx x1 in
            Pattern_desc.of_concrete (Ppat_open (x0, x1))
      method expression : 'ctx -> Expression.t -> Expression.t  =
        fun _ctx expression ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let pexp_desc = self#expression_desc _ctx pexp_desc in
          let pexp_loc = self#location _ctx pexp_loc in
          let pexp_attributes = self#attributes _ctx pexp_attributes in
          Expression.of_concrete { pexp_desc; pexp_loc; pexp_attributes }
      method expression_desc : 'ctx -> Expression_desc.t -> Expression_desc.t  =
        fun _ctx expression_desc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Expression_desc.of_concrete (Pexp_ident x0)
          | Pexp_constant x0 ->
            let x0 = self#constant _ctx x0 in
            Expression_desc.of_concrete (Pexp_constant x0)
          | Pexp_let (x0, x1, x2) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#value_binding _ctx x1 in
            let x2 = self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_let (x0, x1, x2))
          | Pexp_function x0 ->
            let x0 = self#list self#case _ctx x0 in
            Expression_desc.of_concrete (Pexp_function x0)
          | Pexp_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            let x2 = self#pattern _ctx x2 in
            let x3 = self#expression _ctx x3 in
            Expression_desc.of_concrete (Pexp_fun (x0, x1, x2, x3))
          | Pexp_apply (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#list (fun _ctx (x0, x1) -> let x0 = self#arg_label _ctx x0 in let x1 = self#expression _ctx x1 in (x0, x1)) _ctx x1 in
            Expression_desc.of_concrete (Pexp_apply (x0, x1))
          | Pexp_match (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#list self#case _ctx x1 in
            Expression_desc.of_concrete (Pexp_match (x0, x1))
          | Pexp_try (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#list self#case _ctx x1 in
            Expression_desc.of_concrete (Pexp_try (x0, x1))
          | Pexp_tuple x0 ->
            let x0 = self#list self#expression _ctx x0 in
            Expression_desc.of_concrete (Pexp_tuple x0)
          | Pexp_construct (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_construct (x0, x1))
          | Pexp_variant (x0, x1) ->
            let x0 = self#label _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_variant (x0, x1))
          | Pexp_record (x0, x1) ->
            let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#longident_loc _ctx x0 in let x1 = self#expression _ctx x1 in (x0, x1)) _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_record (x0, x1))
          | Pexp_field (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            Expression_desc.of_concrete (Pexp_field (x0, x1))
          | Pexp_setfield (x0, x1, x2) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            let x2 = self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_setfield (x0, x1, x2))
          | Pexp_array x0 ->
            let x0 = self#list self#expression _ctx x0 in
            Expression_desc.of_concrete (Pexp_array x0)
          | Pexp_ifthenelse (x0, x1, x2) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#expression _ctx x1 in
            let x2 = self#option self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_ifthenelse (x0, x1, x2))
          | Pexp_sequence (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_sequence (x0, x1))
          | Pexp_while (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_while (x0, x1))
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let x0 = self#pattern _ctx x0 in
            let x1 = self#expression _ctx x1 in
            let x2 = self#expression _ctx x2 in
            let x3 = self#direction_flag _ctx x3 in
            let x4 = self#expression _ctx x4 in
            Expression_desc.of_concrete (Pexp_for (x0, x1, x2, x3, x4))
          | Pexp_constraint (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            Expression_desc.of_concrete (Pexp_constraint (x0, x1))
          | Pexp_coerce (x0, x1, x2) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#option self#core_type _ctx x1 in
            let x2 = self#core_type _ctx x2 in
            Expression_desc.of_concrete (Pexp_coerce (x0, x1, x2))
          | Pexp_send (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#loc self#label _ctx x1 in
            Expression_desc.of_concrete (Pexp_send (x0, x1))
          | Pexp_new x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Expression_desc.of_concrete (Pexp_new x0)
          | Pexp_setinstvar (x0, x1) ->
            let x0 = self#loc self#label _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_setinstvar (x0, x1))
          | Pexp_override x0 ->
            let x0 = self#list (fun _ctx (x0, x1) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#expression _ctx x1 in (x0, x1)) _ctx x0 in
            Expression_desc.of_concrete (Pexp_override x0)
          | Pexp_letmodule (x0, x1, x2) ->
            let x0 = self#loc self#string _ctx x0 in
            let x1 = self#module_expr _ctx x1 in
            let x2 = self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_letmodule (x0, x1, x2))
          | Pexp_letexception (x0, x1) ->
            let x0 = self#extension_constructor _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_letexception (x0, x1))
          | Pexp_assert x0 ->
            let x0 = self#expression _ctx x0 in
            Expression_desc.of_concrete (Pexp_assert x0)
          | Pexp_lazy x0 ->
            let x0 = self#expression _ctx x0 in
            Expression_desc.of_concrete (Pexp_lazy x0)
          | Pexp_poly (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#option self#core_type _ctx x1 in
            Expression_desc.of_concrete (Pexp_poly (x0, x1))
          | Pexp_object x0 ->
            let x0 = self#class_structure _ctx x0 in
            Expression_desc.of_concrete (Pexp_object x0)
          | Pexp_newtype (x0, x1) ->
            let x0 = self#loc self#string _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Expression_desc.of_concrete (Pexp_newtype (x0, x1))
          | Pexp_pack x0 ->
            let x0 = self#module_expr _ctx x0 in
            Expression_desc.of_concrete (Pexp_pack x0)
          | Pexp_open (x0, x1, x2) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            let x2 = self#expression _ctx x2 in
            Expression_desc.of_concrete (Pexp_open (x0, x1, x2))
          | Pexp_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Expression_desc.of_concrete (Pexp_extension x0)
          | Pexp_unreachable ->
            Expression_desc.of_concrete Pexp_unreachable
      method case : 'ctx -> Case.t -> Case.t  =
        fun _ctx case ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let pc_lhs = self#pattern _ctx pc_lhs in
          let pc_guard = self#option self#expression _ctx pc_guard in
          let pc_rhs = self#expression _ctx pc_rhs in
          Case.of_concrete { pc_lhs; pc_guard; pc_rhs }
      method value_description : 'ctx -> Value_description.t -> Value_description.t  =
        fun _ctx value_description ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let pval_name = self#loc self#string _ctx pval_name in
          let pval_type = self#core_type _ctx pval_type in
          let pval_prim = self#list self#string _ctx pval_prim in
          let pval_attributes = self#attributes _ctx pval_attributes in
          let pval_loc = self#location _ctx pval_loc in
          Value_description.of_concrete { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
      method type_declaration : 'ctx -> Type_declaration.t -> Type_declaration.t  =
        fun _ctx type_declaration ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let ptype_name = self#loc self#string _ctx ptype_name in
          let ptype_params = self#list (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#variance _ctx x1 in (x0, x1)) _ctx ptype_params in
          let ptype_cstrs = self#list (fun _ctx (x0, x1, x2) -> let x0 = self#core_type _ctx x0 in let x1 = self#core_type _ctx x1 in let x2 = self#location _ctx x2 in (x0, x1, x2)) _ctx ptype_cstrs in
          let ptype_kind = self#type_kind _ctx ptype_kind in
          let ptype_private = self#private_flag _ctx ptype_private in
          let ptype_manifest = self#option self#core_type _ctx ptype_manifest in
          let ptype_attributes = self#attributes _ctx ptype_attributes in
          let ptype_loc = self#location _ctx ptype_loc in
          Type_declaration.of_concrete { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }
      method type_kind : 'ctx -> Type_kind.t -> Type_kind.t  =
        fun _ctx type_kind ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            Type_kind.of_concrete Ptype_abstract
          | Ptype_variant x0 ->
            let x0 = self#list self#constructor_declaration _ctx x0 in
            Type_kind.of_concrete (Ptype_variant x0)
          | Ptype_record x0 ->
            let x0 = self#list self#label_declaration _ctx x0 in
            Type_kind.of_concrete (Ptype_record x0)
          | Ptype_open ->
            Type_kind.of_concrete Ptype_open
      method label_declaration : 'ctx -> Label_declaration.t -> Label_declaration.t  =
        fun _ctx label_declaration ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let pld_name = self#loc self#string _ctx pld_name in
          let pld_mutable = self#mutable_flag _ctx pld_mutable in
          let pld_type = self#core_type _ctx pld_type in
          let pld_loc = self#location _ctx pld_loc in
          let pld_attributes = self#attributes _ctx pld_attributes in
          Label_declaration.of_concrete { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
      method constructor_declaration : 'ctx -> Constructor_declaration.t -> Constructor_declaration.t  =
        fun _ctx constructor_declaration ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let pcd_name = self#loc self#string _ctx pcd_name in
          let pcd_args = self#constructor_arguments _ctx pcd_args in
          let pcd_res = self#option self#core_type _ctx pcd_res in
          let pcd_loc = self#location _ctx pcd_loc in
          let pcd_attributes = self#attributes _ctx pcd_attributes in
          Constructor_declaration.of_concrete { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
      method constructor_arguments : 'ctx -> Constructor_arguments.t -> Constructor_arguments.t  =
        fun _ctx constructor_arguments ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let x0 = self#list self#core_type _ctx x0 in
            Constructor_arguments.of_concrete (Pcstr_tuple x0)
          | Pcstr_record x0 ->
            let x0 = self#list self#label_declaration _ctx x0 in
            Constructor_arguments.of_concrete (Pcstr_record x0)
      method type_extension : 'ctx -> Type_extension.t -> Type_extension.t  =
        fun _ctx type_extension ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let ptyext_path = self#longident_loc _ctx ptyext_path in
          let ptyext_params = self#list (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#variance _ctx x1 in (x0, x1)) _ctx ptyext_params in
          let ptyext_constructors = self#list self#extension_constructor _ctx ptyext_constructors in
          let ptyext_private = self#private_flag _ctx ptyext_private in
          let ptyext_attributes = self#attributes _ctx ptyext_attributes in
          Type_extension.of_concrete { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }
      method extension_constructor : 'ctx -> Extension_constructor.t -> Extension_constructor.t  =
        fun _ctx extension_constructor ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let pext_name = self#loc self#string _ctx pext_name in
          let pext_kind = self#extension_constructor_kind _ctx pext_kind in
          let pext_loc = self#location _ctx pext_loc in
          let pext_attributes = self#attributes _ctx pext_attributes in
          Extension_constructor.of_concrete { pext_name; pext_kind; pext_loc; pext_attributes }
      method extension_constructor_kind : 'ctx -> Extension_constructor_kind.t -> Extension_constructor_kind.t  =
        fun _ctx extension_constructor_kind ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let x0 = self#constructor_arguments _ctx x0 in
            let x1 = self#option self#core_type _ctx x1 in
            Extension_constructor_kind.of_concrete (Pext_decl (x0, x1))
          | Pext_rebind x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Extension_constructor_kind.of_concrete (Pext_rebind x0)
      method class_type : 'ctx -> Class_type.t -> Class_type.t  =
        fun _ctx class_type ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let pcty_desc = self#class_type_desc _ctx pcty_desc in
          let pcty_loc = self#location _ctx pcty_loc in
          let pcty_attributes = self#attributes _ctx pcty_attributes in
          Class_type.of_concrete { pcty_desc; pcty_loc; pcty_attributes }
      method class_type_desc : 'ctx -> Class_type_desc.t -> Class_type_desc.t  =
        fun _ctx class_type_desc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#list self#core_type _ctx x1 in
            Class_type_desc.of_concrete (Pcty_constr (x0, x1))
          | Pcty_signature x0 ->
            let x0 = self#class_signature _ctx x0 in
            Class_type_desc.of_concrete (Pcty_signature x0)
          | Pcty_arrow (x0, x1, x2) ->
            let x0 = self#arg_label _ctx x0 in
            let x1 = self#core_type _ctx x1 in
            let x2 = self#class_type _ctx x2 in
            Class_type_desc.of_concrete (Pcty_arrow (x0, x1, x2))
          | Pcty_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Class_type_desc.of_concrete (Pcty_extension x0)
          | Pcty_open (x0, x1, x2) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            let x2 = self#class_type _ctx x2 in
            Class_type_desc.of_concrete (Pcty_open (x0, x1, x2))
      method class_signature : 'ctx -> Class_signature.t -> Class_signature.t  =
        fun _ctx class_signature ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let pcsig_self = self#core_type _ctx pcsig_self in
          let pcsig_fields = self#list self#class_type_field _ctx pcsig_fields in
          Class_signature.of_concrete { pcsig_self; pcsig_fields }
      method class_type_field : 'ctx -> Class_type_field.t -> Class_type_field.t  =
        fun _ctx class_type_field ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let pctf_desc = self#class_type_field_desc _ctx pctf_desc in
          let pctf_loc = self#location _ctx pctf_loc in
          let pctf_attributes = self#attributes _ctx pctf_attributes in
          Class_type_field.of_concrete { pctf_desc; pctf_loc; pctf_attributes }
      method class_type_field_desc : 'ctx -> Class_type_field_desc.t -> Class_type_field_desc.t  =
        fun _ctx class_type_field_desc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let x0 = self#class_type _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_inherit x0)
          | Pctf_val x0 ->
            let x0 = (fun _ctx (x0, x1, x2, x3) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#mutable_flag _ctx x1 in let x2 = self#virtual_flag _ctx x2 in let x3 = self#core_type _ctx x3 in (x0, x1, x2, x3)) _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_val x0)
          | Pctf_method x0 ->
            let x0 = (fun _ctx (x0, x1, x2, x3) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#private_flag _ctx x1 in let x2 = self#virtual_flag _ctx x2 in let x3 = self#core_type _ctx x3 in (x0, x1, x2, x3)) _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_method x0)
          | Pctf_constraint x0 ->
            let x0 = (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_constraint x0)
          | Pctf_attribute x0 ->
            let x0 = self#attribute _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_attribute x0)
          | Pctf_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Class_type_field_desc.of_concrete (Pctf_extension x0)
      method class_infos_class_expr : 'ctx -> Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t  =
        fun _ctx class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag _ctx pci_virt in
          let pci_params = self#list (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#variance _ctx x1 in (x0, x1)) _ctx pci_params in
          let pci_name = self#loc self#string _ctx pci_name in
          let pci_expr = self#class_expr _ctx pci_expr in
          let pci_loc = self#location _ctx pci_loc in
          let pci_attributes = self#attributes _ctx pci_attributes in
          Class_infos.of_concrete_class_expr { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
      method class_infos_class_type : 'ctx -> Class_type.t Class_infos.t -> Class_type.t Class_infos.t  =
        fun _ctx class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag _ctx pci_virt in
          let pci_params = self#list (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#variance _ctx x1 in (x0, x1)) _ctx pci_params in
          let pci_name = self#loc self#string _ctx pci_name in
          let pci_expr = self#class_type _ctx pci_expr in
          let pci_loc = self#location _ctx pci_loc in
          let pci_attributes = self#attributes _ctx pci_attributes in
          Class_infos.of_concrete_class_type { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
      method class_description : 'ctx -> Class_description.t -> Class_description.t  =
        fun _ctx class_description ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type _ctx concrete in
          Class_description.of_concrete concrete
      method class_type_declaration : 'ctx -> Class_type_declaration.t -> Class_type_declaration.t  =
        fun _ctx class_type_declaration ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type _ctx concrete in
          Class_type_declaration.of_concrete concrete
      method class_expr : 'ctx -> Class_expr.t -> Class_expr.t  =
        fun _ctx class_expr ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let pcl_desc = self#class_expr_desc _ctx pcl_desc in
          let pcl_loc = self#location _ctx pcl_loc in
          let pcl_attributes = self#attributes _ctx pcl_attributes in
          Class_expr.of_concrete { pcl_desc; pcl_loc; pcl_attributes }
      method class_expr_desc : 'ctx -> Class_expr_desc.t -> Class_expr_desc.t  =
        fun _ctx class_expr_desc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#list self#core_type _ctx x1 in
            Class_expr_desc.of_concrete (Pcl_constr (x0, x1))
          | Pcl_structure x0 ->
            let x0 = self#class_structure _ctx x0 in
            Class_expr_desc.of_concrete (Pcl_structure x0)
          | Pcl_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label _ctx x0 in
            let x1 = self#option self#expression _ctx x1 in
            let x2 = self#pattern _ctx x2 in
            let x3 = self#class_expr _ctx x3 in
            Class_expr_desc.of_concrete (Pcl_fun (x0, x1, x2, x3))
          | Pcl_apply (x0, x1) ->
            let x0 = self#class_expr _ctx x0 in
            let x1 = self#list (fun _ctx (x0, x1) -> let x0 = self#arg_label _ctx x0 in let x1 = self#expression _ctx x1 in (x0, x1)) _ctx x1 in
            Class_expr_desc.of_concrete (Pcl_apply (x0, x1))
          | Pcl_let (x0, x1, x2) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#value_binding _ctx x1 in
            let x2 = self#class_expr _ctx x2 in
            Class_expr_desc.of_concrete (Pcl_let (x0, x1, x2))
          | Pcl_constraint (x0, x1) ->
            let x0 = self#class_expr _ctx x0 in
            let x1 = self#class_type _ctx x1 in
            Class_expr_desc.of_concrete (Pcl_constraint (x0, x1))
          | Pcl_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Class_expr_desc.of_concrete (Pcl_extension x0)
          | Pcl_open (x0, x1, x2) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            let x2 = self#class_expr _ctx x2 in
            Class_expr_desc.of_concrete (Pcl_open (x0, x1, x2))
      method class_structure : 'ctx -> Class_structure.t -> Class_structure.t  =
        fun _ctx class_structure ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let pcstr_self = self#pattern _ctx pcstr_self in
          let pcstr_fields = self#list self#class_field _ctx pcstr_fields in
          Class_structure.of_concrete { pcstr_self; pcstr_fields }
      method class_field : 'ctx -> Class_field.t -> Class_field.t  =
        fun _ctx class_field ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let pcf_desc = self#class_field_desc _ctx pcf_desc in
          let pcf_loc = self#location _ctx pcf_loc in
          let pcf_attributes = self#attributes _ctx pcf_attributes in
          Class_field.of_concrete { pcf_desc; pcf_loc; pcf_attributes }
      method class_field_desc : 'ctx -> Class_field_desc.t -> Class_field_desc.t  =
        fun _ctx class_field_desc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#class_expr _ctx x1 in
            let x2 = self#option (self#loc self#string) _ctx x2 in
            Class_field_desc.of_concrete (Pcf_inherit (x0, x1, x2))
          | Pcf_val x0 ->
            let x0 = (fun _ctx (x0, x1, x2) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#mutable_flag _ctx x1 in let x2 = self#class_field_kind _ctx x2 in (x0, x1, x2)) _ctx x0 in
            Class_field_desc.of_concrete (Pcf_val x0)
          | Pcf_method x0 ->
            let x0 = (fun _ctx (x0, x1, x2) -> let x0 = self#loc self#label _ctx x0 in let x1 = self#private_flag _ctx x1 in let x2 = self#class_field_kind _ctx x2 in (x0, x1, x2)) _ctx x0 in
            Class_field_desc.of_concrete (Pcf_method x0)
          | Pcf_constraint x0 ->
            let x0 = (fun _ctx (x0, x1) -> let x0 = self#core_type _ctx x0 in let x1 = self#core_type _ctx x1 in (x0, x1)) _ctx x0 in
            Class_field_desc.of_concrete (Pcf_constraint x0)
          | Pcf_initializer x0 ->
            let x0 = self#expression _ctx x0 in
            Class_field_desc.of_concrete (Pcf_initializer x0)
          | Pcf_attribute x0 ->
            let x0 = self#attribute _ctx x0 in
            Class_field_desc.of_concrete (Pcf_attribute x0)
          | Pcf_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Class_field_desc.of_concrete (Pcf_extension x0)
      method class_field_kind : 'ctx -> Class_field_kind.t -> Class_field_kind.t  =
        fun _ctx class_field_kind ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let x0 = self#core_type _ctx x0 in
            Class_field_kind.of_concrete (Cfk_virtual x0)
          | Cfk_concrete (x0, x1) ->
            let x0 = self#override_flag _ctx x0 in
            let x1 = self#expression _ctx x1 in
            Class_field_kind.of_concrete (Cfk_concrete (x0, x1))
      method class_declaration : 'ctx -> Class_declaration.t -> Class_declaration.t  =
        fun _ctx class_declaration ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_expr _ctx concrete in
          Class_declaration.of_concrete concrete
      method module_type : 'ctx -> Module_type.t -> Module_type.t  =
        fun _ctx module_type ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let pmty_desc = self#module_type_desc _ctx pmty_desc in
          let pmty_loc = self#location _ctx pmty_loc in
          let pmty_attributes = self#attributes _ctx pmty_attributes in
          Module_type.of_concrete { pmty_desc; pmty_loc; pmty_attributes }
      method module_type_desc : 'ctx -> Module_type_desc.t -> Module_type_desc.t  =
        fun _ctx module_type_desc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Module_type_desc.of_concrete (Pmty_ident x0)
          | Pmty_signature x0 ->
            let x0 = self#signature _ctx x0 in
            Module_type_desc.of_concrete (Pmty_signature x0)
          | Pmty_functor (x0, x1, x2) ->
            let x0 = self#loc self#string _ctx x0 in
            let x1 = self#option self#module_type _ctx x1 in
            let x2 = self#module_type _ctx x2 in
            Module_type_desc.of_concrete (Pmty_functor (x0, x1, x2))
          | Pmty_with (x0, x1) ->
            let x0 = self#module_type _ctx x0 in
            let x1 = self#list self#with_constraint _ctx x1 in
            Module_type_desc.of_concrete (Pmty_with (x0, x1))
          | Pmty_typeof x0 ->
            let x0 = self#module_expr _ctx x0 in
            Module_type_desc.of_concrete (Pmty_typeof x0)
          | Pmty_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Module_type_desc.of_concrete (Pmty_extension x0)
          | Pmty_alias x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Module_type_desc.of_concrete (Pmty_alias x0)
      method signature : 'ctx -> Signature.t -> Signature.t  =
        fun _ctx signature ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let concrete = self#list self#signature_item _ctx concrete in
          Signature.of_concrete concrete
      method signature_item : 'ctx -> Signature_item.t -> Signature_item.t  =
        fun _ctx signature_item ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let psig_desc = self#signature_item_desc _ctx psig_desc in
          let psig_loc = self#location _ctx psig_loc in
          Signature_item.of_concrete { psig_desc; psig_loc }
      method signature_item_desc : 'ctx -> Signature_item_desc.t -> Signature_item_desc.t  =
        fun _ctx signature_item_desc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let x0 = self#value_description _ctx x0 in
            Signature_item_desc.of_concrete (Psig_value x0)
          | Psig_type (x0, x1) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#type_declaration _ctx x1 in
            Signature_item_desc.of_concrete (Psig_type (x0, x1))
          | Psig_typext x0 ->
            let x0 = self#type_extension _ctx x0 in
            Signature_item_desc.of_concrete (Psig_typext x0)
          | Psig_exception x0 ->
            let x0 = self#extension_constructor _ctx x0 in
            Signature_item_desc.of_concrete (Psig_exception x0)
          | Psig_module x0 ->
            let x0 = self#module_declaration _ctx x0 in
            Signature_item_desc.of_concrete (Psig_module x0)
          | Psig_recmodule x0 ->
            let x0 = self#list self#module_declaration _ctx x0 in
            Signature_item_desc.of_concrete (Psig_recmodule x0)
          | Psig_modtype x0 ->
            let x0 = self#module_type_declaration _ctx x0 in
            Signature_item_desc.of_concrete (Psig_modtype x0)
          | Psig_open x0 ->
            let x0 = self#open_description _ctx x0 in
            Signature_item_desc.of_concrete (Psig_open x0)
          | Psig_include x0 ->
            let x0 = self#include_description _ctx x0 in
            Signature_item_desc.of_concrete (Psig_include x0)
          | Psig_class x0 ->
            let x0 = self#list self#class_description _ctx x0 in
            Signature_item_desc.of_concrete (Psig_class x0)
          | Psig_class_type x0 ->
            let x0 = self#list self#class_type_declaration _ctx x0 in
            Signature_item_desc.of_concrete (Psig_class_type x0)
          | Psig_attribute x0 ->
            let x0 = self#attribute _ctx x0 in
            Signature_item_desc.of_concrete (Psig_attribute x0)
          | Psig_extension (x0, x1) ->
            let x0 = self#extension _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            Signature_item_desc.of_concrete (Psig_extension (x0, x1))
      method module_declaration : 'ctx -> Module_declaration.t -> Module_declaration.t  =
        fun _ctx module_declaration ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let pmd_name = self#loc self#string _ctx pmd_name in
          let pmd_type = self#module_type _ctx pmd_type in
          let pmd_attributes = self#attributes _ctx pmd_attributes in
          let pmd_loc = self#location _ctx pmd_loc in
          Module_declaration.of_concrete { pmd_name; pmd_type; pmd_attributes; pmd_loc }
      method module_type_declaration : 'ctx -> Module_type_declaration.t -> Module_type_declaration.t  =
        fun _ctx module_type_declaration ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let pmtd_name = self#loc self#string _ctx pmtd_name in
          let pmtd_type = self#option self#module_type _ctx pmtd_type in
          let pmtd_attributes = self#attributes _ctx pmtd_attributes in
          let pmtd_loc = self#location _ctx pmtd_loc in
          Module_type_declaration.of_concrete { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
      method open_description : 'ctx -> Open_description.t -> Open_description.t  =
        fun _ctx open_description ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let popen_lid = self#longident_loc _ctx popen_lid in
          let popen_override = self#override_flag _ctx popen_override in
          let popen_loc = self#location _ctx popen_loc in
          let popen_attributes = self#attributes _ctx popen_attributes in
          Open_description.of_concrete { popen_lid; popen_override; popen_loc; popen_attributes }
      method include_infos_module_expr : 'ctx -> Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t  =
        fun _ctx include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_expr _ctx pincl_mod in
          let pincl_loc = self#location _ctx pincl_loc in
          let pincl_attributes = self#attributes _ctx pincl_attributes in
          Include_infos.of_concrete_module_expr { pincl_mod; pincl_loc; pincl_attributes }
      method include_infos_module_type : 'ctx -> Module_type.t Include_infos.t -> Module_type.t Include_infos.t  =
        fun _ctx include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_type _ctx pincl_mod in
          let pincl_loc = self#location _ctx pincl_loc in
          let pincl_attributes = self#attributes _ctx pincl_attributes in
          Include_infos.of_concrete_module_type { pincl_mod; pincl_loc; pincl_attributes }
      method include_description : 'ctx -> Include_description.t -> Include_description.t  =
        fun _ctx include_description ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let concrete = self#include_infos_module_type _ctx concrete in
          Include_description.of_concrete concrete
      method include_declaration : 'ctx -> Include_declaration.t -> Include_declaration.t  =
        fun _ctx include_declaration ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let concrete = self#include_infos_module_expr _ctx concrete in
          Include_declaration.of_concrete concrete
      method with_constraint : 'ctx -> With_constraint.t -> With_constraint.t  =
        fun _ctx with_constraint ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#type_declaration _ctx x1 in
            With_constraint.of_concrete (Pwith_type (x0, x1))
          | Pwith_module (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            With_constraint.of_concrete (Pwith_module (x0, x1))
          | Pwith_typesubst (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#type_declaration _ctx x1 in
            With_constraint.of_concrete (Pwith_typesubst (x0, x1))
          | Pwith_modsubst (x0, x1) ->
            let x0 = self#longident_loc _ctx x0 in
            let x1 = self#longident_loc _ctx x1 in
            With_constraint.of_concrete (Pwith_modsubst (x0, x1))
      method module_expr : 'ctx -> Module_expr.t -> Module_expr.t  =
        fun _ctx module_expr ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let pmod_desc = self#module_expr_desc _ctx pmod_desc in
          let pmod_loc = self#location _ctx pmod_loc in
          let pmod_attributes = self#attributes _ctx pmod_attributes in
          Module_expr.of_concrete { pmod_desc; pmod_loc; pmod_attributes }
      method module_expr_desc : 'ctx -> Module_expr_desc.t -> Module_expr_desc.t  =
        fun _ctx module_expr_desc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let x0 = self#longident_loc _ctx x0 in
            Module_expr_desc.of_concrete (Pmod_ident x0)
          | Pmod_structure x0 ->
            let x0 = self#structure _ctx x0 in
            Module_expr_desc.of_concrete (Pmod_structure x0)
          | Pmod_functor (x0, x1, x2) ->
            let x0 = self#loc self#string _ctx x0 in
            let x1 = self#option self#module_type _ctx x1 in
            let x2 = self#module_expr _ctx x2 in
            Module_expr_desc.of_concrete (Pmod_functor (x0, x1, x2))
          | Pmod_apply (x0, x1) ->
            let x0 = self#module_expr _ctx x0 in
            let x1 = self#module_expr _ctx x1 in
            Module_expr_desc.of_concrete (Pmod_apply (x0, x1))
          | Pmod_constraint (x0, x1) ->
            let x0 = self#module_expr _ctx x0 in
            let x1 = self#module_type _ctx x1 in
            Module_expr_desc.of_concrete (Pmod_constraint (x0, x1))
          | Pmod_unpack x0 ->
            let x0 = self#expression _ctx x0 in
            Module_expr_desc.of_concrete (Pmod_unpack x0)
          | Pmod_extension x0 ->
            let x0 = self#extension _ctx x0 in
            Module_expr_desc.of_concrete (Pmod_extension x0)
      method structure : 'ctx -> Structure.t -> Structure.t  =
        fun _ctx structure ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let concrete = self#list self#structure_item _ctx concrete in
          Structure.of_concrete concrete
      method structure_item : 'ctx -> Structure_item.t -> Structure_item.t  =
        fun _ctx structure_item ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let pstr_desc = self#structure_item_desc _ctx pstr_desc in
          let pstr_loc = self#location _ctx pstr_loc in
          Structure_item.of_concrete { pstr_desc; pstr_loc }
      method structure_item_desc : 'ctx -> Structure_item_desc.t -> Structure_item_desc.t  =
        fun _ctx structure_item_desc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let x0 = self#expression _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            Structure_item_desc.of_concrete (Pstr_eval (x0, x1))
          | Pstr_value (x0, x1) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#value_binding _ctx x1 in
            Structure_item_desc.of_concrete (Pstr_value (x0, x1))
          | Pstr_primitive x0 ->
            let x0 = self#value_description _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_primitive x0)
          | Pstr_type (x0, x1) ->
            let x0 = self#rec_flag _ctx x0 in
            let x1 = self#list self#type_declaration _ctx x1 in
            Structure_item_desc.of_concrete (Pstr_type (x0, x1))
          | Pstr_typext x0 ->
            let x0 = self#type_extension _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_typext x0)
          | Pstr_exception x0 ->
            let x0 = self#extension_constructor _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_exception x0)
          | Pstr_module x0 ->
            let x0 = self#module_binding _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_module x0)
          | Pstr_recmodule x0 ->
            let x0 = self#list self#module_binding _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_recmodule x0)
          | Pstr_modtype x0 ->
            let x0 = self#module_type_declaration _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_modtype x0)
          | Pstr_open x0 ->
            let x0 = self#open_description _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_open x0)
          | Pstr_class x0 ->
            let x0 = self#list self#class_declaration _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_class x0)
          | Pstr_class_type x0 ->
            let x0 = self#list self#class_type_declaration _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_class_type x0)
          | Pstr_include x0 ->
            let x0 = self#include_declaration _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_include x0)
          | Pstr_attribute x0 ->
            let x0 = self#attribute _ctx x0 in
            Structure_item_desc.of_concrete (Pstr_attribute x0)
          | Pstr_extension (x0, x1) ->
            let x0 = self#extension _ctx x0 in
            let x1 = self#attributes _ctx x1 in
            Structure_item_desc.of_concrete (Pstr_extension (x0, x1))
      method value_binding : 'ctx -> Value_binding.t -> Value_binding.t  =
        fun _ctx value_binding ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let pvb_pat = self#pattern _ctx pvb_pat in
          let pvb_expr = self#expression _ctx pvb_expr in
          let pvb_attributes = self#attributes _ctx pvb_attributes in
          let pvb_loc = self#location _ctx pvb_loc in
          Value_binding.of_concrete { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
      method module_binding : 'ctx -> Module_binding.t -> Module_binding.t  =
        fun _ctx module_binding ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let pmb_name = self#loc self#string _ctx pmb_name in
          let pmb_expr = self#module_expr _ctx pmb_expr in
          let pmb_attributes = self#attributes _ctx pmb_attributes in
          let pmb_loc = self#location _ctx pmb_loc in
          Module_binding.of_concrete { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
      method toplevel_phrase : 'ctx -> Toplevel_phrase.t -> Toplevel_phrase.t  =
        fun _ctx toplevel_phrase ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let x0 = self#structure _ctx x0 in
            Toplevel_phrase.of_concrete (Ptop_def x0)
          | Ptop_dir (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#directive_argument _ctx x1 in
            Toplevel_phrase.of_concrete (Ptop_dir (x0, x1))
      method directive_argument : 'ctx -> Directive_argument.t -> Directive_argument.t  =
        fun _ctx directive_argument ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            Directive_argument.of_concrete Pdir_none
          | Pdir_string x0 ->
            let x0 = self#string _ctx x0 in
            Directive_argument.of_concrete (Pdir_string x0)
          | Pdir_int (x0, x1) ->
            let x0 = self#string _ctx x0 in
            let x1 = self#option self#char _ctx x1 in
            Directive_argument.of_concrete (Pdir_int (x0, x1))
          | Pdir_ident x0 ->
            let x0 = self#longident _ctx x0 in
            Directive_argument.of_concrete (Pdir_ident x0)
          | Pdir_bool x0 ->
            let x0 = self#bool _ctx x0 in
            Directive_argument.of_concrete (Pdir_bool x0)
    end

  class virtual ['res] lift =
    object (self)
      method virtual record : (string * 'res) list -> 'res
      method virtual constr : string -> 'res list -> 'res
      method virtual tuple : 'res list -> 'res
      method virtual bool : bool -> 'res
      method virtual char : char -> 'res
      method virtual int : int -> 'res
      method virtual list : 'a . ('a -> 'res) -> 'a list -> 'res
      method virtual option : 'a . ('a -> 'res) -> 'a option -> 'res
      method virtual string : string -> 'res
      method virtual location : Astlib.Location.t -> 'res
      method virtual loc : 'a . ('a -> 'res) -> 'a Astlib.Loc.t -> 'res
      method longident : Longident.t -> 'res  =
        fun longident ->
          let concrete =
            match Longident.to_concrete longident with
            | None -> failwith "longident"
            | Some n -> n
          in
          match (concrete : Longident.concrete) with
          | Lident x0 ->
            let x0 = self#string x0 in
            self#constr "Lident" [x0]
          | Ldot (x0, x1) ->
            let x0 = self#longident x0 in
            let x1 = self#string x1 in
            self#constr "Ldot" [x0; x1]
          | Lapply (x0, x1) ->
            let x0 = self#longident x0 in
            let x1 = self#longident x1 in
            self#constr "Lapply" [x0; x1]
      method longident_loc : Longident_loc.t -> 'res  =
        fun longident_loc ->
          let concrete =
            match Longident_loc.to_concrete longident_loc with
            | None -> failwith "longident_loc"
            | Some n -> n
          in
          let concrete = self#loc self#longident concrete in
          concrete
      method rec_flag : Rec_flag.t -> 'res  =
        fun rec_flag ->
          let concrete =
            match Rec_flag.to_concrete rec_flag with
            | None -> failwith "rec_flag"
            | Some n -> n
          in
          match (concrete : Rec_flag.concrete) with
          | Nonrecursive ->
            self#constr "Nonrecursive" []
          | Recursive ->
            self#constr "Recursive" []
      method direction_flag : Direction_flag.t -> 'res  =
        fun direction_flag ->
          let concrete =
            match Direction_flag.to_concrete direction_flag with
            | None -> failwith "direction_flag"
            | Some n -> n
          in
          match (concrete : Direction_flag.concrete) with
          | Upto ->
            self#constr "Upto" []
          | Downto ->
            self#constr "Downto" []
      method private_flag : Private_flag.t -> 'res  =
        fun private_flag ->
          let concrete =
            match Private_flag.to_concrete private_flag with
            | None -> failwith "private_flag"
            | Some n -> n
          in
          match (concrete : Private_flag.concrete) with
          | Private ->
            self#constr "Private" []
          | Public ->
            self#constr "Public" []
      method mutable_flag : Mutable_flag.t -> 'res  =
        fun mutable_flag ->
          let concrete =
            match Mutable_flag.to_concrete mutable_flag with
            | None -> failwith "mutable_flag"
            | Some n -> n
          in
          match (concrete : Mutable_flag.concrete) with
          | Immutable ->
            self#constr "Immutable" []
          | Mutable ->
            self#constr "Mutable" []
      method virtual_flag : Virtual_flag.t -> 'res  =
        fun virtual_flag ->
          let concrete =
            match Virtual_flag.to_concrete virtual_flag with
            | None -> failwith "virtual_flag"
            | Some n -> n
          in
          match (concrete : Virtual_flag.concrete) with
          | Virtual ->
            self#constr "Virtual" []
          | Concrete ->
            self#constr "Concrete" []
      method override_flag : Override_flag.t -> 'res  =
        fun override_flag ->
          let concrete =
            match Override_flag.to_concrete override_flag with
            | None -> failwith "override_flag"
            | Some n -> n
          in
          match (concrete : Override_flag.concrete) with
          | Override ->
            self#constr "Override" []
          | Fresh ->
            self#constr "Fresh" []
      method closed_flag : Closed_flag.t -> 'res  =
        fun closed_flag ->
          let concrete =
            match Closed_flag.to_concrete closed_flag with
            | None -> failwith "closed_flag"
            | Some n -> n
          in
          match (concrete : Closed_flag.concrete) with
          | Closed ->
            self#constr "Closed" []
          | Open ->
            self#constr "Open" []
      method label : Label.t -> 'res  =
        fun label ->
          let concrete =
            match Label.to_concrete label with
            | None -> failwith "label"
            | Some n -> n
          in
          let concrete = self#string concrete in
          concrete
      method arg_label : Arg_label.t -> 'res  =
        fun arg_label ->
          let concrete =
            match Arg_label.to_concrete arg_label with
            | None -> failwith "arg_label"
            | Some n -> n
          in
          match (concrete : Arg_label.concrete) with
          | Nolabel ->
            self#constr "Nolabel" []
          | Labelled x0 ->
            let x0 = self#string x0 in
            self#constr "Labelled" [x0]
          | Optional x0 ->
            let x0 = self#string x0 in
            self#constr "Optional" [x0]
      method variance : Variance.t -> 'res  =
        fun variance ->
          let concrete =
            match Variance.to_concrete variance with
            | None -> failwith "variance"
            | Some n -> n
          in
          match (concrete : Variance.concrete) with
          | Covariant ->
            self#constr "Covariant" []
          | Contravariant ->
            self#constr "Contravariant" []
          | Invariant ->
            self#constr "Invariant" []
      method constant : Constant.t -> 'res  =
        fun constant ->
          let concrete =
            match Constant.to_concrete constant with
            | None -> failwith "constant"
            | Some n -> n
          in
          match (concrete : Constant.concrete) with
          | Pconst_integer (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            self#constr "Pconst_integer" [x0; x1]
          | Pconst_char x0 ->
            let x0 = self#char x0 in
            self#constr "Pconst_char" [x0]
          | Pconst_string (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#string x1 in
            self#constr "Pconst_string" [x0; x1]
          | Pconst_float (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            self#constr "Pconst_float" [x0; x1]
      method attribute : Attribute.t -> 'res  =
        fun attribute ->
          let concrete =
            match Attribute.to_concrete attribute with
            | None -> failwith "attribute"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string x0 in
          let x1 = self#payload x1 in
          self#tuple [x0; x1]
      method extension : Extension.t -> 'res  =
        fun extension ->
          let concrete =
            match Extension.to_concrete extension with
            | None -> failwith "extension"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#loc self#string x0 in
          let x1 = self#payload x1 in
          self#tuple [x0; x1]
      method attributes : Attributes.t -> 'res  =
        fun attributes ->
          let concrete =
            match Attributes.to_concrete attributes with
            | None -> failwith "attributes"
            | Some n -> n
          in
          let concrete = self#list self#attribute concrete in
          concrete
      method payload : Payload.t -> 'res  =
        fun payload ->
          let concrete =
            match Payload.to_concrete payload with
            | None -> failwith "payload"
            | Some n -> n
          in
          match (concrete : Payload.concrete) with
          | PStr x0 ->
            let x0 = self#structure x0 in
            self#constr "PStr" [x0]
          | PSig x0 ->
            let x0 = self#signature x0 in
            self#constr "PSig" [x0]
          | PTyp x0 ->
            let x0 = self#core_type x0 in
            self#constr "PTyp" [x0]
          | PPat (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#option self#expression x1 in
            self#constr "PPat" [x0; x1]
      method core_type : Core_type.t -> 'res  =
        fun core_type ->
          let concrete =
            match Core_type.to_concrete core_type with
            | None -> failwith "core_type"
            | Some n -> n
          in
          let { ptyp_desc; ptyp_loc; ptyp_attributes } : Core_type.concrete = concrete in
          let ptyp_desc = self#core_type_desc ptyp_desc in
          let ptyp_loc = self#location ptyp_loc in
          let ptyp_attributes = self#attributes ptyp_attributes in
          self#record [("ptyp_desc", ptyp_desc); ("ptyp_loc", ptyp_loc); ("ptyp_attributes", ptyp_attributes)]
      method core_type_desc : Core_type_desc.t -> 'res  =
        fun core_type_desc ->
          let concrete =
            match Core_type_desc.to_concrete core_type_desc with
            | None -> failwith "core_type_desc"
            | Some n -> n
          in
          match (concrete : Core_type_desc.concrete) with
          | Ptyp_any ->
            self#constr "Ptyp_any" []
          | Ptyp_var x0 ->
            let x0 = self#string x0 in
            self#constr "Ptyp_var" [x0]
          | Ptyp_arrow (x0, x1, x2) ->
            let x0 = self#arg_label x0 in
            let x1 = self#core_type x1 in
            let x2 = self#core_type x2 in
            self#constr "Ptyp_arrow" [x0; x1; x2]
          | Ptyp_tuple x0 ->
            let x0 = self#list self#core_type x0 in
            self#constr "Ptyp_tuple" [x0]
          | Ptyp_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            self#constr "Ptyp_constr" [x0; x1]
          | Ptyp_object (x0, x1) ->
            let x0 = self#list self#object_field x0 in
            let x1 = self#closed_flag x1 in
            self#constr "Ptyp_object" [x0; x1]
          | Ptyp_class (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            self#constr "Ptyp_class" [x0; x1]
          | Ptyp_alias (x0, x1) ->
            let x0 = self#core_type x0 in
            let x1 = self#string x1 in
            self#constr "Ptyp_alias" [x0; x1]
          | Ptyp_variant (x0, x1, x2) ->
            let x0 = self#list self#row_field x0 in
            let x1 = self#closed_flag x1 in
            let x2 = self#option (self#list self#label) x2 in
            self#constr "Ptyp_variant" [x0; x1; x2]
          | Ptyp_poly (x0, x1) ->
            let x0 = self#list (self#loc self#string) x0 in
            let x1 = self#core_type x1 in
            self#constr "Ptyp_poly" [x0; x1]
          | Ptyp_package x0 ->
            let x0 = self#package_type x0 in
            self#constr "Ptyp_package" [x0]
          | Ptyp_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Ptyp_extension" [x0]
      method package_type : Package_type.t -> 'res  =
        fun package_type ->
          let concrete =
            match Package_type.to_concrete package_type with
            | None -> failwith "package_type"
            | Some n -> n
          in
          let (x0, x1) = concrete in
          let x0 = self#longident_loc x0 in
          let x1 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#core_type x1 in self#tuple [x0; x1]) x1 in
          self#tuple [x0; x1]
      method row_field : Row_field.t -> 'res  =
        fun row_field ->
          let concrete =
            match Row_field.to_concrete row_field with
            | None -> failwith "row_field"
            | Some n -> n
          in
          match (concrete : Row_field.concrete) with
          | Rtag (x0, x1, x2, x3) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#attributes x1 in
            let x2 = self#bool x2 in
            let x3 = self#list self#core_type x3 in
            self#constr "Rtag" [x0; x1; x2; x3]
          | Rinherit x0 ->
            let x0 = self#core_type x0 in
            self#constr "Rinherit" [x0]
      method object_field : Object_field.t -> 'res  =
        fun object_field ->
          let concrete =
            match Object_field.to_concrete object_field with
            | None -> failwith "object_field"
            | Some n -> n
          in
          match (concrete : Object_field.concrete) with
          | Otag (x0, x1, x2) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#attributes x1 in
            let x2 = self#core_type x2 in
            self#constr "Otag" [x0; x1; x2]
          | Oinherit x0 ->
            let x0 = self#core_type x0 in
            self#constr "Oinherit" [x0]
      method pattern : Pattern.t -> 'res  =
        fun pattern ->
          let concrete =
            match Pattern.to_concrete pattern with
            | None -> failwith "pattern"
            | Some n -> n
          in
          let { ppat_desc; ppat_loc; ppat_attributes } : Pattern.concrete = concrete in
          let ppat_desc = self#pattern_desc ppat_desc in
          let ppat_loc = self#location ppat_loc in
          let ppat_attributes = self#attributes ppat_attributes in
          self#record [("ppat_desc", ppat_desc); ("ppat_loc", ppat_loc); ("ppat_attributes", ppat_attributes)]
      method pattern_desc : Pattern_desc.t -> 'res  =
        fun pattern_desc ->
          let concrete =
            match Pattern_desc.to_concrete pattern_desc with
            | None -> failwith "pattern_desc"
            | Some n -> n
          in
          match (concrete : Pattern_desc.concrete) with
          | Ppat_any ->
            self#constr "Ppat_any" []
          | Ppat_var x0 ->
            let x0 = self#loc self#string x0 in
            self#constr "Ppat_var" [x0]
          | Ppat_alias (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#loc self#string x1 in
            self#constr "Ppat_alias" [x0; x1]
          | Ppat_constant x0 ->
            let x0 = self#constant x0 in
            self#constr "Ppat_constant" [x0]
          | Ppat_interval (x0, x1) ->
            let x0 = self#constant x0 in
            let x1 = self#constant x1 in
            self#constr "Ppat_interval" [x0; x1]
          | Ppat_tuple x0 ->
            let x0 = self#list self#pattern x0 in
            self#constr "Ppat_tuple" [x0]
          | Ppat_construct (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#option self#pattern x1 in
            self#constr "Ppat_construct" [x0; x1]
          | Ppat_variant (x0, x1) ->
            let x0 = self#label x0 in
            let x1 = self#option self#pattern x1 in
            self#constr "Ppat_variant" [x0; x1]
          | Ppat_record (x0, x1) ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#pattern x1 in self#tuple [x0; x1]) x0 in
            let x1 = self#closed_flag x1 in
            self#constr "Ppat_record" [x0; x1]
          | Ppat_array x0 ->
            let x0 = self#list self#pattern x0 in
            self#constr "Ppat_array" [x0]
          | Ppat_or (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#pattern x1 in
            self#constr "Ppat_or" [x0; x1]
          | Ppat_constraint (x0, x1) ->
            let x0 = self#pattern x0 in
            let x1 = self#core_type x1 in
            self#constr "Ppat_constraint" [x0; x1]
          | Ppat_type x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Ppat_type" [x0]
          | Ppat_lazy x0 ->
            let x0 = self#pattern x0 in
            self#constr "Ppat_lazy" [x0]
          | Ppat_unpack x0 ->
            let x0 = self#loc self#string x0 in
            self#constr "Ppat_unpack" [x0]
          | Ppat_exception x0 ->
            let x0 = self#pattern x0 in
            self#constr "Ppat_exception" [x0]
          | Ppat_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Ppat_extension" [x0]
          | Ppat_open (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#pattern x1 in
            self#constr "Ppat_open" [x0; x1]
      method expression : Expression.t -> 'res  =
        fun expression ->
          let concrete =
            match Expression.to_concrete expression with
            | None -> failwith "expression"
            | Some n -> n
          in
          let { pexp_desc; pexp_loc; pexp_attributes } : Expression.concrete = concrete in
          let pexp_desc = self#expression_desc pexp_desc in
          let pexp_loc = self#location pexp_loc in
          let pexp_attributes = self#attributes pexp_attributes in
          self#record [("pexp_desc", pexp_desc); ("pexp_loc", pexp_loc); ("pexp_attributes", pexp_attributes)]
      method expression_desc : Expression_desc.t -> 'res  =
        fun expression_desc ->
          let concrete =
            match Expression_desc.to_concrete expression_desc with
            | None -> failwith "expression_desc"
            | Some n -> n
          in
          match (concrete : Expression_desc.concrete) with
          | Pexp_ident x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pexp_ident" [x0]
          | Pexp_constant x0 ->
            let x0 = self#constant x0 in
            self#constr "Pexp_constant" [x0]
          | Pexp_let (x0, x1, x2) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            let x2 = self#expression x2 in
            self#constr "Pexp_let" [x0; x1; x2]
          | Pexp_function x0 ->
            let x0 = self#list self#case x0 in
            self#constr "Pexp_function" [x0]
          | Pexp_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label x0 in
            let x1 = self#option self#expression x1 in
            let x2 = self#pattern x2 in
            let x3 = self#expression x3 in
            self#constr "Pexp_fun" [x0; x1; x2; x3]
          | Pexp_apply (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list (fun (x0, x1) -> let x0 = self#arg_label x0 in let x1 = self#expression x1 in self#tuple [x0; x1]) x1 in
            self#constr "Pexp_apply" [x0; x1]
          | Pexp_match (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list self#case x1 in
            self#constr "Pexp_match" [x0; x1]
          | Pexp_try (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#list self#case x1 in
            self#constr "Pexp_try" [x0; x1]
          | Pexp_tuple x0 ->
            let x0 = self#list self#expression x0 in
            self#constr "Pexp_tuple" [x0]
          | Pexp_construct (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#option self#expression x1 in
            self#constr "Pexp_construct" [x0; x1]
          | Pexp_variant (x0, x1) ->
            let x0 = self#label x0 in
            let x1 = self#option self#expression x1 in
            self#constr "Pexp_variant" [x0; x1]
          | Pexp_record (x0, x1) ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#longident_loc x0 in let x1 = self#expression x1 in self#tuple [x0; x1]) x0 in
            let x1 = self#option self#expression x1 in
            self#constr "Pexp_record" [x0; x1]
          | Pexp_field (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#longident_loc x1 in
            self#constr "Pexp_field" [x0; x1]
          | Pexp_setfield (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#expression x2 in
            self#constr "Pexp_setfield" [x0; x1; x2]
          | Pexp_array x0 ->
            let x0 = self#list self#expression x0 in
            self#constr "Pexp_array" [x0]
          | Pexp_ifthenelse (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            let x2 = self#option self#expression x2 in
            self#constr "Pexp_ifthenelse" [x0; x1; x2]
          | Pexp_sequence (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_sequence" [x0; x1]
          | Pexp_while (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_while" [x0; x1]
          | Pexp_for (x0, x1, x2, x3, x4) ->
            let x0 = self#pattern x0 in
            let x1 = self#expression x1 in
            let x2 = self#expression x2 in
            let x3 = self#direction_flag x3 in
            let x4 = self#expression x4 in
            self#constr "Pexp_for" [x0; x1; x2; x3; x4]
          | Pexp_constraint (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#core_type x1 in
            self#constr "Pexp_constraint" [x0; x1]
          | Pexp_coerce (x0, x1, x2) ->
            let x0 = self#expression x0 in
            let x1 = self#option self#core_type x1 in
            let x2 = self#core_type x2 in
            self#constr "Pexp_coerce" [x0; x1; x2]
          | Pexp_send (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#loc self#label x1 in
            self#constr "Pexp_send" [x0; x1]
          | Pexp_new x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pexp_new" [x0]
          | Pexp_setinstvar (x0, x1) ->
            let x0 = self#loc self#label x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_setinstvar" [x0; x1]
          | Pexp_override x0 ->
            let x0 = self#list (fun (x0, x1) -> let x0 = self#loc self#label x0 in let x1 = self#expression x1 in self#tuple [x0; x1]) x0 in
            self#constr "Pexp_override" [x0]
          | Pexp_letmodule (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#module_expr x1 in
            let x2 = self#expression x2 in
            self#constr "Pexp_letmodule" [x0; x1; x2]
          | Pexp_letexception (x0, x1) ->
            let x0 = self#extension_constructor x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_letexception" [x0; x1]
          | Pexp_assert x0 ->
            let x0 = self#expression x0 in
            self#constr "Pexp_assert" [x0]
          | Pexp_lazy x0 ->
            let x0 = self#expression x0 in
            self#constr "Pexp_lazy" [x0]
          | Pexp_poly (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#option self#core_type x1 in
            self#constr "Pexp_poly" [x0; x1]
          | Pexp_object x0 ->
            let x0 = self#class_structure x0 in
            self#constr "Pexp_object" [x0]
          | Pexp_newtype (x0, x1) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#expression x1 in
            self#constr "Pexp_newtype" [x0; x1]
          | Pexp_pack x0 ->
            let x0 = self#module_expr x0 in
            self#constr "Pexp_pack" [x0]
          | Pexp_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#expression x2 in
            self#constr "Pexp_open" [x0; x1; x2]
          | Pexp_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pexp_extension" [x0]
          | Pexp_unreachable ->
            self#constr "Pexp_unreachable" []
      method case : Case.t -> 'res  =
        fun case ->
          let concrete =
            match Case.to_concrete case with
            | None -> failwith "case"
            | Some n -> n
          in
          let { pc_lhs; pc_guard; pc_rhs } : Case.concrete = concrete in
          let pc_lhs = self#pattern pc_lhs in
          let pc_guard = self#option self#expression pc_guard in
          let pc_rhs = self#expression pc_rhs in
          self#record [("pc_lhs", pc_lhs); ("pc_guard", pc_guard); ("pc_rhs", pc_rhs)]
      method value_description : Value_description.t -> 'res  =
        fun value_description ->
          let concrete =
            match Value_description.to_concrete value_description with
            | None -> failwith "value_description"
            | Some n -> n
          in
          let { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Value_description.concrete = concrete in
          let pval_name = self#loc self#string pval_name in
          let pval_type = self#core_type pval_type in
          let pval_prim = self#list self#string pval_prim in
          let pval_attributes = self#attributes pval_attributes in
          let pval_loc = self#location pval_loc in
          self#record [("pval_name", pval_name); ("pval_type", pval_type); ("pval_prim", pval_prim); ("pval_attributes", pval_attributes); ("pval_loc", pval_loc)]
      method type_declaration : Type_declaration.t -> 'res  =
        fun type_declaration ->
          let concrete =
            match Type_declaration.to_concrete type_declaration with
            | None -> failwith "type_declaration"
            | Some n -> n
          in
          let { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Type_declaration.concrete = concrete in
          let ptype_name = self#loc self#string ptype_name in
          let ptype_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in self#tuple [x0; x1]) ptype_params in
          let ptype_cstrs = self#list (fun (x0, x1, x2) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in let x2 = self#location x2 in self#tuple [x0; x1; x2]) ptype_cstrs in
          let ptype_kind = self#type_kind ptype_kind in
          let ptype_private = self#private_flag ptype_private in
          let ptype_manifest = self#option self#core_type ptype_manifest in
          let ptype_attributes = self#attributes ptype_attributes in
          let ptype_loc = self#location ptype_loc in
          self#record [("ptype_name", ptype_name); ("ptype_params", ptype_params); ("ptype_cstrs", ptype_cstrs); ("ptype_kind", ptype_kind); ("ptype_private", ptype_private); ("ptype_manifest", ptype_manifest); ("ptype_attributes", ptype_attributes); ("ptype_loc", ptype_loc)]
      method type_kind : Type_kind.t -> 'res  =
        fun type_kind ->
          let concrete =
            match Type_kind.to_concrete type_kind with
            | None -> failwith "type_kind"
            | Some n -> n
          in
          match (concrete : Type_kind.concrete) with
          | Ptype_abstract ->
            self#constr "Ptype_abstract" []
          | Ptype_variant x0 ->
            let x0 = self#list self#constructor_declaration x0 in
            self#constr "Ptype_variant" [x0]
          | Ptype_record x0 ->
            let x0 = self#list self#label_declaration x0 in
            self#constr "Ptype_record" [x0]
          | Ptype_open ->
            self#constr "Ptype_open" []
      method label_declaration : Label_declaration.t -> 'res  =
        fun label_declaration ->
          let concrete =
            match Label_declaration.to_concrete label_declaration with
            | None -> failwith "label_declaration"
            | Some n -> n
          in
          let { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Label_declaration.concrete = concrete in
          let pld_name = self#loc self#string pld_name in
          let pld_mutable = self#mutable_flag pld_mutable in
          let pld_type = self#core_type pld_type in
          let pld_loc = self#location pld_loc in
          let pld_attributes = self#attributes pld_attributes in
          self#record [("pld_name", pld_name); ("pld_mutable", pld_mutable); ("pld_type", pld_type); ("pld_loc", pld_loc); ("pld_attributes", pld_attributes)]
      method constructor_declaration : Constructor_declaration.t -> 'res  =
        fun constructor_declaration ->
          let concrete =
            match Constructor_declaration.to_concrete constructor_declaration with
            | None -> failwith "constructor_declaration"
            | Some n -> n
          in
          let { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Constructor_declaration.concrete = concrete in
          let pcd_name = self#loc self#string pcd_name in
          let pcd_args = self#constructor_arguments pcd_args in
          let pcd_res = self#option self#core_type pcd_res in
          let pcd_loc = self#location pcd_loc in
          let pcd_attributes = self#attributes pcd_attributes in
          self#record [("pcd_name", pcd_name); ("pcd_args", pcd_args); ("pcd_res", pcd_res); ("pcd_loc", pcd_loc); ("pcd_attributes", pcd_attributes)]
      method constructor_arguments : Constructor_arguments.t -> 'res  =
        fun constructor_arguments ->
          let concrete =
            match Constructor_arguments.to_concrete constructor_arguments with
            | None -> failwith "constructor_arguments"
            | Some n -> n
          in
          match (concrete : Constructor_arguments.concrete) with
          | Pcstr_tuple x0 ->
            let x0 = self#list self#core_type x0 in
            self#constr "Pcstr_tuple" [x0]
          | Pcstr_record x0 ->
            let x0 = self#list self#label_declaration x0 in
            self#constr "Pcstr_record" [x0]
      method type_extension : Type_extension.t -> 'res  =
        fun type_extension ->
          let concrete =
            match Type_extension.to_concrete type_extension with
            | None -> failwith "type_extension"
            | Some n -> n
          in
          let { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Type_extension.concrete = concrete in
          let ptyext_path = self#longident_loc ptyext_path in
          let ptyext_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in self#tuple [x0; x1]) ptyext_params in
          let ptyext_constructors = self#list self#extension_constructor ptyext_constructors in
          let ptyext_private = self#private_flag ptyext_private in
          let ptyext_attributes = self#attributes ptyext_attributes in
          self#record [("ptyext_path", ptyext_path); ("ptyext_params", ptyext_params); ("ptyext_constructors", ptyext_constructors); ("ptyext_private", ptyext_private); ("ptyext_attributes", ptyext_attributes)]
      method extension_constructor : Extension_constructor.t -> 'res  =
        fun extension_constructor ->
          let concrete =
            match Extension_constructor.to_concrete extension_constructor with
            | None -> failwith "extension_constructor"
            | Some n -> n
          in
          let { pext_name; pext_kind; pext_loc; pext_attributes } : Extension_constructor.concrete = concrete in
          let pext_name = self#loc self#string pext_name in
          let pext_kind = self#extension_constructor_kind pext_kind in
          let pext_loc = self#location pext_loc in
          let pext_attributes = self#attributes pext_attributes in
          self#record [("pext_name", pext_name); ("pext_kind", pext_kind); ("pext_loc", pext_loc); ("pext_attributes", pext_attributes)]
      method extension_constructor_kind : Extension_constructor_kind.t -> 'res  =
        fun extension_constructor_kind ->
          let concrete =
            match Extension_constructor_kind.to_concrete extension_constructor_kind with
            | None -> failwith "extension_constructor_kind"
            | Some n -> n
          in
          match (concrete : Extension_constructor_kind.concrete) with
          | Pext_decl (x0, x1) ->
            let x0 = self#constructor_arguments x0 in
            let x1 = self#option self#core_type x1 in
            self#constr "Pext_decl" [x0; x1]
          | Pext_rebind x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pext_rebind" [x0]
      method class_type : Class_type.t -> 'res  =
        fun class_type ->
          let concrete =
            match Class_type.to_concrete class_type with
            | None -> failwith "class_type"
            | Some n -> n
          in
          let { pcty_desc; pcty_loc; pcty_attributes } : Class_type.concrete = concrete in
          let pcty_desc = self#class_type_desc pcty_desc in
          let pcty_loc = self#location pcty_loc in
          let pcty_attributes = self#attributes pcty_attributes in
          self#record [("pcty_desc", pcty_desc); ("pcty_loc", pcty_loc); ("pcty_attributes", pcty_attributes)]
      method class_type_desc : Class_type_desc.t -> 'res  =
        fun class_type_desc ->
          let concrete =
            match Class_type_desc.to_concrete class_type_desc with
            | None -> failwith "class_type_desc"
            | Some n -> n
          in
          match (concrete : Class_type_desc.concrete) with
          | Pcty_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            self#constr "Pcty_constr" [x0; x1]
          | Pcty_signature x0 ->
            let x0 = self#class_signature x0 in
            self#constr "Pcty_signature" [x0]
          | Pcty_arrow (x0, x1, x2) ->
            let x0 = self#arg_label x0 in
            let x1 = self#core_type x1 in
            let x2 = self#class_type x2 in
            self#constr "Pcty_arrow" [x0; x1; x2]
          | Pcty_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pcty_extension" [x0]
          | Pcty_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#class_type x2 in
            self#constr "Pcty_open" [x0; x1; x2]
      method class_signature : Class_signature.t -> 'res  =
        fun class_signature ->
          let concrete =
            match Class_signature.to_concrete class_signature with
            | None -> failwith "class_signature"
            | Some n -> n
          in
          let { pcsig_self; pcsig_fields } : Class_signature.concrete = concrete in
          let pcsig_self = self#core_type pcsig_self in
          let pcsig_fields = self#list self#class_type_field pcsig_fields in
          self#record [("pcsig_self", pcsig_self); ("pcsig_fields", pcsig_fields)]
      method class_type_field : Class_type_field.t -> 'res  =
        fun class_type_field ->
          let concrete =
            match Class_type_field.to_concrete class_type_field with
            | None -> failwith "class_type_field"
            | Some n -> n
          in
          let { pctf_desc; pctf_loc; pctf_attributes } : Class_type_field.concrete = concrete in
          let pctf_desc = self#class_type_field_desc pctf_desc in
          let pctf_loc = self#location pctf_loc in
          let pctf_attributes = self#attributes pctf_attributes in
          self#record [("pctf_desc", pctf_desc); ("pctf_loc", pctf_loc); ("pctf_attributes", pctf_attributes)]
      method class_type_field_desc : Class_type_field_desc.t -> 'res  =
        fun class_type_field_desc ->
          let concrete =
            match Class_type_field_desc.to_concrete class_type_field_desc with
            | None -> failwith "class_type_field_desc"
            | Some n -> n
          in
          match (concrete : Class_type_field_desc.concrete) with
          | Pctf_inherit x0 ->
            let x0 = self#class_type x0 in
            self#constr "Pctf_inherit" [x0]
          | Pctf_val x0 ->
            let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#loc self#label x0 in let x1 = self#mutable_flag x1 in let x2 = self#virtual_flag x2 in let x3 = self#core_type x3 in self#tuple [x0; x1; x2; x3]) x0 in
            self#constr "Pctf_val" [x0]
          | Pctf_method x0 ->
            let x0 = (fun (x0, x1, x2, x3) -> let x0 = self#loc self#label x0 in let x1 = self#private_flag x1 in let x2 = self#virtual_flag x2 in let x3 = self#core_type x3 in self#tuple [x0; x1; x2; x3]) x0 in
            self#constr "Pctf_method" [x0]
          | Pctf_constraint x0 ->
            let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in self#tuple [x0; x1]) x0 in
            self#constr "Pctf_constraint" [x0]
          | Pctf_attribute x0 ->
            let x0 = self#attribute x0 in
            self#constr "Pctf_attribute" [x0]
          | Pctf_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pctf_extension" [x0]
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'res  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_expr class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_expr.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag pci_virt in
          let pci_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in self#tuple [x0; x1]) pci_params in
          let pci_name = self#loc self#string pci_name in
          let pci_expr = self#class_expr pci_expr in
          let pci_loc = self#location pci_loc in
          let pci_attributes = self#attributes pci_attributes in
          self#record [("pci_virt", pci_virt); ("pci_params", pci_params); ("pci_name", pci_name); ("pci_expr", pci_expr); ("pci_loc", pci_loc); ("pci_attributes", pci_attributes)]
      method class_infos_class_type : Class_type.t Class_infos.t -> 'res  =
        fun class_infos ->
          let concrete =
            match Class_infos.to_concrete_class_type class_infos with
            | None -> failwith "class_infos"
            | Some n -> n
          in
          let { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Class_type.t Class_infos.concrete = concrete in
          let pci_virt = self#virtual_flag pci_virt in
          let pci_params = self#list (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#variance x1 in self#tuple [x0; x1]) pci_params in
          let pci_name = self#loc self#string pci_name in
          let pci_expr = self#class_type pci_expr in
          let pci_loc = self#location pci_loc in
          let pci_attributes = self#attributes pci_attributes in
          self#record [("pci_virt", pci_virt); ("pci_params", pci_params); ("pci_name", pci_name); ("pci_expr", pci_expr); ("pci_loc", pci_loc); ("pci_attributes", pci_attributes)]
      method class_description : Class_description.t -> 'res  =
        fun class_description ->
          let concrete =
            match Class_description.to_concrete class_description with
            | None -> failwith "class_description"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type concrete in
          concrete
      method class_type_declaration : Class_type_declaration.t -> 'res  =
        fun class_type_declaration ->
          let concrete =
            match Class_type_declaration.to_concrete class_type_declaration with
            | None -> failwith "class_type_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_type concrete in
          concrete
      method class_expr : Class_expr.t -> 'res  =
        fun class_expr ->
          let concrete =
            match Class_expr.to_concrete class_expr with
            | None -> failwith "class_expr"
            | Some n -> n
          in
          let { pcl_desc; pcl_loc; pcl_attributes } : Class_expr.concrete = concrete in
          let pcl_desc = self#class_expr_desc pcl_desc in
          let pcl_loc = self#location pcl_loc in
          let pcl_attributes = self#attributes pcl_attributes in
          self#record [("pcl_desc", pcl_desc); ("pcl_loc", pcl_loc); ("pcl_attributes", pcl_attributes)]
      method class_expr_desc : Class_expr_desc.t -> 'res  =
        fun class_expr_desc ->
          let concrete =
            match Class_expr_desc.to_concrete class_expr_desc with
            | None -> failwith "class_expr_desc"
            | Some n -> n
          in
          match (concrete : Class_expr_desc.concrete) with
          | Pcl_constr (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#list self#core_type x1 in
            self#constr "Pcl_constr" [x0; x1]
          | Pcl_structure x0 ->
            let x0 = self#class_structure x0 in
            self#constr "Pcl_structure" [x0]
          | Pcl_fun (x0, x1, x2, x3) ->
            let x0 = self#arg_label x0 in
            let x1 = self#option self#expression x1 in
            let x2 = self#pattern x2 in
            let x3 = self#class_expr x3 in
            self#constr "Pcl_fun" [x0; x1; x2; x3]
          | Pcl_apply (x0, x1) ->
            let x0 = self#class_expr x0 in
            let x1 = self#list (fun (x0, x1) -> let x0 = self#arg_label x0 in let x1 = self#expression x1 in self#tuple [x0; x1]) x1 in
            self#constr "Pcl_apply" [x0; x1]
          | Pcl_let (x0, x1, x2) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            let x2 = self#class_expr x2 in
            self#constr "Pcl_let" [x0; x1; x2]
          | Pcl_constraint (x0, x1) ->
            let x0 = self#class_expr x0 in
            let x1 = self#class_type x1 in
            self#constr "Pcl_constraint" [x0; x1]
          | Pcl_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pcl_extension" [x0]
          | Pcl_open (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#longident_loc x1 in
            let x2 = self#class_expr x2 in
            self#constr "Pcl_open" [x0; x1; x2]
      method class_structure : Class_structure.t -> 'res  =
        fun class_structure ->
          let concrete =
            match Class_structure.to_concrete class_structure with
            | None -> failwith "class_structure"
            | Some n -> n
          in
          let { pcstr_self; pcstr_fields } : Class_structure.concrete = concrete in
          let pcstr_self = self#pattern pcstr_self in
          let pcstr_fields = self#list self#class_field pcstr_fields in
          self#record [("pcstr_self", pcstr_self); ("pcstr_fields", pcstr_fields)]
      method class_field : Class_field.t -> 'res  =
        fun class_field ->
          let concrete =
            match Class_field.to_concrete class_field with
            | None -> failwith "class_field"
            | Some n -> n
          in
          let { pcf_desc; pcf_loc; pcf_attributes } : Class_field.concrete = concrete in
          let pcf_desc = self#class_field_desc pcf_desc in
          let pcf_loc = self#location pcf_loc in
          let pcf_attributes = self#attributes pcf_attributes in
          self#record [("pcf_desc", pcf_desc); ("pcf_loc", pcf_loc); ("pcf_attributes", pcf_attributes)]
      method class_field_desc : Class_field_desc.t -> 'res  =
        fun class_field_desc ->
          let concrete =
            match Class_field_desc.to_concrete class_field_desc with
            | None -> failwith "class_field_desc"
            | Some n -> n
          in
          match (concrete : Class_field_desc.concrete) with
          | Pcf_inherit (x0, x1, x2) ->
            let x0 = self#override_flag x0 in
            let x1 = self#class_expr x1 in
            let x2 = self#option (self#loc self#string) x2 in
            self#constr "Pcf_inherit" [x0; x1; x2]
          | Pcf_val x0 ->
            let x0 = (fun (x0, x1, x2) -> let x0 = self#loc self#label x0 in let x1 = self#mutable_flag x1 in let x2 = self#class_field_kind x2 in self#tuple [x0; x1; x2]) x0 in
            self#constr "Pcf_val" [x0]
          | Pcf_method x0 ->
            let x0 = (fun (x0, x1, x2) -> let x0 = self#loc self#label x0 in let x1 = self#private_flag x1 in let x2 = self#class_field_kind x2 in self#tuple [x0; x1; x2]) x0 in
            self#constr "Pcf_method" [x0]
          | Pcf_constraint x0 ->
            let x0 = (fun (x0, x1) -> let x0 = self#core_type x0 in let x1 = self#core_type x1 in self#tuple [x0; x1]) x0 in
            self#constr "Pcf_constraint" [x0]
          | Pcf_initializer x0 ->
            let x0 = self#expression x0 in
            self#constr "Pcf_initializer" [x0]
          | Pcf_attribute x0 ->
            let x0 = self#attribute x0 in
            self#constr "Pcf_attribute" [x0]
          | Pcf_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pcf_extension" [x0]
      method class_field_kind : Class_field_kind.t -> 'res  =
        fun class_field_kind ->
          let concrete =
            match Class_field_kind.to_concrete class_field_kind with
            | None -> failwith "class_field_kind"
            | Some n -> n
          in
          match (concrete : Class_field_kind.concrete) with
          | Cfk_virtual x0 ->
            let x0 = self#core_type x0 in
            self#constr "Cfk_virtual" [x0]
          | Cfk_concrete (x0, x1) ->
            let x0 = self#override_flag x0 in
            let x1 = self#expression x1 in
            self#constr "Cfk_concrete" [x0; x1]
      method class_declaration : Class_declaration.t -> 'res  =
        fun class_declaration ->
          let concrete =
            match Class_declaration.to_concrete class_declaration with
            | None -> failwith "class_declaration"
            | Some n -> n
          in
          let concrete = self#class_infos_class_expr concrete in
          concrete
      method module_type : Module_type.t -> 'res  =
        fun module_type ->
          let concrete =
            match Module_type.to_concrete module_type with
            | None -> failwith "module_type"
            | Some n -> n
          in
          let { pmty_desc; pmty_loc; pmty_attributes } : Module_type.concrete = concrete in
          let pmty_desc = self#module_type_desc pmty_desc in
          let pmty_loc = self#location pmty_loc in
          let pmty_attributes = self#attributes pmty_attributes in
          self#record [("pmty_desc", pmty_desc); ("pmty_loc", pmty_loc); ("pmty_attributes", pmty_attributes)]
      method module_type_desc : Module_type_desc.t -> 'res  =
        fun module_type_desc ->
          let concrete =
            match Module_type_desc.to_concrete module_type_desc with
            | None -> failwith "module_type_desc"
            | Some n -> n
          in
          match (concrete : Module_type_desc.concrete) with
          | Pmty_ident x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pmty_ident" [x0]
          | Pmty_signature x0 ->
            let x0 = self#signature x0 in
            self#constr "Pmty_signature" [x0]
          | Pmty_functor (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#option self#module_type x1 in
            let x2 = self#module_type x2 in
            self#constr "Pmty_functor" [x0; x1; x2]
          | Pmty_with (x0, x1) ->
            let x0 = self#module_type x0 in
            let x1 = self#list self#with_constraint x1 in
            self#constr "Pmty_with" [x0; x1]
          | Pmty_typeof x0 ->
            let x0 = self#module_expr x0 in
            self#constr "Pmty_typeof" [x0]
          | Pmty_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pmty_extension" [x0]
          | Pmty_alias x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pmty_alias" [x0]
      method signature : Signature.t -> 'res  =
        fun signature ->
          let concrete =
            match Signature.to_concrete signature with
            | None -> failwith "signature"
            | Some n -> n
          in
          let concrete = self#list self#signature_item concrete in
          concrete
      method signature_item : Signature_item.t -> 'res  =
        fun signature_item ->
          let concrete =
            match Signature_item.to_concrete signature_item with
            | None -> failwith "signature_item"
            | Some n -> n
          in
          let { psig_desc; psig_loc } : Signature_item.concrete = concrete in
          let psig_desc = self#signature_item_desc psig_desc in
          let psig_loc = self#location psig_loc in
          self#record [("psig_desc", psig_desc); ("psig_loc", psig_loc)]
      method signature_item_desc : Signature_item_desc.t -> 'res  =
        fun signature_item_desc ->
          let concrete =
            match Signature_item_desc.to_concrete signature_item_desc with
            | None -> failwith "signature_item_desc"
            | Some n -> n
          in
          match (concrete : Signature_item_desc.concrete) with
          | Psig_value x0 ->
            let x0 = self#value_description x0 in
            self#constr "Psig_value" [x0]
          | Psig_type (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#type_declaration x1 in
            self#constr "Psig_type" [x0; x1]
          | Psig_typext x0 ->
            let x0 = self#type_extension x0 in
            self#constr "Psig_typext" [x0]
          | Psig_exception x0 ->
            let x0 = self#extension_constructor x0 in
            self#constr "Psig_exception" [x0]
          | Psig_module x0 ->
            let x0 = self#module_declaration x0 in
            self#constr "Psig_module" [x0]
          | Psig_recmodule x0 ->
            let x0 = self#list self#module_declaration x0 in
            self#constr "Psig_recmodule" [x0]
          | Psig_modtype x0 ->
            let x0 = self#module_type_declaration x0 in
            self#constr "Psig_modtype" [x0]
          | Psig_open x0 ->
            let x0 = self#open_description x0 in
            self#constr "Psig_open" [x0]
          | Psig_include x0 ->
            let x0 = self#include_description x0 in
            self#constr "Psig_include" [x0]
          | Psig_class x0 ->
            let x0 = self#list self#class_description x0 in
            self#constr "Psig_class" [x0]
          | Psig_class_type x0 ->
            let x0 = self#list self#class_type_declaration x0 in
            self#constr "Psig_class_type" [x0]
          | Psig_attribute x0 ->
            let x0 = self#attribute x0 in
            self#constr "Psig_attribute" [x0]
          | Psig_extension (x0, x1) ->
            let x0 = self#extension x0 in
            let x1 = self#attributes x1 in
            self#constr "Psig_extension" [x0; x1]
      method module_declaration : Module_declaration.t -> 'res  =
        fun module_declaration ->
          let concrete =
            match Module_declaration.to_concrete module_declaration with
            | None -> failwith "module_declaration"
            | Some n -> n
          in
          let { pmd_name; pmd_type; pmd_attributes; pmd_loc } : Module_declaration.concrete = concrete in
          let pmd_name = self#loc self#string pmd_name in
          let pmd_type = self#module_type pmd_type in
          let pmd_attributes = self#attributes pmd_attributes in
          let pmd_loc = self#location pmd_loc in
          self#record [("pmd_name", pmd_name); ("pmd_type", pmd_type); ("pmd_attributes", pmd_attributes); ("pmd_loc", pmd_loc)]
      method module_type_declaration : Module_type_declaration.t -> 'res  =
        fun module_type_declaration ->
          let concrete =
            match Module_type_declaration.to_concrete module_type_declaration with
            | None -> failwith "module_type_declaration"
            | Some n -> n
          in
          let { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Module_type_declaration.concrete = concrete in
          let pmtd_name = self#loc self#string pmtd_name in
          let pmtd_type = self#option self#module_type pmtd_type in
          let pmtd_attributes = self#attributes pmtd_attributes in
          let pmtd_loc = self#location pmtd_loc in
          self#record [("pmtd_name", pmtd_name); ("pmtd_type", pmtd_type); ("pmtd_attributes", pmtd_attributes); ("pmtd_loc", pmtd_loc)]
      method open_description : Open_description.t -> 'res  =
        fun open_description ->
          let concrete =
            match Open_description.to_concrete open_description with
            | None -> failwith "open_description"
            | Some n -> n
          in
          let { popen_lid; popen_override; popen_loc; popen_attributes } : Open_description.concrete = concrete in
          let popen_lid = self#longident_loc popen_lid in
          let popen_override = self#override_flag popen_override in
          let popen_loc = self#location popen_loc in
          let popen_attributes = self#attributes popen_attributes in
          self#record [("popen_lid", popen_lid); ("popen_override", popen_override); ("popen_loc", popen_loc); ("popen_attributes", popen_attributes)]
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'res  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_expr include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_expr.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_expr pincl_mod in
          let pincl_loc = self#location pincl_loc in
          let pincl_attributes = self#attributes pincl_attributes in
          self#record [("pincl_mod", pincl_mod); ("pincl_loc", pincl_loc); ("pincl_attributes", pincl_attributes)]
      method include_infos_module_type : Module_type.t Include_infos.t -> 'res  =
        fun include_infos ->
          let concrete =
            match Include_infos.to_concrete_module_type include_infos with
            | None -> failwith "include_infos"
            | Some n -> n
          in
          let { pincl_mod; pincl_loc; pincl_attributes } : Module_type.t Include_infos.concrete = concrete in
          let pincl_mod = self#module_type pincl_mod in
          let pincl_loc = self#location pincl_loc in
          let pincl_attributes = self#attributes pincl_attributes in
          self#record [("pincl_mod", pincl_mod); ("pincl_loc", pincl_loc); ("pincl_attributes", pincl_attributes)]
      method include_description : Include_description.t -> 'res  =
        fun include_description ->
          let concrete =
            match Include_description.to_concrete include_description with
            | None -> failwith "include_description"
            | Some n -> n
          in
          let concrete = self#include_infos_module_type concrete in
          concrete
      method include_declaration : Include_declaration.t -> 'res  =
        fun include_declaration ->
          let concrete =
            match Include_declaration.to_concrete include_declaration with
            | None -> failwith "include_declaration"
            | Some n -> n
          in
          let concrete = self#include_infos_module_expr concrete in
          concrete
      method with_constraint : With_constraint.t -> 'res  =
        fun with_constraint ->
          let concrete =
            match With_constraint.to_concrete with_constraint with
            | None -> failwith "with_constraint"
            | Some n -> n
          in
          match (concrete : With_constraint.concrete) with
          | Pwith_type (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#type_declaration x1 in
            self#constr "Pwith_type" [x0; x1]
          | Pwith_module (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#longident_loc x1 in
            self#constr "Pwith_module" [x0; x1]
          | Pwith_typesubst (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#type_declaration x1 in
            self#constr "Pwith_typesubst" [x0; x1]
          | Pwith_modsubst (x0, x1) ->
            let x0 = self#longident_loc x0 in
            let x1 = self#longident_loc x1 in
            self#constr "Pwith_modsubst" [x0; x1]
      method module_expr : Module_expr.t -> 'res  =
        fun module_expr ->
          let concrete =
            match Module_expr.to_concrete module_expr with
            | None -> failwith "module_expr"
            | Some n -> n
          in
          let { pmod_desc; pmod_loc; pmod_attributes } : Module_expr.concrete = concrete in
          let pmod_desc = self#module_expr_desc pmod_desc in
          let pmod_loc = self#location pmod_loc in
          let pmod_attributes = self#attributes pmod_attributes in
          self#record [("pmod_desc", pmod_desc); ("pmod_loc", pmod_loc); ("pmod_attributes", pmod_attributes)]
      method module_expr_desc : Module_expr_desc.t -> 'res  =
        fun module_expr_desc ->
          let concrete =
            match Module_expr_desc.to_concrete module_expr_desc with
            | None -> failwith "module_expr_desc"
            | Some n -> n
          in
          match (concrete : Module_expr_desc.concrete) with
          | Pmod_ident x0 ->
            let x0 = self#longident_loc x0 in
            self#constr "Pmod_ident" [x0]
          | Pmod_structure x0 ->
            let x0 = self#structure x0 in
            self#constr "Pmod_structure" [x0]
          | Pmod_functor (x0, x1, x2) ->
            let x0 = self#loc self#string x0 in
            let x1 = self#option self#module_type x1 in
            let x2 = self#module_expr x2 in
            self#constr "Pmod_functor" [x0; x1; x2]
          | Pmod_apply (x0, x1) ->
            let x0 = self#module_expr x0 in
            let x1 = self#module_expr x1 in
            self#constr "Pmod_apply" [x0; x1]
          | Pmod_constraint (x0, x1) ->
            let x0 = self#module_expr x0 in
            let x1 = self#module_type x1 in
            self#constr "Pmod_constraint" [x0; x1]
          | Pmod_unpack x0 ->
            let x0 = self#expression x0 in
            self#constr "Pmod_unpack" [x0]
          | Pmod_extension x0 ->
            let x0 = self#extension x0 in
            self#constr "Pmod_extension" [x0]
      method structure : Structure.t -> 'res  =
        fun structure ->
          let concrete =
            match Structure.to_concrete structure with
            | None -> failwith "structure"
            | Some n -> n
          in
          let concrete = self#list self#structure_item concrete in
          concrete
      method structure_item : Structure_item.t -> 'res  =
        fun structure_item ->
          let concrete =
            match Structure_item.to_concrete structure_item with
            | None -> failwith "structure_item"
            | Some n -> n
          in
          let { pstr_desc; pstr_loc } : Structure_item.concrete = concrete in
          let pstr_desc = self#structure_item_desc pstr_desc in
          let pstr_loc = self#location pstr_loc in
          self#record [("pstr_desc", pstr_desc); ("pstr_loc", pstr_loc)]
      method structure_item_desc : Structure_item_desc.t -> 'res  =
        fun structure_item_desc ->
          let concrete =
            match Structure_item_desc.to_concrete structure_item_desc with
            | None -> failwith "structure_item_desc"
            | Some n -> n
          in
          match (concrete : Structure_item_desc.concrete) with
          | Pstr_eval (x0, x1) ->
            let x0 = self#expression x0 in
            let x1 = self#attributes x1 in
            self#constr "Pstr_eval" [x0; x1]
          | Pstr_value (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#value_binding x1 in
            self#constr "Pstr_value" [x0; x1]
          | Pstr_primitive x0 ->
            let x0 = self#value_description x0 in
            self#constr "Pstr_primitive" [x0]
          | Pstr_type (x0, x1) ->
            let x0 = self#rec_flag x0 in
            let x1 = self#list self#type_declaration x1 in
            self#constr "Pstr_type" [x0; x1]
          | Pstr_typext x0 ->
            let x0 = self#type_extension x0 in
            self#constr "Pstr_typext" [x0]
          | Pstr_exception x0 ->
            let x0 = self#extension_constructor x0 in
            self#constr "Pstr_exception" [x0]
          | Pstr_module x0 ->
            let x0 = self#module_binding x0 in
            self#constr "Pstr_module" [x0]
          | Pstr_recmodule x0 ->
            let x0 = self#list self#module_binding x0 in
            self#constr "Pstr_recmodule" [x0]
          | Pstr_modtype x0 ->
            let x0 = self#module_type_declaration x0 in
            self#constr "Pstr_modtype" [x0]
          | Pstr_open x0 ->
            let x0 = self#open_description x0 in
            self#constr "Pstr_open" [x0]
          | Pstr_class x0 ->
            let x0 = self#list self#class_declaration x0 in
            self#constr "Pstr_class" [x0]
          | Pstr_class_type x0 ->
            let x0 = self#list self#class_type_declaration x0 in
            self#constr "Pstr_class_type" [x0]
          | Pstr_include x0 ->
            let x0 = self#include_declaration x0 in
            self#constr "Pstr_include" [x0]
          | Pstr_attribute x0 ->
            let x0 = self#attribute x0 in
            self#constr "Pstr_attribute" [x0]
          | Pstr_extension (x0, x1) ->
            let x0 = self#extension x0 in
            let x1 = self#attributes x1 in
            self#constr "Pstr_extension" [x0; x1]
      method value_binding : Value_binding.t -> 'res  =
        fun value_binding ->
          let concrete =
            match Value_binding.to_concrete value_binding with
            | None -> failwith "value_binding"
            | Some n -> n
          in
          let { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Value_binding.concrete = concrete in
          let pvb_pat = self#pattern pvb_pat in
          let pvb_expr = self#expression pvb_expr in
          let pvb_attributes = self#attributes pvb_attributes in
          let pvb_loc = self#location pvb_loc in
          self#record [("pvb_pat", pvb_pat); ("pvb_expr", pvb_expr); ("pvb_attributes", pvb_attributes); ("pvb_loc", pvb_loc)]
      method module_binding : Module_binding.t -> 'res  =
        fun module_binding ->
          let concrete =
            match Module_binding.to_concrete module_binding with
            | None -> failwith "module_binding"
            | Some n -> n
          in
          let { pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Module_binding.concrete = concrete in
          let pmb_name = self#loc self#string pmb_name in
          let pmb_expr = self#module_expr pmb_expr in
          let pmb_attributes = self#attributes pmb_attributes in
          let pmb_loc = self#location pmb_loc in
          self#record [("pmb_name", pmb_name); ("pmb_expr", pmb_expr); ("pmb_attributes", pmb_attributes); ("pmb_loc", pmb_loc)]
      method toplevel_phrase : Toplevel_phrase.t -> 'res  =
        fun toplevel_phrase ->
          let concrete =
            match Toplevel_phrase.to_concrete toplevel_phrase with
            | None -> failwith "toplevel_phrase"
            | Some n -> n
          in
          match (concrete : Toplevel_phrase.concrete) with
          | Ptop_def x0 ->
            let x0 = self#structure x0 in
            self#constr "Ptop_def" [x0]
          | Ptop_dir (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#directive_argument x1 in
            self#constr "Ptop_dir" [x0; x1]
      method directive_argument : Directive_argument.t -> 'res  =
        fun directive_argument ->
          let concrete =
            match Directive_argument.to_concrete directive_argument with
            | None -> failwith "directive_argument"
            | Some n -> n
          in
          match (concrete : Directive_argument.concrete) with
          | Pdir_none ->
            self#constr "Pdir_none" []
          | Pdir_string x0 ->
            let x0 = self#string x0 in
            self#constr "Pdir_string" [x0]
          | Pdir_int (x0, x1) ->
            let x0 = self#string x0 in
            let x1 = self#option self#char x1 in
            self#constr "Pdir_int" [x0; x1]
          | Pdir_ident x0 ->
            let x0 = self#longident x0 in
            self#constr "Pdir_ident" [x0]
          | Pdir_bool x0 ->
            let x0 = self#bool x0 in
            self#constr "Pdir_bool" [x0]
    end
end
(*$*)
