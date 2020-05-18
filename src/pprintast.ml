(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Thomas Gazagnaire, OCamlPro                       *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*               Hongbo Zhang, University of Pennsylvania                 *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Original Code from Ber-metaocaml, modified for 3.12.0 and fixed *)
(* Printing code expressions *)
(* Authors:  Ed Pizzi, Fabrice Le Fessant *)
(* Extensive Rewrite: Hongbo Zhang: University of Pennsylvania *)
(* TODO more fine-grained precedence pretty-printing *)

open Import
open Current_ast
open Format
open Loc

(* make sure we don't accidentally use polymorphic compare on versioned ASTs *)
open! Int.Infix

let prefix_symbols  = [ '!'; '?'; '~' ] ;;
let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/';
                      '$'; '%'; '#' ]

(* type fixity = Infix| Prefix  *)
let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::" ]

(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let fixity_of_string  = function
  | s when List.mem s ~set:special_infix_strings -> `Infix s
  | s when List.mem s.[0] ~set:infix_symbols -> `Infix s
  | s when List.mem s.[0] ~set:prefix_symbols -> `Prefix s
  | s when Char.equal s.[0] '.' -> `Mixfix s
  | _ -> `Normal

let view_fixity_of_exp = function%view
  | { pexp_desc = Pexp_ident (Longident_loc { txt = Lident l; _ })
    ; pexp_attributes = Attributes []
    } ->
      fixity_of_string l
  | _ -> `Normal

let is_infix  = function  | `Infix _ -> true | _  -> false
let is_mixfix = function `Mixfix _ -> true | _ -> false

(* which identifiers are in fact operators needing parentheses *)
let needs_parens txt =
  let fix = fixity_of_string txt in
  is_infix fix
  || is_mixfix fix
  || List.mem txt.[0] ~set:prefix_symbols

(* some infixes need spaces around parens to avoid clashes with comment
   syntax *)
let needs_spaces txt =
  Char.equal txt.[0] '*'
  || Char.equal txt.[String.length txt - 1] '*'

(* add parentheses to binders when they are in fact infix or prefix operators *)
let protect_ident ppf txt =
  let format : (_, _, _) format =
    if not (needs_parens txt) then "%s"
    else if needs_spaces txt then "(@;%s@;)"
    else "(%s)"
  in fprintf ppf format txt

let protect_longident ppf print_longident longprefix txt =
  let format : (_, _, _) format =
    if not (needs_parens txt) then "%a.%s"
    else if needs_spaces txt then  "%a.(@;%s@;)"
    else "%a.(%s)" in
  fprintf ppf format print_longident longprefix txt

let varify_constructors var_names t =
  let check_variable vl loc v =
    if List.mem v ~set:vl then
      raise Ocaml_common.Syntaxerr.(Error(Variable_in_scope(loc,v)))
  in
  let var_names = List.map ~f:(fun v -> v.txt) var_names in
  let rec loop t =
    let ptyp_desc =
      match%view Core_type.ptyp_desc t with
      | Ptyp_any -> Core_type_desc.ptyp_any
      | Ptyp_var x ->
        check_variable var_names (Core_type.ptyp_loc t) x;
        Core_type_desc.ptyp_var x
      | Ptyp_arrow (label,core_type,core_type') ->
        Core_type_desc.ptyp_arrow label (loop core_type) (loop core_type')
      | Ptyp_tuple lst -> Core_type_desc.ptyp_tuple (List.map ~f:loop lst)
      | Ptyp_constr(Longident_loc { txt = Lident s }, [])
        when List.mem s ~set:var_names ->
        Core_type_desc.ptyp_var s
      | Ptyp_constr(longident, lst) ->
        Core_type_desc.ptyp_constr longident (List.map ~f:loop lst)
      | Ptyp_object (lst, o) ->
        Core_type_desc.ptyp_object (List.map ~f:loop_object_field lst) o
      | Ptyp_class (longident, lst) ->
        Core_type_desc.ptyp_class longident (List.map ~f:loop lst)
      | Ptyp_alias(core_type, string) ->
        check_variable var_names (Core_type.ptyp_loc t) string;
        Core_type_desc.ptyp_alias (loop core_type) string
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
        Core_type_desc.ptyp_variant
          (List.map ~f:loop_row_field row_field_list)
          flag lbl_lst_option
      | Ptyp_poly(string_lst, core_type) ->
        List.iter ~f:(fun v ->
          check_variable var_names (Core_type.ptyp_loc t) v.txt) string_lst;
        Core_type_desc.ptyp_poly string_lst (loop core_type)
      | Ptyp_package (Package_type (longident,lst)) ->
        Core_type_desc.ptyp_package
          (Package_type.create
             (longident, List.map ~f:(fun (n,typ) -> (n,loop typ)) lst))
      | Ptyp_extension ext ->
        Core_type_desc.ptyp_extension ext
    in
    Core_type.update t ~ptyp_desc
  and loop_row_field x =
    match%view x with
    | Rtag (label,attrs,flag,lst) ->
      Row_field.rtag label attrs flag (List.map ~f:loop lst)
    | Rinherit t ->
      Row_field.rinherit (loop t)
  and loop_object_field x =
    match%view x with
    | Otag(label, attrs, t) ->
      Object_field.otag label attrs (loop t)
    | Oinherit t ->
      Object_field.oinherit (loop t)
  in
  loop t

type space_formatter = (unit, Format.formatter, unit) format

let override = function%view
  | Override -> "!"
  | Fresh -> ""

(* variance encoding: need to sync up with the [parser.mly] *)
let type_variance = function%view
  | Invariant -> ""
  | Covariant -> "+"
  | Contravariant -> "-"

type construct =
  [ `cons of expression list
  | `list of expression list
  | `nil
  | `normal
  | `simple of Longident.t
  | `tuple ]

let view_expr x =
  match%view Expression.pexp_desc x with
  | Pexp_construct (Longident_loc {txt= Lident "()"; _},_) -> `tuple
  | Pexp_construct (Longident_loc {txt= Lident "[]";_},_) -> `nil
  | Pexp_construct (Longident_loc {txt= Lident"::";_},Some _) ->
    let rec loop exp acc =
      match%view exp with
      | {pexp_desc=Pexp_construct (Longident_loc {txt=Lident "[]";_},_);
         pexp_attributes = Attributes []} ->
        (List.rev acc,true)
      | {pexp_desc=
           Pexp_construct (Longident_loc {txt=Lident "::";_},
                           Some ({pexp_desc= Pexp_tuple([e1;e2]);
                                  pexp_attributes = Attributes []}));
         pexp_attributes = Attributes []}
        ->
        loop e2 (e1::acc)
      | e -> (List.rev (e::acc),false) in
    let (ls,b) = loop x []  in
    if b then
      `list ls
    else `cons ls
  | Pexp_construct (Longident_loc x,None) -> `simple (x.txt)
  | _ -> `normal

let is_simple_construct :construct -> bool = function
  | `nil | `tuple | `list _ | `simple _  -> true
  | `cons _ | `normal -> false

let pp = fprintf

type ctxt = {
  pipe : bool;
  semi : bool;
  ifthenelse : bool;
}

let reset_ctxt = { pipe=false; semi=false; ifthenelse=false }
let under_pipe ctxt = { ctxt with pipe=true }
let under_semi ctxt = { ctxt with semi=true }
let under_ifthenelse ctxt = { ctxt with ifthenelse=true }
(*
let reset_semi ctxt = { ctxt with semi=false }
let reset_ifthenelse ctxt = { ctxt with ifthenelse=false }
let reset_pipe ctxt = { ctxt with pipe=false }
*)

let list : 'a . ?sep:space_formatter -> ?first:space_formatter ->
  ?last:space_formatter -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit
  = fun ?sep ?first ?last fu f xs ->
    let first = match first with Some x -> x |None -> ("": _ format6)
    and last = match last with Some x -> x |None -> ("": _ format6)
    and sep = match sep with Some x -> x |None -> ("@ ": _ format6) in
    let aux f = function
      | [] -> ()
      | [x] -> fu f x
      | xs ->
          let rec loop  f = function
            | [x] -> fu f x
            | x::xs ->  fu f x; pp f sep; loop f xs;
            | _ -> assert false in begin
            pp f first; loop f xs; pp f last;
          end in
    aux f xs

let option : 'a. ?first:space_formatter -> ?last:space_formatter ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
  = fun  ?first  ?last fu f a ->
    let first = match first with Some x -> x | None -> ("": _ format6)
    and last = match last with Some x -> x | None -> ("": _ format6) in
    match a with
    | None -> ()
    | Some x -> pp f first; fu f x; pp f last

let paren: 'a . ?first:space_formatter -> ?last:space_formatter ->
  bool -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  = fun  ?(first=("": _ format6)) ?(last=("": _ format6)) b fu f x ->
    if b then (pp f "("; pp f first; fu f x; pp f last; pp f ")")
    else fu f x

let rec longident f = function%view
  | Lident s -> protect_ident f s
  | Ldot(y,s) -> protect_longident f longident y s
  | Lapply (y,s) ->
      pp f "%a(%a)" longident y longident s

let longident_loc f x =
  pp f "%a" longident (Longident_loc.to_concrete x).txt

let constant f = function%view
  | Pconst_char i -> pp f "%C"  i
  | Pconst_string (i, None) -> pp f "%S" i
  | Pconst_string (i, Some delim) -> pp f "{%s|%s|%s}" delim i delim
  | Pconst_integer (i, None) -> paren (Char.equal i.[0] '-') (fun f -> pp f "%s") f i
  | Pconst_integer (i, Some m) ->
    paren (Char.equal i.[0] '-') (fun f (i, m) -> pp f "%s%c" i m) f (i,m)
  | Pconst_float (i, None) -> paren (Char.equal i.[0] '-') (fun f -> pp f "%s") f i
  | Pconst_float (i, Some m) -> paren (Char.equal i.[0] '-') (fun f (i,m) ->
      pp f "%s%c" i m) f (i,m)

(* trailing space*)
let mutable_flag f = function%view
  | Immutable -> ()
  | Mutable -> pp f "mutable@;"
let virtual_flag f  = function%view
  | Concrete -> ()
  | Virtual -> pp f "virtual@;"

(* trailing space added *)
let rec_flag f rf =
  match%view rf with
  | Nonrecursive -> ()
  | Recursive -> pp f "rec "
let nonrec_flag f rf =
  match%view rf with
  | Nonrecursive -> pp f "nonrec "
  | Recursive -> ()
let direction_flag f = function%view
  | Upto -> pp f "to@ "
  | Downto -> pp f "downto@ "
let private_flag f = function%view
  | Public -> ()
  | Private -> pp f "private@ "

let constant_string f s = pp f "%S" s
let tyvar f str = pp f "'%s" str
let tyvar_loc f str = pp f "'%s" str.txt
let string_quot f x = pp f "`%s" x

let has_attributes attrs = not (List.is_empty (Attributes.to_concrete attrs))

(* c ['a,'b] *)
let rec class_params_def ctxt f =  function
  | [] -> ()
  | l ->
      pp f "[%a] " (* space *)
        (list (type_param ctxt) ~sep:",") l

and type_with_label ctxt f (label, c) =
  match%view label with
  | Nolabel    -> core_type1 ctxt f c (* otherwise parenthesize *)
  | Labelled s -> pp f "%s:%a" s (core_type1 ctxt) c
  | Optional s -> pp f "?%s:%a" s (core_type1 ctxt) c

and core_type ctxt f x =
  if has_attributes (Core_type.ptyp_attributes x)
  then begin
    pp f "((%a)%a)" (core_type ctxt)
      (Core_type.update x ~ptyp_attributes:(Attributes.create []))
      (attributes ctxt)
      (Core_type.ptyp_attributes x)
  end
  else
    match%view Core_type.ptyp_desc x with
    | Ptyp_arrow (l, ct1, ct2) ->
      pp f "@[<2>%a@;->@;%a@]" (* FIXME remove parens later *)
        (type_with_label ctxt) (l,ct1) (core_type ctxt) ct2
    | Ptyp_alias (ct, s) ->
      pp f "@[<2>%a@;as@;'%s@]" (core_type1 ctxt) ct s
    (* Intentionally left out, as part of the fix for PR#7344
       (commit c15ad73e56c54663c9cb6f7cac4241bb3c4e3cb8)
       | Ptyp_poly ([], ct) ->
       core_type ctxt f ct
    *)
    | Ptyp_poly (sl, ct) ->
      pp f "@[<2>%a%a@]"
        (fun f l ->
           pp f "%a"
             (fun f l -> match l with
                | [] -> ()
                | _ ->
                  pp f "%a@;.@;"
                    (list tyvar_loc ~sep:"@;")  l)
             l)
        sl (core_type ctxt) ct
    | _ -> pp f "@[<2>%a@]" (core_type1 ctxt) x

and core_type1 ctxt f x =
  if has_attributes (Core_type.ptyp_attributes x)
  then core_type ctxt f x
  else
    match%view Core_type.ptyp_desc x with
    | Ptyp_any -> pp f "_";
    | Ptyp_var s -> tyvar f  s;
    | Ptyp_tuple l ->  pp f "(%a)" (list (core_type1 ctxt) ~sep:"@;*@;") l
    | Ptyp_constr (li, l) ->
        pp f (* "%a%a@;" *) "%a%a"
          (fun f l ->
             match l with
             | [] -> ()
             | [x] -> pp f "%a@;" (core_type1 ctxt)  x
             | _ -> list ~first:"(" ~last:")@;" (core_type ctxt) ~sep:",@;" f l)
          l longident_loc li
    | Ptyp_variant (l, closed, low) ->
        let type_variant_helper f x =
          match%view x with
          | Rtag (l, attrs, _, ctl) ->
              pp f "@[<2>%a%a@;%a@]" string_quot l.txt
                (fun f l -> match l with
                   |[] -> ()
                   | _ -> pp f "@;of@;%a"
                            (list (core_type ctxt) ~sep:"&")  ctl) ctl
                (attributes ctxt) attrs
          | Rinherit ct -> core_type ctxt f ct in
        pp f "@[<2>[%a%a]@]"
          (fun f l ->
             match%view l, closed with
             | [], Closed -> ()
             | [], Open -> pp f ">" (* Cf #7200: print [>] correctly *)
             | _ ->
                 pp f "%s@;%a"
                   (match%view (closed,low) with
                    | (Closed,None) -> ""
                    | (Closed,Some _) -> "<" (* FIXME desugar the syntax sugar*)
                    | (Open,_) -> ">")
                   (list type_variant_helper ~sep:"@;<1 -2>| ") l) l
          (fun f low -> match low with
             | Some [] | None -> ()
             | Some xs ->
                 pp f ">@ %a"
                   (list string_quot) xs)
          low
    | Ptyp_object (l, o) ->
        let core_field_type f = function%view
          | Otag (l, attrs, ct) ->
            pp f "@[<hov2>%s: %a@ %a@ @]" l.txt
              (core_type ctxt) ct (attributes ctxt) attrs (* Cf #7200 *)
          | Oinherit ct ->
            pp f "@[<hov2>%a@ @]" (core_type ctxt) ct
        in
        let field_var f = function%view
          | Closed -> ()
          | Open ->
              match l with
              | [] -> pp f ".."
              | _ -> pp f " ;.."
        in
        pp f "@[<hov2><@ %a%a@ > @]" (list core_field_type ~sep:";") l
          field_var o (* Cf #7200 *)
    | Ptyp_class (li, l) ->   (*FIXME*)
        pp f "@[<hov2>%a#%a@]"
          (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")") l
          longident_loc li
    | Ptyp_package (Package_type (lid, cstrs)) ->
        let aux f (s, ct) =
          pp f "type %a@ =@ %a" longident_loc s (core_type ctxt) ct
        in
        (match cstrs with
         |[] -> pp f "@[<hov2>(module@ %a)@]" longident_loc lid
         |_ ->
             pp f "@[<hov2>(module@ %a@ with@ %a)@]" longident_loc lid
               (list aux  ~sep:"@ and@ ")  cstrs)
    | Ptyp_extension e -> extension ctxt f e
    | _ -> paren true (core_type ctxt) f x

(********************pattern********************)
(* be cautious when use [pattern], [pattern1] is preferred *)
and pattern ctxt f x =
  let rec list_of_pattern acc = function%view (* only consider ((A|B)|C)*)
    | {ppat_desc= Ppat_or (p1,p2); ppat_attributes = Attributes []} ->
        list_of_pattern  (p2::acc) p1
    | x -> x::acc
  in
  if has_attributes (Pattern.ppat_attributes x)
  then begin
    pp f "((%a)%a)" (pattern ctxt)
      (Pattern.update x ~ppat_attributes:(Attributes.create []))
      (attributes ctxt)
      (Pattern.ppat_attributes x)
  end
  else
    match%view Pattern.ppat_desc x with
    | Ppat_alias (p, s) ->
        pp f "@[<2>%a@;as@;%a@]" (pattern ctxt) p protect_ident s.txt (* RA*)
    | Ppat_or _ -> (* *)
        pp f "@[<hov0>%a@]" (list ~sep:"@,|" (pattern ctxt))
          (list_of_pattern [] x)
    | _ -> pattern1 ctxt f x

and pattern1 ctxt (f:Format.formatter) (x:pattern) : unit =
  let rec pattern_list_helper f = function%view
    | {ppat_desc =
         Ppat_construct
           (Longident_loc { txt = Lident("::") ;_},
            Some ({ppat_desc = Ppat_tuple([pat1; pat2]);_}));
       ppat_attributes = Attributes []}

      ->
        pp f "%a::%a" (simple_pattern ctxt) pat1 pattern_list_helper pat2 (*RA*)
    | p -> pattern1 ctxt f p
  in
  if has_attributes (Pattern.ppat_attributes x)
  then pattern ctxt f x
  else
    match%view Pattern.ppat_desc x with
    | Ppat_variant (l, Some p) ->
        pp f "@[<2>`%s@;%a@]" l (simple_pattern ctxt) p
    | Ppat_construct (Longident_loc {txt=Lident("()"|"[]");_}, _) ->
      simple_pattern ctxt f x
    | Ppat_construct (Longident_loc {txt;_} as li, po) ->
        (* FIXME The third field always false *)
      (match%view txt with
       | Lident "::" -> pp f "%a" pattern_list_helper x
       | _ ->
         (match po with
          | Some x -> pp f "%a@;%a"  longident_loc li (simple_pattern ctxt) x
          | None -> pp f "%a" longident_loc li))
    | _ -> simple_pattern ctxt f x

and simple_pattern ctxt (f:Format.formatter) (x:pattern) : unit =
  if has_attributes (Pattern.ppat_attributes x)
  then pattern ctxt f x
  else
    match%view Pattern.ppat_desc x with
    | Ppat_construct (Longident_loc {txt=Lident ("()"|"[]" as x);_}, _) -> pp f  "%s" x
    | Ppat_any -> pp f "_";
    | Ppat_var ({txt = txt;_}) -> protect_ident f txt
    | Ppat_array l ->
        pp f "@[<2>[|%a|]@]"  (list (pattern1 ctxt) ~sep:";") l
    | Ppat_unpack (s) ->
        pp f "(module@ %s)@ " s.txt
    | Ppat_type (li) ->
        pp f "#%a" longident_loc li
    | Ppat_record (l, closed) ->
        let longident_x_pattern f (li, p) =
          match%view (li,p) with
          | (Longident_loc {txt=Lident s;_ },
             {ppat_desc=Ppat_var {txt;_};
              ppat_attributes=Attributes[]; _})
            when String.equal s txt ->
              pp f "@[<2>%a@]"  longident_loc li
          | (li, _) ->
              pp f "@[<2>%a@;=@;%a@]" longident_loc li (pattern1 ctxt) p
        in
        begin
          match%view closed with
          | Closed ->
            pp f "@[<2>{@;%a@;}@]" (list longident_x_pattern ~sep:";@;") l
          | _ ->
            pp f "@[<2>{@;%a;_}@]" (list longident_x_pattern ~sep:";@;") l
        end
    | Ppat_tuple l ->
        pp f "@[<1>(%a)@]" (list  ~sep:",@;" (pattern1 ctxt))  l (* level1*)
    | Ppat_constant (c) -> pp f "%a" constant c
    | Ppat_interval (c1, c2) -> pp f "%a..%a" constant c1 constant c2
    | Ppat_variant (l,None) ->  pp f "`%s" l
    | Ppat_constraint (p, ct) ->
        pp f "@[<2>(%a@;:@;%a)@]" (pattern1 ctxt) p (core_type ctxt) ct
    | Ppat_lazy p ->
        pp f "@[<2>(lazy@;%a)@]" (pattern1 ctxt) p
    | Ppat_exception p ->
        pp f "@[<2>exception@;%a@]" (pattern1 ctxt) p
    | Ppat_extension e -> extension ctxt f e
    | Ppat_open (lid, p) ->
      let with_paren =
        match%view Pattern.ppat_desc p with
        | Ppat_array _ | Ppat_record _
        | Ppat_construct (Longident_loc {txt=Lident ("()"|"[]");_}, _) -> false
        | _ -> true
      in
      pp f "@[<2>%a.%a @]" longident_loc lid
          (paren with_paren @@ pattern1 ctxt) p
    | _ -> paren true (pattern ctxt) f x

and label_exp ctxt f (l,opt,p) =
  match%view l with
  | Nolabel ->
    (* single case pattern parens needed here *)
    pp f "%a@ " (simple_pattern ctxt) p
  | Optional rest ->
    begin
      match%view p with
      | {ppat_desc = Ppat_var {txt;_}; ppat_attributes = Attributes []}
        when String.equal txt rest ->
        (match opt with
         | Some o -> pp f "?(%s=@;%a)@;" rest  (expression ctxt) o
         | None -> pp f "?%s@ " rest)
      | _ ->
        (match opt with
         | Some o ->
           pp f "?%s:(%a=@;%a)@;"
             rest (pattern1 ctxt) p (expression ctxt) o
         | None -> pp f "?%s:%a@;" rest (simple_pattern ctxt) p)
    end
  | Labelled l ->
    begin
      match%view p with
      | {ppat_desc  = Ppat_var {txt;_}; ppat_attributes = Attributes []}
        when String.equal txt l ->
        pp f "~%s@;" l
      | _ ->  pp f "~%s:%a@;" l (simple_pattern ctxt) p
    end

and sugar_expr ctxt f e =
  if has_attributes (Expression.pexp_attributes e)
  then false
  else begin
    match%view Expression.pexp_desc e with
    | Pexp_apply
        ( { pexp_desc = Pexp_ident (Longident_loc {txt = id; _})
          ; pexp_attributes = Attributes []
          ; _ }
        , args )
      when List.for_all ~f:(function%view (Nolabel, _) -> true | _ -> false) args ->
      begin
        let print_indexop a path_prefix assign left right print_index indices
              rem_args =
          let print_path ppf = function
            | None -> ()
            | Some m -> pp ppf ".%a" longident m in
          match assign, rem_args with
          | false, [] ->
            pp f "@[%a%a%s%a%s@]"
              (simple_expr ctxt) a print_path path_prefix
              left (list ~sep:"," print_index) indices right; true
          | true, [v] ->
            pp f "@[%a%a%s%a%s@ <-@;<1 2>%a@]"
              (simple_expr ctxt) a print_path path_prefix
              left (list ~sep:"," print_index) indices right
              (simple_expr ctxt) v; true
          | _ -> false
        in
        match%view id, List.map ~f:snd args with
        | Lident "!", [e] ->
          pp f "@[<hov>!%a@]" (simple_expr ctxt) e; true
        | Ldot (path, ("get"|"set" as func)), a :: other_args -> begin
            let assign = String.equal func "set" in
            let print = print_indexop a None assign in
            match%view path, other_args with
            | Lident "Array", i :: rest ->
              print ".(" ")" (expression ctxt) [i] rest
            | Lident "String", i :: rest ->
              print ".[" "]" (expression ctxt) [i] rest
            | Ldot (Lident "Bigarray", "Array1"), i1 :: rest ->
              print ".{" "}" (simple_expr ctxt) [i1] rest
            | Ldot (Lident "Bigarray", "Array2"), i1 :: i2 :: rest ->
              print ".{" "}" (simple_expr ctxt) [i1; i2] rest
            | Ldot (Lident "Bigarray", "Array3"), i1 :: i2 :: i3 :: rest ->
              print ".{" "}" (simple_expr ctxt) [i1; i2; i3] rest
            | Ldot (Lident "Bigarray", "Genarray"),
              {pexp_desc = Pexp_array indexes; pexp_attributes = Attributes []} :: rest ->
              print ".{" "}" (simple_expr ctxt) indexes rest
            | _ -> false
          end
        | (Lident s | Ldot(_,s)) , a :: i :: rest
          when Char.equal s.[0] '.' ->
          let n = String.length s in
          (* extract operator:
             assignment operators end with [right_bracket ^ "<-"],
             access operators end with [right_bracket] directly
          *)
          let assign = Char.equal s.[n - 1] '-'  in
          let kind =
            (* extract the right end bracket *)
            if assign then s.[n - 3] else s.[n - 1] in
          let left, right =
            match kind with
            | ')' -> '(', ")"
            | ']' -> '[', "]"
            | '}' -> '{', "}"
            | _ -> assert false in
          let path_prefix =
            match%view id with
            | Ldot(m,_) -> Some m
            | _ -> None
          in
          let left =
            String.sub s ~pos:0 ~len:(1 + Option.value_exn (String.index s left))
          in
          print_indexop a path_prefix assign left right
            (expression ctxt) [i] rest
        | _ -> false
      end
    | _ -> false
  end

and expression ctxt f x =
  if has_attributes (Expression.pexp_attributes x)
  then
    pp f "((%a)@,%a)" (expression ctxt)
      (Expression.update x ~pexp_attributes:(Attributes.create []))
      (attributes ctxt)
      (Expression.pexp_attributes x)
  else
    match%view Expression.pexp_desc x with
    | Pexp_function _ | Pexp_fun _ | Pexp_match _ | Pexp_try _ | Pexp_sequence _
      when ctxt.pipe || ctxt.semi ->
      paren true (expression reset_ctxt) f x
    | Pexp_ifthenelse _ | Pexp_sequence _ when ctxt.ifthenelse ->
      paren true (expression reset_ctxt) f x
    | Pexp_let _ | Pexp_letmodule _ | Pexp_open _ | Pexp_letexception _
      when ctxt.semi ->
      paren true (expression reset_ctxt) f x
    | Pexp_fun (l, e0, p, e) ->
      pp f "@[<2>fun@;%a->@;%a@]"
        (label_exp ctxt) (l, e0, p)
        (expression ctxt) e
    | Pexp_function l ->
      pp f "@[<hv>function%a@]" (case_list ctxt) l
    | Pexp_match (e, l) ->
      pp f "@[<hv0>@[<hv0>@[<2>match %a@]@ with@]%a@]"
        (expression reset_ctxt) e (case_list ctxt) l
    | Pexp_try (e, l) ->
      pp f "@[<0>@[<hv2>try@ %a@]@ @[<0>with%a@]@]"
        (* "try@;@[<2>%a@]@\nwith@\n%a"*)
        (expression reset_ctxt) e  (case_list ctxt) l
    | Pexp_let (rf, l, e) ->
      (* pp f "@[<2>let %a%a in@;<1 -2>%a@]"
         (*no indentation here, a new line*) *)
      (*   rec_flag rf *)
      pp f "@[<2>%a in@;<1 -2>%a@]"
        (bindings reset_ctxt) (rf,l)
        (expression ctxt) e
    | Pexp_apply (e, l) ->
      begin
        if not (sugar_expr ctxt f x)
        then
          match view_fixity_of_exp e with
          | `Infix s ->
            begin
              match%view l with
              | [ (Nolabel, _) as arg1; (Nolabel, _) as arg2 ] ->
                (* FIXME associativity label_x_expression_param *)
                pp f "@[<2>%a@;%s@;%a@]"
                  (label_x_expression_param reset_ctxt) arg1 s
                  (label_x_expression_param ctxt) arg2
              | _ ->
                pp f "@[<2>%a %a@]"
                  (simple_expr ctxt) e
                  (list (label_x_expression_param ctxt)) l
            end
          | `Prefix s ->
            let s =
              if List.mem s ~set:["~+";"~-";"~+.";"~-."] &&
                 (match%view l with
                  (* See #7200: avoid turning (~- 1) into (- 1) which is
                     parsed as an int literal *)
                  |[(_,{pexp_desc=Pexp_constant _})] -> false
                  | _ -> true)
              then String.sub s ~pos:1 ~len:(String.length s -1)
              else s
            in
            begin
              match%view l with
              | [(Nolabel, x)] ->
                pp f "@[<2>%s@;%a@]" s (simple_expr ctxt) x
              | _   ->
                pp f "@[<2>%a %a@]" (simple_expr ctxt) e
                  (list (label_x_expression_param ctxt)) l
            end
          | _ ->
            pp f "@[<hov2>%a@]"
              begin
                fun f (e,l) ->
                  pp f "%a@ %a" (expression2 ctxt) e
                    (list (label_x_expression_param reset_ctxt))  l
                    (* reset here only because [function,match,try,sequence]
                       are lower priority *)
              end
              (e,l)
      end
    | Pexp_construct (li, Some eo)
      when not (is_simple_construct (view_expr x))-> (* Not efficient FIXME*)
      (match view_expr x with
       | `cons ls -> list (simple_expr ctxt) f ls ~sep:"@;::@;"
       | `normal ->
         pp f "@[<2>%a@;%a@]" longident_loc li
           (simple_expr ctxt) eo
       | _ -> assert false)
    | Pexp_setfield (e1, li, e2) ->
      pp f "@[<2>%a.%a@ <-@ %a@]"
        (simple_expr ctxt) e1 longident_loc li (simple_expr ctxt) e2
    | Pexp_ifthenelse (e1, e2, eo) ->
      (* @;@[<2>else@ %a@]@] *)
      let fmt:(_,_,_)format ="@[<hv0>@[<2>if@ %a@]@;@[<2>then@ %a@]%a@]" in
      let expression_under_ifthenelse = expression (under_ifthenelse ctxt) in
      pp f fmt expression_under_ifthenelse e1 expression_under_ifthenelse e2
        (fun f eo -> match eo with
           | Some x ->
             pp f "@;@[<2>else@;%a@]" (expression (under_semi ctxt)) x
           | None -> () (* pp f "()" *)) eo
    | Pexp_sequence _ ->
      let rec sequence_helper acc = function%view
        | {pexp_desc=Pexp_sequence(e1,e2); pexp_attributes = Attributes []} ->
          sequence_helper (e1::acc) e2
        | v -> List.rev (v::acc)
      in
      let lst = sequence_helper [] x in
      pp f "@[<hv>%a@]"
        (list (expression (under_semi ctxt)) ~sep:";@;") lst
    | Pexp_new (li) ->
      pp f "@[<hov2>new@ %a@]" longident_loc li;
    | Pexp_setinstvar (s, e) ->
      pp f "@[<hov2>%s@ <-@ %a@]" s.txt (expression ctxt) e
    | Pexp_override l -> (* FIXME *)
      let string_x_expression f (s, e) =
        pp f "@[<hov2>%s@ =@ %a@]" s.txt (expression ctxt) e in
      pp f "@[<hov2>{<%a>}@]"
        (list string_x_expression  ~sep:";"  )  l;
    | Pexp_letmodule (s, me, e) ->
      pp f "@[<hov2>let@ module@ %s@ =@ %a@ in@ %a@]" s.txt
        (module_expr reset_ctxt) me (expression ctxt) e
    | Pexp_letexception (cd, e) ->
      pp f "@[<hov2>let@ exception@ %a@ in@ %a@]"
        (extension_constructor ctxt) cd
        (expression ctxt) e
    | Pexp_assert e ->
      pp f "@[<hov2>assert@ %a@]" (simple_expr ctxt) e
    | Pexp_lazy (e) ->
      pp f "@[<hov2>lazy@ %a@]" (simple_expr ctxt) e
    (* Pexp_poly: impossible but we should print it anyway, rather than
       assert false *)
    | Pexp_poly (e, None) ->
      pp f "@[<hov2>!poly!@ %a@]" (simple_expr ctxt) e
    | Pexp_poly (e, Some ct) ->
      pp f "@[<hov2>(!poly!@ %a@ : %a)@]"
        (simple_expr ctxt) e (core_type ctxt) ct
    | Pexp_open (ovf, lid, e) ->
      pp f "@[<2>let open%s %a in@;%a@]" (override ovf) longident_loc lid
        (expression ctxt) e
    | Pexp_variant (l,Some eo) ->
      pp f "@[<2>`%s@;%a@]" l (simple_expr ctxt) eo
    | Pexp_extension e -> extension ctxt f e
    | Pexp_unreachable -> pp f "."
    | _ -> expression1 ctxt f x

and expression1 ctxt f x =
  if has_attributes (Expression.pexp_attributes x)
  then expression ctxt f x
  else
    match%view Expression.pexp_desc x with
    | Pexp_object cs -> pp f "%a" (class_structure ctxt) cs
    | _ -> expression2 ctxt f x
(* used in [Pexp_apply] *)

and expression2 ctxt f x =
  if has_attributes (Expression.pexp_attributes x)
  then expression ctxt f x
  else
    match%view Expression.pexp_desc x with
    | Pexp_field (e, li) ->
      pp f "@[<hov2>%a.%a@]" (simple_expr ctxt) e longident_loc li
    | Pexp_send (e, s) -> pp f "@[<hov2>%a#%s@]" (simple_expr ctxt) e s.txt
    | _ -> simple_expr ctxt f x

and simple_expr ctxt f x =
  if has_attributes (Expression.pexp_attributes x)
  then expression ctxt f x
  else
    match%view Expression.pexp_desc x with
    | Pexp_construct _  when is_simple_construct (view_expr x) ->
        (match view_expr x with
         | `nil -> pp f "[]"
         | `tuple -> pp f "()"
         | `list xs ->
             pp f "@[<hv0>[%a]@]"
               (list (expression (under_semi ctxt)) ~sep:";@;") xs
         | `simple x -> longident f x
         | _ -> assert false)
    | Pexp_ident (li) ->
      longident_loc f li
    (* (match view_fixity_of_exp x with *)
    (* |`Normal -> longident_loc f li *)
    (* | `Prefix _ | `Infix _ -> pp f "( %a )" longident_loc li) *)
    | Pexp_constant c -> constant f c;
    | Pexp_pack me ->
        pp f "(module@;%a)" (module_expr ctxt) me
    | Pexp_newtype (lid, e) ->
        pp f "fun@;(type@;%s)@;->@;%a" lid.txt (expression ctxt) e
    | Pexp_tuple l ->
        pp f "@[<hov2>(%a)@]" (list (simple_expr ctxt) ~sep:",@;") l
    | Pexp_constraint (e, ct) ->
        pp f "(%a : %a)" (expression ctxt) e (core_type ctxt) ct
    | Pexp_coerce (e, cto1, ct) ->
        pp f "(%a%a :> %a)" (expression ctxt) e
          (option (core_type ctxt) ~first:" : " ~last:" ") cto1 (* no sep hint*)
          (core_type ctxt) ct
    | Pexp_variant (l, None) -> pp f "`%s" l
    | Pexp_record (l, eo) ->
        let longident_x_expression f (li, e) =
          match%view e with
          |  {pexp_desc=Pexp_ident (Longident_loc {txt;_});
              pexp_attributes=Attributes[]; _}
            when Longid.equal (Longident_loc.to_concrete li).txt txt ->
              pp f "@[<hov2>%a@]" longident_loc li
          | _ ->
              pp f "@[<hov2>%a@;=@;%a@]" longident_loc li (simple_expr ctxt) e
        in
        pp f "@[<hv0>@[<hv2>{@;%a%a@]@;}@]"(* "@[<hov2>{%a%a}@]" *)
          (option ~last:" with@;" (simple_expr ctxt)) eo
          (list longident_x_expression ~sep:";@;") l
    | Pexp_array (l) ->
        pp f "@[<0>@[<2>[|%a|]@]@]"
          (list (simple_expr (under_semi ctxt)) ~sep:";") l
    | Pexp_while (e1, e2) ->
        let fmt : (_,_,_) format = "@[<2>while@;%a@;do@;%a@;done@]" in
        pp f fmt (expression ctxt) e1 (expression ctxt) e2
    | Pexp_for (s, e1, e2, df, e3) ->
        let fmt:(_,_,_)format =
          "@[<hv0>@[<hv2>@[<2>for %a =@;%a@;%a%a@;do@]@;%a@]@;done@]" in
        let expression = expression ctxt in
        pp f fmt (pattern ctxt) s expression e1 direction_flag
          df expression e2 expression e3
    | _ ->  paren true (expression ctxt) f x

and attributes ctxt f l =
  List.iter ~f:(attribute ctxt f) (Attributes.to_concrete l)

and item_attributes ctxt f l =
  List.iter ~f:(item_attribute ctxt f) (Attributes.to_concrete l)

and attribute ctxt f a =
  let (s, e) = Attribute.to_concrete a in
  pp f "@[<2>[@@%s@ %a]@]" s.txt (payload ctxt) e

and item_attribute ctxt f a =
  let (s, e) = Attribute.to_concrete a in
  pp f "@[<2>[@@@@%s@ %a]@]" s.txt (payload ctxt) e

and floating_attribute ctxt f a =
  let (s, e) = Attribute.to_concrete a in
  pp f "@[<2>[@@@@@@%s@ %a]@]" s.txt (payload ctxt) e

and value_description ctxt f x =
  (* note: value_description has an attribute field,
           but they're already printed by the callers this method *)
  pp f "@[<hov2>%a%a@]" (core_type ctxt)
    (Value_description.pval_type x)
    (fun f x ->
       if not (List.is_empty (Value_description.pval_prim x))
       then pp f "@ =@ %a" (list constant_string) (Value_description.pval_prim x)
    ) x

and extension ctxt f x =
  let (s, e) = Extension.to_concrete x in
  pp f "@[<2>[%%%s@ %a]@]" s.txt (payload ctxt) e

and item_extension ctxt f x =
  let (s, e) = Extension.to_concrete x in
  pp f "@[<2>[%%%%%s@ %a]@]" s.txt (payload ctxt) e

and exception_declaration ctxt f ext =
  pp f "@[<hov2>exception@ %a@]" (extension_constructor ctxt) ext

and class_type_field ctxt f x =
    match%view Class_type_field.pctf_desc x with
    | Pctf_inherit (ct) ->
        pp f "@[<2>inherit@ %a@]%a" (class_type ctxt) ct
          (item_attributes ctxt) (Class_type_field.pctf_attributes x)
    | Pctf_val (s, mf, vf, ct) ->
        pp f "@[<2>val @ %a%a%s@ :@ %a@]%a"
          mutable_flag mf virtual_flag vf s.txt (core_type ctxt) ct
          (item_attributes ctxt) (Class_type_field.pctf_attributes x)
    | Pctf_method (s, pf, vf, ct) ->
        pp f "@[<2>method %a %a%s :@;%a@]%a"
          private_flag pf virtual_flag vf s.txt (core_type ctxt) ct
          (item_attributes ctxt) (Class_type_field.pctf_attributes x)
    | Pctf_constraint (ct1, ct2) ->
        pp f "@[<2>constraint@ %a@ =@ %a@]%a"
          (core_type ctxt) ct1 (core_type ctxt) ct2
          (item_attributes ctxt) (Class_type_field.pctf_attributes x)
    | Pctf_attribute a -> floating_attribute ctxt f a
    | Pctf_extension e ->
        item_extension ctxt f e;
        item_attributes ctxt f (Class_type_field.pctf_attributes x)

and class_signature ctxt f x =
  match%view x with
  | { pcsig_self = ct; pcsig_fields = l ;_} ->
    pp f "@[<hv0>@[<hv2>object@[<1>%a@]@ %a@]@ end@]"
      (fun f -> function%view
         | {ptyp_desc=Ptyp_any; ptyp_attributes=Attributes []; _} -> ()
         | ct -> pp f " (%a)" (core_type ctxt) ct) ct
      (list (class_type_field ctxt) ~sep:"@;") l

(* call [class_signature] called by [class_signature] *)
and class_type ctxt f x =
  match%view Class_type.pcty_desc x with
  | Pcty_signature cs ->
      class_signature ctxt f cs;
      attributes ctxt f (Class_type.pcty_attributes x)
  | Pcty_constr (li, l) ->
      pp f "%a%a%a"
        (fun f l -> match l with
           | [] -> ()
           | _  -> pp f "[%a]@ " (list (core_type ctxt) ~sep:"," ) l) l
        longident_loc li
        (attributes ctxt) (Class_type.pcty_attributes x)
  | Pcty_arrow (l, co, cl) ->
      pp f "@[<2>%a@;->@;%a@]" (* FIXME remove parens later *)
        (type_with_label ctxt) (l,co)
        (class_type ctxt) cl
  | Pcty_extension e ->
      extension ctxt f e;
      attributes ctxt f (Class_type.pcty_attributes x)
  | Pcty_open (ovf, lid, e) ->
      pp f "@[<2>let open%s %a in@;%a@]" (override ovf) longident_loc lid
        (class_type ctxt) e

(* [class type a = object end] *)
and class_type_declaration_list ctxt f l =
  let class_type_declaration kwd f x =
    let x = Class_type_declaration.to_concrete x in
    match%view x with
    | { pci_params=ls; pci_name={ txt; _ }; _ } ->
      pp f "@[<2>%s %a%a%s@ =@ %a@]%a" kwd
        virtual_flag (Class_infos.pci_virt x)
        (class_params_def ctxt) ls txt
        (class_type ctxt) (Class_infos.pci_expr x)
        (item_attributes ctxt) (Class_infos.pci_attributes x)
  in
  match l with
  | [] -> ()
  | [x] -> class_type_declaration "class type" f x
  | x :: xs ->
      pp f "@[<v>%a@,%a@]"
        (class_type_declaration "class type") x
        (list ~sep:"@," (class_type_declaration "and")) xs

and class_field ctxt f x =
  match%view Class_field.pcf_desc x with
  | Pcf_inherit (ovf, ce, so) ->
      pp f "@[<2>inherit@ %s@ %a%a@]%a" (override ovf)
        (class_expr ctxt) ce
        (fun f so -> match so with
           | None -> ();
           | Some (s) -> pp f "@ as %s" s.txt ) so
        (item_attributes ctxt) (Class_field.pcf_attributes x)
  | Pcf_val (s, mf, Cfk_concrete (ovf, e)) ->
      pp f "@[<2>val%s %a%s =@;%a@]%a" (override ovf)
        mutable_flag mf s.txt
        (expression ctxt) e
        (item_attributes ctxt) (Class_field.pcf_attributes x)
  | Pcf_method (s, pf, Cfk_virtual ct) ->
      pp f "@[<2>method virtual %a %s :@;%a@]%a"
        private_flag pf s.txt
        (core_type ctxt) ct
        (item_attributes ctxt) (Class_field.pcf_attributes x)
  | Pcf_val (s, mf, Cfk_virtual ct) ->
      pp f "@[<2>val virtual %a%s :@ %a@]%a"
        mutable_flag mf s.txt
        (core_type ctxt) ct
        (item_attributes ctxt) (Class_field.pcf_attributes x)
  | Pcf_method (s, pf, Cfk_concrete (ovf, e)) ->
      let bind e =
        binding ctxt f
          (Value_binding.create
             ~pvb_pat:
             (Pattern.create
                ~ppat_desc:(Pattern_desc.ppat_var s)
                ~ppat_loc:Location.none
                ~ppat_attributes:(Attributes.create []))
             ~pvb_expr:e
             ~pvb_attributes:(Attributes.create [])
             ~pvb_loc:Location.none)
      in
      pp f "@[<2>method%s %a%a@]%a"
        (override ovf)
        private_flag pf
        (fun f -> function%view
           | {pexp_desc=Pexp_poly (e, Some ct); pexp_attributes=Attributes []; _} ->
               pp f "%s :@;%a=@;%a"
                 s.txt (core_type ctxt) ct (expression ctxt) e
           | {pexp_desc=Pexp_poly (e, None); pexp_attributes=Attributes []; _} ->
               bind e
           | _ -> bind e) e
        (item_attributes ctxt) (Class_field.pcf_attributes x)
  | Pcf_constraint (ct1, ct2) ->
      pp f "@[<2>constraint %a =@;%a@]%a"
        (core_type ctxt) ct1
        (core_type ctxt) ct2
        (item_attributes ctxt) (Class_field.pcf_attributes x)
  | Pcf_initializer (e) ->
      pp f "@[<2>initializer@ %a@]%a"
        (expression ctxt) e
        (item_attributes ctxt) (Class_field.pcf_attributes x)
  | Pcf_attribute a -> floating_attribute ctxt f a
  | Pcf_extension e ->
      item_extension ctxt f e;
      item_attributes ctxt f (Class_field.pcf_attributes x)

and class_structure ctxt f x =
  match%view x with
  | { pcstr_self = p; pcstr_fields =  l } ->
    pp f "@[<hv0>@[<hv2>object%a@;%a@]@;end@]"
      (fun f p ->
         match%view Pattern.ppat_desc p with
         | Ppat_any -> ()
         | Ppat_constraint _ -> pp f " %a" (pattern ctxt) p
         | _ -> pp f " (%a)" (pattern ctxt) p)
      p
      (list (class_field ctxt)) l

and class_expr ctxt f x =
  if has_attributes (Class_expr.pcl_attributes x)
  then begin
    pp f "((%a)%a)" (class_expr ctxt)
      (Class_expr.update x ~pcl_attributes:(Attributes.create []))
      (attributes ctxt)
      (Class_expr.pcl_attributes x)
  end else
    match%view Class_expr.pcl_desc x with
    | Pcl_structure (cs) -> class_structure ctxt f cs
    | Pcl_fun (l, eo, p, e) ->
        pp f "fun@ %a@ ->@ %a"
          (label_exp ctxt) (l,eo,p)
          (class_expr ctxt) e
    | Pcl_let (rf, l, ce) ->
        pp f "%a@ in@ %a"
          (bindings ctxt) (rf,l)
          (class_expr ctxt) ce
    | Pcl_apply (ce, l) ->
        pp f "((%a)@ %a)" (* Cf: #7200 *)
          (class_expr ctxt) ce
          (list (label_x_expression_param ctxt)) l
    | Pcl_constr (li, l) ->
        pp f "%a%a"
          (fun f l->
             if not (List.is_empty l) then
               pp f "[%a]@ "
                 (list (core_type ctxt) ~sep:",") l) l
          longident_loc li
    | Pcl_constraint (ce, ct) ->
        pp f "(%a@ :@ %a)"
          (class_expr ctxt) ce
          (class_type ctxt) ct
    | Pcl_extension e -> extension ctxt f e
    | Pcl_open (ovf, lid, e) ->
        pp f "@[<2>let open%s %a in@;%a@]" (override ovf) longident_loc lid
          (class_expr ctxt) e

and module_type ctxt f x =
  if has_attributes (Module_type.pmty_attributes x)
  then begin
    pp f "((%a)%a)" (module_type ctxt)
      (Module_type.update x ~pmty_attributes:(Attributes.create []))
      (attributes ctxt)
      (Module_type.pmty_attributes x)
  end else
    match%view Module_type.pmty_desc x with
    | Pmty_functor (_, None, mt2) ->
        pp f "@[<hov2>functor () ->@ %a@]" (module_type ctxt) mt2
    | Pmty_functor (s, Some mt1, mt2) ->
        if String.equal s.txt "_" then
          pp f "@[<hov2>%a@ ->@ %a@]"
            (module_type1 ctxt) mt1 (module_type ctxt) mt2
        else
          pp f "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]" s.txt
            (module_type ctxt) mt1 (module_type ctxt) mt2
    | Pmty_with (mt, []) -> module_type ctxt f mt
    | Pmty_with (mt, l) ->
        let with_constraint f = function%view
          | Pwith_type (li, ({ptype_params= ls ;_} as td)) ->
              let ls = List.map ~f:fst ls in
              pp f "type@ %a %a =@ %a"
                (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")")
                ls longident_loc li (type_declaration ctxt) td
          | Pwith_module (li, li2) ->
              pp f "module %a =@ %a" longident_loc li longident_loc li2;
          | Pwith_typesubst (li, ({ptype_params=ls;_} as td)) ->
              let ls = List.map ~f:fst ls in
              pp f "type@ %a %a :=@ %a"
                (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")")
                ls longident_loc li
                (type_declaration ctxt) td
          | Pwith_modsubst (li, li2) ->
             pp f "module %a :=@ %a" longident_loc li longident_loc li2 in
        pp f "@[<hov2>%a@ with@ %a@]"
          (module_type1 ctxt) mt (list with_constraint ~sep:"@ and@ ") l
    | _ -> module_type1 ctxt f x

and module_type1 ctxt f x =
  if has_attributes (Module_type.pmty_attributes x)
  then module_type ctxt f x
  else
    match%view Module_type.pmty_desc x with
    | Pmty_ident li ->
        pp f "%a" longident_loc li;
    | Pmty_alias li ->
        pp f "(module %a)" longident_loc li;
    | Pmty_signature (Signature s) ->
        pp f "@[<hv0>@[<hv2>sig@ %a@]@ end@]" (* "@[<hov>sig@ %a@ end@]" *)
          (list (signature_item ctxt)) s (* FIXME wrong indentation*)
    | Pmty_typeof me ->
        pp f "@[<hov2>module@ type@ of@ %a@]" (module_expr ctxt) me
    | Pmty_extension e -> extension ctxt f e
    | _ -> paren true (module_type ctxt) f x

and signature ctxt f x =
  list ~sep:"@\n" (signature_item ctxt) f (Signature.to_concrete x)

and signature_item ctxt f x : unit =
  match%view Signature_item.psig_desc x with
  | Psig_type (rf, l) ->
    type_def_list ctxt f (rf, l)
  | Psig_value vd ->
    let intro =
      if List.is_empty (Value_description.pval_prim vd) then "val" else "external"
    in
    pp f "@[<2>%s@ %a@ :@ %a@]%a" intro
      protect_ident (Value_description.pval_name vd).txt
      (value_description ctxt) vd
      (item_attributes ctxt) (Value_description.pval_attributes vd)
  | Psig_typext te ->
    type_extension ctxt f te
  | Psig_exception ed ->
    exception_declaration ctxt f ed
  | Psig_class l ->
    let class_description kwd f x =
      let x = Class_description.to_concrete x in
      match%view x with
      | {pci_params=ls;pci_name={txt;_};_} ->
        pp f "@[<2>%s %a%a%s@;:@;%a@]%a" kwd
          virtual_flag (Class_infos.pci_virt x)
          (class_params_def ctxt) ls txt
          (class_type ctxt) (Class_infos.pci_expr x)
          (item_attributes ctxt) (Class_infos.pci_attributes x)
    in
    begin
      match l with
      | [] -> ()
      | [x] -> class_description "class" f x
      | x :: xs ->
        pp f "@[<v>%a@,%a@]"
          (class_description "class") x
          (list ~sep:"@," (class_description "and")) xs
    end
  | Psig_module ({pmd_type={pmty_desc=Pmty_alias alias;
                            pmty_attributes=Attributes[]; _};_} as pmd) ->
    pp f "@[<hov>module@ %s@ =@ %a@]%a" (Module_declaration.pmd_name pmd).txt
      longident_loc alias
      (item_attributes ctxt) (Module_declaration.pmd_attributes pmd)
  | Psig_module pmd ->
    pp f "@[<hov>module@ %s@ :@ %a@]%a"
      (Module_declaration.pmd_name pmd).txt
      (module_type ctxt) (Module_declaration.pmd_type pmd)
      (item_attributes ctxt) (Module_declaration.pmd_attributes pmd)
  | Psig_open od ->
    pp f "@[<hov2>open%s@ %a@]%a"
      (override (Open_description.popen_override od))
      longident_loc (Open_description.popen_lid od)
      (item_attributes ctxt) (Open_description.popen_attributes od)
  | Psig_include (Include_description incl) ->
    pp f "@[<hov2>include@ %a@]%a"
      (module_type ctxt) (Include_infos.pincl_mod incl)
      (item_attributes ctxt) (Include_infos.pincl_attributes incl)
  | Psig_modtype {pmtd_name=s; pmtd_type=md; pmtd_attributes=attrs} ->
    pp f "@[<hov2>module@ type@ %s%a@]%a"
      s.txt
      (fun f md -> match md with
         | None -> ()
         | Some mt ->
           pp_print_space f () ;
           pp f "@ =@ %a" (module_type ctxt) mt
      ) md
      (item_attributes ctxt) attrs
  | Psig_class_type (l) -> class_type_declaration_list ctxt f l
  | Psig_recmodule decls ->
    let rec  string_x_module_type_list f ?(first=true) l =
      match l with
      | [] -> () ;
      | pmd :: tl ->
        if not first then
          pp f "@ @[<hov2>and@ %s:@ %a@]%a" (Module_declaration.pmd_name pmd).txt
            (module_type1 ctxt) (Module_declaration.pmd_type pmd)
            (item_attributes ctxt) (Module_declaration.pmd_attributes pmd)
        else
          pp f "@[<hov2>module@ rec@ %s:@ %a@]%a" (Module_declaration.pmd_name pmd).txt
            (module_type1 ctxt) (Module_declaration.pmd_type pmd)
            (item_attributes ctxt) (Module_declaration.pmd_attributes pmd);
        string_x_module_type_list f ~first:false tl
    in
    string_x_module_type_list f decls
  | Psig_attribute a -> floating_attribute ctxt f a
  | Psig_extension(e, a) ->
    item_extension ctxt f e;
    item_attributes ctxt f a

and module_expr ctxt f x =
  if has_attributes (Module_expr.pmod_attributes x)
  then
    pp f "((%a)%a)" (module_expr ctxt)
      (Module_expr.update x ~pmod_attributes:(Attributes.create []))
      (attributes ctxt)
      (Module_expr.pmod_attributes x)
  else
    match%view Module_expr.pmod_desc x with
    | Pmod_structure (Structure s) ->
        pp f "@[<hv2>struct@;@[<0>%a@]@;<1 -2>end@]"
          (list (structure_item ctxt) ~sep:"@\n") s;
    | Pmod_constraint (me, mt) ->
        pp f "@[<hov2>(%a@ :@ %a)@]"
          (module_expr ctxt) me
          (module_type ctxt) mt
    | Pmod_ident (li) ->
        pp f "%a" longident_loc li;
    | Pmod_functor (_, None, me) ->
        pp f "functor ()@;->@;%a" (module_expr ctxt) me
    | Pmod_functor (s, Some mt, me) ->
        pp f "functor@ (%s@ :@ %a)@;->@;%a"
          s.txt (module_type ctxt) mt (module_expr ctxt) me
    | Pmod_apply (me1, me2) ->
        pp f "(%a)(%a)" (module_expr ctxt) me1 (module_expr ctxt) me2
        (* Cf: #7200 *)
    | Pmod_unpack e ->
        pp f "(val@ %a)" (expression ctxt) e
    | Pmod_extension e -> extension ctxt f e

and structure ctxt f x = list ~sep:"@\n" (structure_item ctxt) f (Structure.to_concrete x)

and payload ctxt f = function%view
  | PStr (Structure [{pstr_desc = Pstr_eval (e, attrs)}]) ->
      pp f "@[<2>%a@]%a"
        (expression ctxt) e
        (item_attributes ctxt) attrs
  | PStr x -> structure ctxt f x
  | PTyp x -> pp f ":"; core_type ctxt f x
  | PSig x -> pp f ":"; signature ctxt f x
  | PPat (x, None) -> pp f "?"; pattern ctxt f x
  | PPat (x, Some e) ->
      pp f "?"; pattern ctxt f x;
      pp f " when "; expression ctxt f e

(* transform [f = fun g h -> ..] to [f g h = ... ] could be improved *)
and binding ctxt f vb =
  match%view vb with
  | {pvb_pat=p; pvb_expr=x; _} ->
    (* .pvb_attributes have already been printed by the caller, #bindings *)
    let rec pp_print_pexp_function f x =
      if has_attributes (Expression.pexp_attributes x)
      then pp f "=@;%a" (expression ctxt) x
      else
        match%view Expression.pexp_desc x with
        | Pexp_fun (Nolabel, _, p, e) ->
          pp f "%a@ %a" (simple_pattern ctxt) p pp_print_pexp_function e
        | Pexp_fun (label, eo, p, e) ->
          pp f "%a@ %a" (label_exp ctxt) (label,eo,p) pp_print_pexp_function e
        | Pexp_newtype (str,e) ->
          pp f "(type@ %s)@ %a" str.txt pp_print_pexp_function e
        | _ -> pp f "=@;%a" (expression ctxt) x
    in
    let tyvars_str tyvars = List.map ~f:(fun v -> v.txt) tyvars in
    let is_desugared_gadt p e =
      let gadt_pattern =
        match%view p with
        | {ppat_desc=Ppat_constraint({ppat_desc=Ppat_var _} as pat,
                                     {ptyp_desc=Ptyp_poly (args_tyvars, rt)});
           ppat_attributes=Attributes[]}->
          Some (pat, args_tyvars, rt)
        | _ -> None
      in
      let rec gadt_exp tyvars e =
        match%view e with
        | {pexp_desc=Pexp_newtype (tyvar, e); pexp_attributes=Attributes []} ->
          gadt_exp (tyvar :: tyvars) e
        | {pexp_desc=Pexp_constraint (e, ct); pexp_attributes=Attributes []} ->
          Some (List.rev tyvars, e, ct)
        | _ -> None
      in
      let gadt_exp = gadt_exp [] e in
      match gadt_pattern, gadt_exp with
      | Some (p, pt_tyvars, pt_ct), Some (e_tyvars, e, e_ct)
        when List.equal String.equal (tyvars_str pt_tyvars) (tyvars_str e_tyvars) ->
        let ety = varify_constructors e_tyvars e_ct in
        if Stdlib.(=)
             (Conversion.ast_to_core_type ety)
             (Conversion.ast_to_core_type pt_ct)
        then Some (p, pt_tyvars, e_ct, e)
        else None
      | _ -> None
    in
    if has_attributes (Expression.pexp_attributes x)
    then pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x
    else
      match is_desugared_gadt p x with
      (* Intentionally left out, as part of the fix for PR#7344
         (commit c15ad73e56c54663c9cb6f7cac4241bb3c4e3cb8)
         | Some (p, [], ct, e) ->
         pp f "%a@;: %a@;=@;%a"
         (simple_pattern ctxt) p (core_type ctxt) ct (expression ctxt) e
      *)
      | Some (p, tyvars, ct, e) -> begin
          pp f "%a@;: type@;%a.@;%a@;=@;%a"
            (simple_pattern ctxt) p (list pp_print_string ~sep:"@;")
            (tyvars_str tyvars) (core_type ctxt) ct (expression ctxt) e
        end
      | None -> begin
          match%view x, p with
          | _, {ppat_desc=Ppat_constraint(p ,ty);
                ppat_attributes=Attributes[]} -> (* special case for the first *)
            begin
              match%view ty with
              | {ptyp_desc=Ptyp_poly _; ptyp_attributes=Attributes[]} ->
                pp f "%a@;:@;%a@;=@;%a" (simple_pattern ctxt) p
                  (core_type ctxt) ty (expression ctxt) x
              | _ ->
                pp f "(%a@;:@;%a)@;=@;%a" (simple_pattern ctxt) p
                  (core_type ctxt) ty (expression ctxt) x
            end
          | {pexp_desc=Pexp_constraint (x, ty)}, _ ->
            (* XXX: this case is janestreet only *)
            pp f "%a@;:@;%a@;=@;%a" (pattern ctxt) p
              (core_type ctxt) ty (expression ctxt) x
          | _, {ppat_desc=Ppat_var _; ppat_attributes=Attributes[]} ->
            pp f "%a@ %a" (simple_pattern ctxt) p pp_print_pexp_function x
          | _ ->
            pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x
        end

(* [in] is not printed *)
and bindings ctxt f (rf,l) =
  let binding kwd rf f x =
    pp f "@[<2>%s %a%a@]%a" kwd rec_flag rf
      (binding ctxt) x (item_attributes ctxt)
      (Value_binding.pvb_attributes x)
  in
  match l with
  | [] -> ()
  | [x] -> binding "let" rf f x
  | x::xs ->
      pp f "@[<v>%a@,%a@]"
        (binding "let" rf) x
        (list ~sep:"@," (binding "and" Rec_flag.nonrecursive)) xs

and structure_item ctxt f x =
  match%view Structure_item.pstr_desc x with
  | Pstr_eval (e, attrs) ->
    pp f "@[<hov2>;;%a@]%a"
      (expression ctxt) e
      (item_attributes ctxt) attrs
  | Pstr_type (_, []) -> assert false
  | Pstr_type (rf, l)  -> type_def_list ctxt f (rf, l)
  | Pstr_value (rf, l) ->
    (* pp f "@[<hov2>let %a%a@]"  rec_flag rf bindings l *)
    pp f "@[<2>%a@]" (bindings ctxt) (rf,l)
  | Pstr_typext te -> type_extension ctxt f te
  | Pstr_exception ed -> exception_declaration ctxt f ed
  | Pstr_module x ->
    let rec module_helper me =
      match%view me with
      | {pmod_desc=Pmod_functor(s,mt,me'); pmod_attributes = Attributes []} ->
        if Option.is_none mt then pp f "()"
        else Ocaml_common.Misc.may (pp f "(%s:%a)" s.txt (module_type ctxt)) mt;
        module_helper me'
      | me -> me
    in
    pp f "@[<hov2>module %s%a@]%a"
      (Module_binding.pmb_name x).txt
      (fun f me ->
         let me = module_helper me in
         match%view me with
         | {pmod_desc=
              Pmod_constraint
                (me',
                 ({pmty_desc=(Pmty_ident (_)
                             | Pmty_signature (_));_} as mt));
            pmod_attributes = Attributes []} ->
           pp f " :@;%a@;=@;%a@;"
             (module_type ctxt) mt (module_expr ctxt) me'
         | _ -> pp f " =@ %a" (module_expr ctxt) me)
      (Module_binding.pmb_expr x)
      (item_attributes ctxt)
      (Module_binding.pmb_attributes x)
  | Pstr_open od ->
    pp f "@[<2>open%s@;%a@]%a"
      (override (Open_description.popen_override od))
      longident_loc (Open_description.popen_lid od)
      (item_attributes ctxt) (Open_description.popen_attributes od)
  | Pstr_modtype {pmtd_name=s; pmtd_type=md; pmtd_attributes=attrs} ->
    pp f "@[<hov2>module@ type@ %s%a@]%a"
      s.txt
      (fun f md -> match md with
         | None -> ()
         | Some mt ->
           pp_print_space f () ;
           pp f "@ =@ %a" (module_type ctxt) mt
      ) md
      (item_attributes ctxt) attrs
  | Pstr_class l ->
    let extract_class_args cl =
      let rec loop acc = function%view
        | {pcl_desc=Pcl_fun (l, eo, p, cl'); pcl_attributes = Attributes []} ->
          loop ((l,eo,p) :: acc) cl'
        | cl -> List.rev acc, cl
      in
      let args, cl = loop [] cl in
      let constr, cl =
        match%view cl with
        | {pcl_desc=Pcl_constraint (cl', ct); pcl_attributes = Attributes []} ->
          Some ct, cl'
        | _ -> None, cl
      in
      args, constr, cl
    in
    let class_constraint f ct = pp f ": @[%a@] " (class_type ctxt) ct in
    let class_declaration kwd f x =
      let x = Class_declaration.to_concrete x in
      match%view x with
      | {pci_params=ls; pci_name={txt;_}; _} ->
        let args, constr, cl = extract_class_args (Class_infos.pci_expr x) in
        pp f "@[<2>%s %a%a%s %a%a=@;%a@]%a" kwd
          virtual_flag (Class_infos.pci_virt x)
          (class_params_def ctxt) ls txt
          (list (label_exp ctxt)) args
          (option class_constraint) constr
          (class_expr ctxt) cl
          (item_attributes ctxt) (Class_infos.pci_attributes x)
    in
    begin
      match l with
      | [] -> ()
      | [x] -> class_declaration "class" f x
      | x :: xs ->
        pp f "@[<v>%a@,%a@]"
          (class_declaration "class") x
          (list ~sep:"@," (class_declaration "and")) xs
    end
  | Pstr_class_type l -> class_type_declaration_list ctxt f l
  | Pstr_primitive vd ->
    pp f "@[<hov2>external@ %a@ :@ %a@]%a"
      protect_ident (Value_description.pval_name vd).txt
      (value_description ctxt) vd
      (item_attributes ctxt) (Value_description.pval_attributes vd)
  | Pstr_include (Include_declaration incl) ->
    pp f "@[<hov2>include@ %a@]%a"
      (module_expr ctxt) (Include_infos.pincl_mod incl)
      (item_attributes ctxt) (Include_infos.pincl_attributes incl)
  | Pstr_recmodule decls -> (* 3.07 *)
    let aux f = function%view
      | ({pmb_expr={pmod_desc=Pmod_constraint (expr, typ)}} as pmb) ->
        pp f "@[<hov2>@ and@ %s:%a@ =@ %a@]%a" (Module_binding.pmb_name pmb).txt
          (module_type ctxt) typ
          (module_expr ctxt) expr
          (item_attributes ctxt) (Module_binding.pmb_attributes pmb)
      | _ -> assert false
    in
    begin
      match%view decls with
      | ({pmb_expr={pmod_desc=Pmod_constraint (expr, typ)}} as pmb) :: l2 ->
        pp f "@[<hv>@[<hov2>module@ rec@ %s:%a@ =@ %a@]%a@ %a@]"
          (Module_binding.pmb_name pmb).txt
          (module_type ctxt) typ
          (module_expr ctxt) expr
          (item_attributes ctxt) (Module_binding.pmb_attributes pmb)
          (fun f l2 -> List.iter ~f:(aux f) l2) l2
      | _ -> assert false
    end
  | Pstr_attribute a -> floating_attribute ctxt f a
  | Pstr_extension(e, a) ->
    item_extension ctxt f e;
    item_attributes ctxt f a

and type_param ctxt f (ct, a) =
  pp f "%s%a" (type_variance a) (core_type ctxt) ct

and type_params ctxt f = function
  | [] -> ()
  | l -> pp f "%a " (list (type_param ctxt) ~first:"(" ~last:")" ~sep:",@;") l

and type_def_list ctxt f (rf, l) =
  let type_decl kwd rf f x =
    let eq =
      match%view x with
      | { ptype_kind = Ptype_abstract; ptype_manifest = None; _ } -> ""
      | _ -> " ="
    in
    pp f "@[<2>%s %a%a%s%s%a@]%a" kwd
      nonrec_flag rf
      (type_params ctxt) (Type_declaration.ptype_params x)
      (Type_declaration.ptype_name x).txt eq
      (type_declaration ctxt) x
      (item_attributes ctxt) (Type_declaration.ptype_attributes x)
  in
  match l with
  | [] -> assert false
  | [x] -> type_decl "type" rf f x
  | x :: xs -> pp f "@[<v>%a@,%a@]"
                 (type_decl "type" rf) x
                 (list ~sep:"@," (type_decl "and" Rec_flag.recursive)) xs

and record_declaration ctxt f lbls =
  let type_record_field f pld =
    pp f "@[<2>%a%s:@;%a@;%a@]"
      mutable_flag (Label_declaration.pld_mutable pld)
      (Label_declaration.pld_name pld).txt
      (core_type ctxt) (Label_declaration.pld_type pld)
      (attributes ctxt) (Label_declaration.pld_attributes pld)
  in
  pp f "{@\n%a}"
    (list type_record_field ~sep:";@\n" ) lbls

and type_declaration ctxt f x =
  (* type_declaration has an attribute field,
     but it's been printed by the caller of this method *)
  let priv f =
    match%view Type_declaration.ptype_private x with
    | Public -> ()
    | Private -> pp f "@;private"
  in
  let manifest f =
    match Type_declaration.ptype_manifest x with
    | None -> ()
    | Some y ->
      match%view Type_declaration.ptype_kind x with
      | Ptype_abstract -> pp f "%t@;%a" priv (core_type ctxt) y
      | _ -> pp f "@;%a" (core_type ctxt) y
  in
  let constructor_declaration f pcd =
    pp f "|@;";
    constructor_declaration ctxt f
      ( (Constructor_declaration.pcd_name pcd).txt
      , Constructor_declaration.pcd_args pcd
      , Constructor_declaration.pcd_res pcd
      , Constructor_declaration.pcd_attributes pcd )
  in
  let repr f =
    let intro f =
      if Option.is_none (Type_declaration.ptype_manifest x)
      then ()
      else pp f "@;="
    in
    match%view Type_declaration.ptype_kind x with
    | Ptype_variant xs ->
      let variants fmt xs =
        if List.is_empty xs then pp fmt " |" else
          pp fmt "@\n%a" (list ~sep:"@\n" constructor_declaration) xs
      in pp f "%t%t%a" intro priv variants xs
    | Ptype_abstract -> ()
    | Ptype_record l ->
        pp f "%t%t@;%a" intro priv (record_declaration ctxt) l
    | Ptype_open -> pp f "%t%t@;.." intro priv
  in
  let constraints f =
    List.iter
      ~f:(fun (ct1,ct2,_) ->
         pp f "@[<hov2>@ constraint@ %a@ =@ %a@]"
           (core_type ctxt) ct1 (core_type ctxt) ct2)
      (Type_declaration.ptype_cstrs x)
  in
  pp f "%t%t%t" manifest repr constraints

and type_extension ctxt f x =
  let extension_constructor f x =
    pp f "@\n|@;%a" (extension_constructor ctxt) x
  in
  pp f "@[<2>type %a%a += %a@ %a@]%a"
    (fun f -> function
       | [] -> ()
       | l ->
           pp f "%a@;" (list (type_param ctxt) ~first:"(" ~last:")" ~sep:",") l)
    (Type_extension.ptyext_params x)
    longident_loc (Type_extension.ptyext_path x)
    private_flag (Type_extension.ptyext_private x) (* Cf: #7200 *)
    (list ~sep:"" extension_constructor)
    (Type_extension.ptyext_constructors x)
    (item_attributes ctxt) (Type_extension.ptyext_attributes x)

and constructor_declaration ctxt f (name, args, res, attrs) =
  let name =
    match name with
    | "::" -> "(::)"
    | s -> s
  in
  match res with
  | None ->
      pp f "%s%a@;%a" name
        (fun f -> function%view
           | Pcstr_tuple [] -> ()
           | Pcstr_tuple l ->
             pp f "@;of@;%a" (list (core_type1 ctxt) ~sep:"@;*@;") l
           | Pcstr_record l -> pp f "@;of@;%a" (record_declaration ctxt) l
        ) args
        (attributes ctxt) attrs
  | Some r ->
      pp f "%s:@;%a@;%a" name
        (fun f -> function%view
           | Pcstr_tuple [] -> core_type1 ctxt f r
           | Pcstr_tuple l -> pp f "%a@;->@;%a"
                                (list (core_type1 ctxt) ~sep:"@;*@;") l
                                (core_type1 ctxt) r
           | Pcstr_record l ->
               pp f "%a@;->@;%a" (record_declaration ctxt) l (core_type1 ctxt) r
        )
        args
        (attributes ctxt) attrs

and extension_constructor ctxt f x =
  (* Cf: #7200 *)
  match%view Extension_constructor.pext_kind x with
  | Pext_decl(l, r) ->
    constructor_declaration ctxt f
      ( (Extension_constructor.pext_name x).txt
      , l
      , r
      , Extension_constructor.pext_attributes x )
  | Pext_rebind li ->
      pp f "%s%a@;=@;%a" (Extension_constructor.pext_name x).txt
        (attributes ctxt) (Extension_constructor.pext_attributes x)
        longident_loc li

and case_list ctxt f l : unit =
  let aux f case =
    match%view case with
    | {pc_lhs; pc_guard; pc_rhs} ->
      pp f "@;| @[<2>%a%a@;->@;%a@]"
        (pattern ctxt) pc_lhs (option (expression ctxt) ~first:"@;when@;")
        pc_guard (expression (under_pipe ctxt)) pc_rhs
  in
  list aux f l ~sep:""

and label_x_expression_param ctxt f (l,e) =
  let simple_name =
    match%view e with
    | {pexp_desc=Pexp_ident (Longident_loc {txt=Lident l;_});
       pexp_attributes=Attributes[]} -> Some l
    | _ -> None
  in
  match%view l with
  | Nolabel  -> expression2 ctxt f e (* level 2*)
  | Optional str ->
    if Option.equal String.equal (Some str) simple_name
    then
      pp f "?%s" str
    else
      pp f "?%s:%a" str (simple_expr ctxt) e
  | Labelled lbl ->
    if Option.equal String.equal (Some lbl) simple_name
    then
      pp f "~%s" lbl
    else
      pp f "~%s:%a" lbl (simple_expr ctxt) e

and directive_argument f x =
  match%view x with
  | Pdir_none -> ()
  | Pdir_string (s) -> pp f "@ %S" s
  | Pdir_int (n, None) -> pp f "@ %s" n
  | Pdir_int (n, Some m) -> pp f "@ %s%c" n m
  | Pdir_ident (li) -> pp f "@ %a" longident li
  | Pdir_bool (b) -> pp f "@ %s" (string_of_bool b)

let toplevel_phrase f x =
  match%view x with
  | Ptop_def (Structure s) ->pp f "@[<hov0>%a@]"  (list (structure_item reset_ctxt)) s
   (* pp_open_hvbox f 0; *)
   (* pp_print_list structure_item f s ; *)
   (* pp_close_box f (); *)
  | Ptop_dir (s, da) ->
   pp f "@[<hov2>#%s@ %a@]" s directive_argument da
   (* pp f "@[<hov2>#%s@ %a@]" s directive_argument da *)

let expression f x =
  pp f "@[%a@]" (expression reset_ctxt) x

let string_of_expression x =
  ignore (flush_str_formatter ()) ;
  let f = str_formatter in
  expression f x;
  flush_str_formatter ()

let string_of_structure x =
  ignore (flush_str_formatter ());
  let f = str_formatter in
  structure reset_ctxt f x;
  flush_str_formatter ()

let top_phrase f x =
  pp_print_newline f ();
  toplevel_phrase f x;
  pp f ";;";
  pp_print_newline f ()

let core_type = core_type reset_ctxt
let pattern = pattern reset_ctxt
let signature = signature reset_ctxt
let structure = structure reset_ctxt
let class_expr       = class_expr       reset_ctxt
let class_field      = class_field      reset_ctxt
let class_type       = class_type       reset_ctxt
let class_signature  = class_signature  reset_ctxt
let class_type_field = class_type_field reset_ctxt
let module_expr      = module_expr      reset_ctxt
let module_type      = module_type      reset_ctxt
let signature_item   = signature_item   reset_ctxt
let structure_item   = structure_item   reset_ctxt
