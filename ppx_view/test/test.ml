open Ppx_ast
open V4_07

let loc = Ocaml_common.Location.none
[@@warning "-3"]

let int x = eint ~loc x
let int32 x = eint32 ~loc x
let float x = efloat ~loc (Float.to_string x)
let ident x = pexp_ident ~loc (Located.lident ~loc x)

let class_infos =
  let extension =
    Extension.create ({ loc; txt = "ext" } , Payload.pStr (Structure.create []) )
  in
  Class_infos.create
    ~pci_virt:Virtual_flag.virtual_
    ~pci_params:[]
    ~pci_name:{ loc; txt = "name" }
    ~pci_loc:loc
    ~pci_attributes:(Attributes.create [])
    ~pci_expr:(pcty_extension ~loc extension)

let%expect_test "match failure" =
  begin try match%view int 3 with
    | Econstant (Pconst_integer ("2", _)) ->
      print_string "matched"
  with e ->
    print_string (Printexc.to_string e)
  end;[%expect {|"Match_failure ppx_view/test/test.ml:25:12"|}]

let%expect_test "match simple" =
  let match_3 = function%view
    | Econstant (Pconst_integer ("3", _)) ->
      print_string "3"
    | Etuple _ ->
      print_string "tuple"
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int 3);
  end;[%expect {|3|}]

let%expect_test "match with guard" =
  let match_3 = function%view
    | Econstant (Pconst_integer (s, _)) when s = "3" ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int 3);
  end;[%expect {|3|}]

let%expect_test "match or-pattern" =
  let match_3 = function%view
    | Econstant (Pconst_integer ("3",  _))
    | Econstant (Pconst_float   ("3.", _)) ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int      3);
    match_3 (int32   3l);
    match_3 (float 3.0)
  end;[%expect {|333|}]

let%expect_test "match or-pattern with variable" =
  let match_3xyz = function%view
    | Econstant (Pconst_integer (s, _))
    | Econstant (Pconst_float   (s, _)) when s.[0] = '3' ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3xyz (int        3);
    match_3xyz (int32   321l);
    match_3xyz (float 3.14)
  end;[%expect {|333|}]

let%expect_test "match deep or-pattern" =
  let match_3_4 = function%view
    | Econstant (Pconst_integer (("3" | "4"),  _)) ->
      print_string "34"
    | _ ->
      print_string "KO"
  in
  begin
    match_3_4 (int 3);
    match_3_4 (int 4);
  end;[%expect {|3434|}]

let%expect_test "match with alias" =
  let match_3 = function%view
    | Econstant (Pconst_integer ("3" as s,  _)) ->
      print_string s
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int 3);
  end;[%expect {|3|}]

let%expect_test "match with record" =
  let match_ident = function%view
    | Eident (Longident_loc { txt = Lident id; _}) -> print_string id
    | _ -> print_string "KO"
  in
  begin
    match_ident (ident "x");
  end;[%expect {|x|}]

let%expect_test "match with polymorphic AST type" =
  (match%view class_infos with
   | { pci_virt = Virtual
     ; pci_params = []
     ; pci_name = {txt = "name"; _}
     ; pci_expr = Ctextension _ext
     ; _
     } ->
     print_string "OK"
   | _ -> print_string "KO");
  [%expect {|OK|}]

let%expect_test "non shortcut desc" =
  (match%view (int 3) with
   | {pexp_desc = Pexp_constant (Pconst_integer ("3", None)); _} ->
     print_string "OK"
   | _ ->
     print_string "KO");
  [%expect {|OK|}]

type custom = Int of int | Unit of unit | Nothing

let%expect_test "match with custom constructor" =
  let open Viewlib in
  let int'const : (int, 'i, 'o) View.t -> (custom, 'i, 'o) View.t =
    fun view value ->
      match value with
      | Int   i -> view i
      | Unit  _ -> View.error
      | Nothing -> View.error
  in
  let unit'const : (unit, 'i, 'i) View.t -> (custom, 'i, 'i) View.t =
    fun view value ->
      match value with
      | Int   _ -> View.error
      | Unit () -> view ()
      | Nothing -> View.error
  in
  let nothing'const : (custom, 'i, 'i) View.t =
    fun value ->
      match value with
      | Int   _ -> View.error
      | Unit () -> View.error
      | Nothing -> View.unit ()
  in
  let useless'const : (custom, 'i, 'i) View.t =
    fun value ->
      match value with
      | Int _            -> View.error
      | Unit _ | Nothing -> View.unit ()
  in
  let match_custom = function%view
    | Unit () ->
      print_string "()"
    | Int i ->
      print_int i
    | Nothing ->
      print_string "."
    | _ ->
      print_string "KO"
  in
  let match_custom' = function%view
    | Useless ->
      print_string "_"
    | Int i ->
      print_int i
    | _ ->
      print_string "KO"
  in
  begin
    match_custom  (Unit ());
    match_custom  (Int   3);
    match_custom  (Nothing);
    match_custom' (Unit ());
    match_custom' (Int   3);
    match_custom' (Nothing);
  end;[%expect {|()3._3_|}]

let%expect_test "match with polymorphic variant" =
  let open Viewlib in
  let int'const : (int, 'i, 'o) View.t -> ([> `Int of int], 'i, 'o) View.t =
    fun view value ->
      match value with
      | `Int i -> view i
      | _      -> View.error
  in
  let unit'const : (unit, 'i, 'i) View.t -> ([> `Unit of unit], 'i, 'i) View.t =
    fun view value ->
      match value with
      | `Unit () -> view ()
      | _        -> View.error
  in
  let nothing'const : ([> `Nothing], 'i, 'i) View.t =
    fun value ->
      match value with
      | `Nothing -> View.unit ()
      | _        -> View.error
  in
  let useless'const : ([`Int of int | `Unit of unit | `Nothing], 'i, 'i) View.t =
    fun value ->
      match value with
      | `Int _             -> View.error
      | `Unit _ | `Nothing -> View.unit ()
  in
  let match_custom = function%view
    | Unit () ->
      print_string "()"
    | Int i ->
      print_int i
    | Nothing ->
      print_string "."
    | _ ->
      print_string "KO"
  in
  let match_custom' = function%view
    | Useless ->
      print_string "_"
    | Int i ->
      print_int i
    | _ ->
      print_string "KO"
  in
  begin
    match_custom  (`Unit ());
    match_custom  (`Int   3);
    match_custom  (`Nothing);
    match_custom' (`Unit ());
    match_custom' (`Int   3);
    match_custom' (`Nothing);
  end;[%expect {|()3._3_|}]

let%expect_test "match with object" =
  let open Viewlib in
  let int'const : (int, 'i, 'o) View.t -> (< int: int; .. >, 'i, 'o) View.t =
    fun view value ->
      view value#int
  in
  let unit'const : (unit, 'i, 'i) View.t -> (< unit: unit; .. >, 'i, 'i) View.t =
    fun view value ->
      view value#unit
  in
  let match_custom = function%view
    | Int (3 as i) ->
      print_int i
    | Unit () ->
      print_string "()"
    | _ ->
      print_string "KO"
  in
  begin
    let make x = object method unit = () method int = x end in
    match_custom (make 1);
    match_custom (make 3);
  end;[%expect {|()3|}]

module M = struct
  type t =
    | Pair of int * int
    | Record of {fst : int; snd : int}

  let pair'const view value =
    match value with
    | Pair (x, y) -> view (x, y)
    | Record _ -> Viewlib.View.error

  let record'const view value =
    match value with
    | Pair _ -> Viewlib.View.error
    | Record {fst; snd} -> view (fst, snd)

  let fst'match view value =
    view (fst value)

  let snd'match view value =
    view (snd value)
end

let%expect_test "or-pattern" =
  let open M in
  (match%view (Record {fst=1; snd=2}) with
   | Pair (x, y)
   | Record {fst=x; snd=y} -> print_int (x + y));
  [%expect {|3|}]

let%expect_test "match with array" =
  let open Viewlib in
  let x = View.larray [|1; 2|] in
  (match%view x with
   | [|x; y|] -> print_int (x + y)
   | _ -> assert false);
  [%expect {|3|}]

module Shortcut = struct
  type a =
    | An_int of int
    | A_float of float

  type t =
    { a : a
    ; b : int
    ; c : int
    }

  let an_int'const view value =
    match value.a with
    | An_int i -> view i
    | _ -> Viewlib.View.error

  let a_float'const view value =
    match value.a with
    | A_float f -> view f
    | _ -> Viewlib.View.error

  let a'match view value = view value.a

  let b'match view value = view value.b

  let c'match view value = view value.c
end

let%expect_test "shortcut fields" =
  let open Shortcut in
  (match%view {a = An_int 1; b = 2; c = 3} with
   | An_int a [@view? {b; c}] ->
     print_int a;
     print_int b;
     print_int c
   | _ -> assert false);
  [%expect {|123|}]

let%expect_test "shortcut fields pattern" =
  let open Shortcut in
  (match%view {a = An_int 1; b = 2; c = 3} with
   | An_int a [@view? {b = (2 as x); c = (_ as y)}] ->
     print_int a;
     print_int x;
     print_int y
   | _ -> assert false);
  [%expect {|123|}]

let%expect_test "constructor translated to keyword" =
  let open Viewlib in
  let virtual'const _ = View.ok in
  (match%view 1 with
   | Virtual -> print_string "OK"
   | _ -> print_string "KO");
  [%expect {|OK|}]
