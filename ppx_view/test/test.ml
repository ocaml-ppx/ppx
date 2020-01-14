open Migrate_parsetree.Ast_407
open Viewast.Parseview


let int   x = Ast_helper.(Exp.constant (Const.int   x))
let int32 x = Ast_helper.(Exp.constant (Const.int32 x))
let float x = Ast_helper.(Exp.constant (Const.float x))
let ident x = Ast_helper.(Exp.ident { txt = Lident x; loc = Location.none; })

let%expect_test "match failure" =
  begin try match%view int 3 with
    | Pexp_constant (Pconst_integer ("2", _)) ->
      print_string "matched"
  with e ->
    print_string (Printexc.to_string e)
  end;[%expect {|"Match_failure ppx_view/test/test.ml:11:18"|}]

let%expect_test "match simple" =
  let match_3 = function%view
    | Pexp_constant (Pconst_integer ("3", _)) ->
      print_string "3"
    | Pexp_tuple _ ->
      print_string "tuple"
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int 3);
  end;[%expect {|3|}]

let%expect_test "match with guard" =
  let match_3 = function%view
    | Pexp_constant (Pconst_integer (s, _)) when s = "3" ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int 3);
  end;[%expect {|3|}]

let%expect_test "match or-pattern" =
  let match_3 = function%view
    | Pexp_constant (Pconst_integer ("3",  _))
    | Pexp_constant (Pconst_float   ("3.", _)) ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int      3);
    match_3 (int32   3l);
    match_3 (float "3.")
  end;[%expect {|333|}]

let%expect_test "match or-pattern with variable" =
  let match_3xyz = function%view
    | Pexp_constant (Pconst_integer (s, _))
    | Pexp_constant (Pconst_float   (s, _)) when s.[0] = '3' ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3xyz (int        3);
    match_3xyz (int32   321l);
    match_3xyz (float "3.14")
  end;[%expect {|333|}]

let%expect_test "match deep or-pattern" =
  let match_3_4 = function%view
    | Pexp_constant (Pconst_integer (("3" | "4"),  _)) ->
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
    | Pexp_constant (Pconst_integer ("3" as s,  _)) ->
      print_string s
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int 3);
  end;[%expect {|3|}]

let%expect_test "match with record" =
  let match_ident = function%view
    | Pexp_ident { txt = Lident id; } ->
      print_string id
    | _ ->
      print_string "KO"
  in
  begin
    match_ident (ident "x");
  end;[%expect {|x|}]

type custom = Int of int | Unit of unit | Nothing

let%expect_test "match with custom constructor" =
  let open Viewlib in
  let int : (int, 'i, 'o) View.t -> (custom, 'i, 'o) View.t =
    fun view value ->
      match value with
      | Int   i -> view i
      | Unit  _ -> View.error
      | Nothing -> View.error
  in
  let unit : (unit, 'i, 'i) View.t -> (custom, 'i, 'i) View.t =
    fun view value ->
      match value with
      | Int   _ -> View.error
      | Unit () -> view ()
      | Nothing -> View.error
  in
  let nothing : (custom, 'i, 'i) View.t =
    fun value ->
      match value with
      | Int   _ -> View.error
      | Unit () -> View.error
      | Nothing -> View.unit ()
  in
  let useless : (custom, 'i, 'i) View.t =
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
  let int : (int, 'i, 'o) View.t -> ([> `Int of int], 'i, 'o) View.t =
    fun view value ->
      match value with
      | `Int i -> view i
      | _      -> View.error
  in
  let unit : (unit, 'i, 'i) View.t -> ([> `Unit of unit], 'i, 'i) View.t =
    fun view value ->
      match value with
      | `Unit () -> view ()
      | _        -> View.error
  in
  let nothing : ([> `Nothing], 'i, 'i) View.t =
    fun value ->
      match value with
      | `Nothing -> View.unit ()
      | _        -> View.error
  in
  let useless : ([`Int of int | `Unit of unit | `Nothing], 'i, 'i) View.t =
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
  let int : (int, 'i, 'o) View.t -> (< int: int; .. >, 'i, 'o) View.t =
    fun view value ->
      view value#int
  in
  let unit : (unit, 'i, 'i) View.t -> (< unit: unit; .. >, 'i, 'i) View.t =
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

  let pair view value =
    match value with
    | Pair (x, y) -> view (x, y)
    | Record _ -> Viewlib.View.error

  let record view value =
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
