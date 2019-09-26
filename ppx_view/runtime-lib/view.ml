type _ variables =
  | Var_nil  : unit variables
  | Var_snoc : 'tl variables * 'hd -> ('hd -> 'tl) variables

type 'a match_result =
  | Ok of 'a
  | Error

let ok x =
  Ok x

let error _ =
  Error

let (>>=) x f =
  match x with
  | Ok y         -> f y
  | Error as err -> err

let (>>|) t f =
  t >>= (fun a -> ok (f a))

let (>>+) a b vars =
  a vars >>= b

let (>>++) x f vars =
  x >>= fun x -> f x vars

type ('input, 'output) variables_fun =
  'input variables -> 'output variables match_result

type ('matched, 'produced) matcher =
  'matched -> 'produced match_result

type ('matched, 'input, 'output) t =
  'matched -> ('input, 'output) variables_fun


exception Guard_failed

let guard_failed () =
  raise Guard_failed

let case view body value =
  view value Var_nil >>| body

let match_ loc cases value =
  let rec eval value = function
    | [] ->
      raise (Match_failure loc)
    | case :: cases ->
      begin match case value with
      | Ok res                 -> res
      | Error                  -> eval value cases
      | exception Guard_failed -> eval value cases
      end
  in
  eval value cases


let __ var list =
  ok (Var_snoc (list, var))

let drop _var list =
  ok list

let sequence view1 view2 value =
  view1 value >>+ view2 value

let choice view1 view2 value vars =
  match view1 value vars with
  | Ok _ as res -> res
  | Error       -> view2 value vars

let tuple2 view1 view2 (value1, value2) =
  view1 value1
  >>+ view2 value2

let tuple3 view1 view2 view3 (value1, value2, value3) =
  view1 value1
  >>+ view2 value2
  >>+ view3 value3

let tuple4 view1 view2 view3 view4 (value1, value2, value3, value4) =
  view1 value1
  >>+ view2 value2
  >>+ view3 value3
  >>+ view4 value4

let not view value vars =
  match view value vars with
  | Ok _  -> Error
  | Error -> Ok vars


let constant const value =
  if value = const then ok else error

let unit value = constant () value

let bool = constant

let false_ value = constant false value

let true_ value = constant true value

let char = constant

let string = constant

let bytes = constant

let int = constant

let int32 = constant

let int64 = constant

let nativeint = constant

let float = constant

let interval lower upper value =
  if value >= lower && value <= upper then
    ok
  else
    error

let some view value =
  match value with
  | None       -> error
  | Some value -> view value

let none value =
  constant None value

let cons view_hd view_tl value =
  match value with
  | []       -> error
  | hd :: tl -> view_hd hd >>+ view_tl tl

let nil value = constant [] value

type 'a larray =
  | Array_nil
  | Array_cons of 'a * 'a larray

let larray arr =
  let rec build idx acc =
    if idx >= 0 then
      build (pred idx) (Array_cons (arr.(idx), acc))
    else
      acc
  in
  build (pred (Array.length arr)) Array_nil

let larray_cons view_hd view_tl value =
  match value with
  | Array_nil           -> error
  | Array_cons (hd, tl) -> view_hd hd >>+ view_tl tl

let larray_nil value =
  constant Array_nil value
