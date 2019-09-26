type _ variables =
  | Var_nil  : unit variables
  | Var_snoc : 'tl variables * 'hd -> ('hd -> 'tl) variables

type 'a match_result

val ok : 'a -> 'a match_result

val error : 'b -> 'a match_result

val (>>=)
   : 'a match_result
  -> ('a -> 'b match_result)
  -> 'b match_result

val (>>|)
   : 'a match_result
  -> ('a -> 'b)
  -> 'b match_result

val (>>+)
   : ('a -> 'b match_result)
  -> ('b -> 'c match_result)
  -> 'a
  -> 'c match_result

val (>>++)
   : 'a match_result
  -> ('a -> 'b -> 'c match_result)
  -> 'b
  -> 'c match_result

type ('input, 'output) variables_fun =
  'input variables -> 'output variables match_result

type ('matched, 'produced) matcher =
  'matched -> 'produced match_result

type ('matched, 'input, 'output) t =
  'matched -> ('input, 'output) variables_fun


val guard_failed : unit -> 'a

val case
   : ('m, unit, 'p) t
  -> ('p variables -> 'r)
  -> 'm -> 'r match_result

val match_
   : (string * int * int)
  -> ('m -> 'r match_result) list
  -> 'm
  -> 'r


val __ : ('m, 'i, 'm -> 'i) t

val drop : ('m, 'i, 'i) t

val sequence
   : ('m, 'i, 'x) t
  -> ('m, 'x, 'o) t
  -> ('m, 'i, 'o) t

val choice
   : ('m, 'i, 'o) t
  -> ('m, 'i, 'o) t
  -> ('m, 'i, 'o) t

val tuple2
   : ('m0, 'i, 'x) t
  -> ('m1, 'x, 'o) t
  -> ('m0 * 'm1, 'i, 'o) t

val tuple3
   : ('m0, 'i, 'x) t
  -> ('m1, 'x, 'y) t
  -> ('m2, 'y, 'o) t
  -> ('m0 * 'm1 * 'm2, 'i, 'o) t

val tuple4
   : ('m0, 'i, 'x) t
  -> ('m1, 'x, 'y) t
  -> ('m2, 'y, 'z) t
  -> ('m3, 'z, 'o) t
  -> ('m0 * 'm1 * 'm2 * 'm3, 'i, 'o) t

val not
   : ('m, 'i, 'i) t
  -> ('m, 'i, 'i) t


val unit : (unit, 'i, 'i) t

val bool : bool -> (bool, 'i, 'i) t

val false_ : (bool, 'i, 'i) t

val true_ : (bool, 'i, 'i) t

val char : char -> (char, 'i, 'i) t

val string : string -> (string, 'i, 'i) t

val bytes : bytes -> (bytes, 'i, 'i) t

val int : int -> (int, 'i, 'i) t

val int32 : int32 -> (int32, 'i, 'i) t

val int64 : int64 -> (int64, 'i, 'i) t

val nativeint : nativeint -> (nativeint, 'i, 'i) t

val float : float -> (float, 'i, 'i) t

val interval : char -> char -> (char, 'i, 'i) t

val some : ('m, 'i, 'o) t -> ('m option, 'i, 'o) t

val none : ('m option, 'i, 'i) t

val cons : ('m, 'i, 'x) t -> ('m list, 'x, 'o) t -> ('m list, 'i, 'o) t

val nil : ('m list, 'i, 'i) t

type 'a larray

val larray : 'a array -> 'a larray

val larray_cons : ('m, 'i, 'x) t -> ('m larray, 'x, 'o) t -> ('m larray, 'i, 'o) t

val larray_nil : ('m larray, 'i, 'i) t
