open! Base

type t = Astlib.Version.t

let compare = Comparable.lift String.compare ~f:Astlib.Version.to_string
let equal = Comparable.lift String.equal ~f:Astlib.Version.to_string
let sexp_of_t t = Sexp.Atom (Astlib.Version.to_string t)
let hash t = hash_string (Astlib.Version.to_string t)
let hash_fold_t hash t = hash_fold_string hash (Astlib.Version.to_string t)
