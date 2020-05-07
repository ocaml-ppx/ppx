let f x =
  match%view x with
  | 1z -> ()

let f x =
  match%view x with
  | 1.0z -> ()

let f x =
  match%view x with
  | Pair (x, y)
  | Record {fst = y; snd = x} -> x - y
