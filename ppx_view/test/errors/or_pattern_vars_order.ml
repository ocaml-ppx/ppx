let f x =
  match%view x with
  | Pair (x, y)
  | Record {fst = y; snd = x} -> x - y
