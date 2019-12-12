let version = Version.of_string "v4.06"

module Next = Version_4_07

let grammar = Next.grammar

let to_next : History.conversion =
  { src_version = version
  ; dst_version = Next.version
  ; f = fun node ~to_node:_ ~of_node:_ -> node
  }

let of_next : History.conversion =
  { src_version = Next.version
  ; dst_version = version
  ; f = fun node ~to_node:_ ~of_node:_ -> node
  }

let conversions = [ to_next; of_next ]
