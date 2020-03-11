let version = Version.of_string "v4_07"

let grammar = Version_4_08.grammar

let to_4_08 : History.conversion =
  { src_version = version
  ; dst_version = Version_4_08.version
  ; f = fun node ~unwrap:_ ~wrap:_ -> Some node
  }

let of_4_08 : History.conversion =
  { src_version = Version_4_08.version
  ; dst_version = version
  ; f = fun node ~unwrap:_ ~wrap:_ -> Some node
  }

let conversions = [to_4_08; of_4_08]
