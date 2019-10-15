module type S = sig
  val version : string
  val grammar : Grammar.t
  val conversions : History.conversion list
end
