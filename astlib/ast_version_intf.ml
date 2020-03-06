module type S = sig
  val version : Version.t
  val grammar : Grammar.t
  val conversions : History.conversion list
end
