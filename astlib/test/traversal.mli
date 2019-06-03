(*$ Astlib_cinaps.print_traversal_mli () *)
module V1 : sig
  module Expression : sig
    val copy : Astlib.V1.Expression.t -> Astlib.V1.Expression.t
  end

  module Case : sig
    val copy : Astlib.V1.Case.t -> Astlib.V1.Case.t
  end

  module Pattern : sig
    val copy : Astlib.V1.Pattern.t -> Astlib.V1.Pattern.t
  end
end

module V2 : sig
  module Expression : sig
    val copy : Astlib.V2.Expression.t -> Astlib.V2.Expression.t
  end

  module Case : sig
    val copy : Astlib.V2.Case.t -> Astlib.V2.Case.t
  end

  module Pattern : sig
    val copy : Astlib.V2.Pattern.t -> Astlib.V2.Pattern.t
  end
end

module V3 : sig
  module Identifier : sig
    val copy : Astlib.V3.Identifier.t -> Astlib.V3.Identifier.t
  end

  module Expression : sig
    val copy : Astlib.V3.Expression.t -> Astlib.V3.Expression.t
  end

  module Case : sig
    val copy : Astlib.V3.Case.t -> Astlib.V3.Case.t
  end

  module Pattern : sig
    val copy : Astlib.V3.Pattern.t -> Astlib.V3.Pattern.t
  end
end
(*$*)
