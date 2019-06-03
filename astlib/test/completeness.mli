(*$ Astlib_cinaps.print_completeness_mli () *)
module V1 : sig
  module Expression : sig
    val check_exn : Astlib.V1.Expression.t -> unit
  end

  module Case : sig
    val check_exn : Astlib.V1.Case.t -> unit
  end

  module Pattern : sig
    val check_exn : Astlib.V1.Pattern.t -> unit
  end
end

module V2 : sig
  module Expression : sig
    val check_exn : Astlib.V2.Expression.t -> unit
  end

  module Case : sig
    val check_exn : Astlib.V2.Case.t -> unit
  end

  module Pattern : sig
    val check_exn : Astlib.V2.Pattern.t -> unit
  end
end

module V3 : sig
  module Identifier : sig
    val check_exn : Astlib.V3.Identifier.t -> unit
  end

  module Expression : sig
    val check_exn : Astlib.V3.Expression.t -> unit
  end

  module Case : sig
    val check_exn : Astlib.V3.Case.t -> unit
  end

  module Pattern : sig
    val check_exn : Astlib.V3.Pattern.t -> unit
  end
end
(*$*)
