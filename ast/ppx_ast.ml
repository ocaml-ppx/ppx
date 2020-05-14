module Compiler_types = Compiler_types
module Conversion = Conversion
module Traverse_builtins = Traverse_builtins
include Unversioned.Types

module Unversioned : sig
  module Private : sig
    exception Cannot_interpret_ast of {
      version : Astlib.Version.t;
      node_name : string;
      node : Node.t;
    }
  end
end = Unversioned

module V4_08 = struct
  include Versions.V4_08
  include Builder.V4_08
  include Viewer.V4_08
  include Traverse.V4_08
end

module V4_07 = struct
  include Versions.V4_07
  include Builder.V4_07
  include Viewer.V4_07
  include Traverse.V4_07
end

module Unstable_for_testing = struct
  include Versions.Unstable_for_testing
  include Builder.Unstable_for_testing
  include Viewer.Unstable_for_testing
  include Traverse.Unstable_for_testing
end
