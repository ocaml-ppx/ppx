module V4_07 = struct
  include Builder_common
  include Builder_v4_07
end

module Unstable_for_testing = struct
  include Builder_common
  include Builder_unstable_for_testing
end
