module V4_07 : sig
  include module type of Builder_common
  include module type of Builder_v4_07
end

module Unstable_for_testing : sig
  include module type of Builder_common
  include module type of Builder_unstable_for_testing
end
