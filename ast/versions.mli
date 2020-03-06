include module type of struct
  include Unversioned
end with module Private := Unversioned.Private
module V4_07 = Version_v4_07
module Unstable_for_testing = Version_unstable_for_testing
