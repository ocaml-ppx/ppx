module Ast = Ast
module Type = Type

module History = struct
  include History

  let history = Actual.history
end
