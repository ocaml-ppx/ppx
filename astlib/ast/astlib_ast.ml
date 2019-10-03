module Ast = Ast
module Grammar = Grammar

module History = struct
  include History

  let history = Actual.history
end
