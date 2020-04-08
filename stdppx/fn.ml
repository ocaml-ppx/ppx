external id : 'a -> 'a = "%identity"
let const x _ = x
let compose f g x = f (g x)
