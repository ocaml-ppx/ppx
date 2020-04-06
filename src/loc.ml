open! Import

type 'a t = 'a Astlib.Loc.t = { txt : 'a; loc : Astlib.Location.t }

let txt t = t.txt
let loc t = t.loc

let make ~loc txt = { loc; txt }
let map = Astlib.Loc.map
