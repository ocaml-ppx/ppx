open Viewlib

val txt'match : ('a, 'i, 'o) View.t -> ('a Astlib.Loc.t, 'i, 'o) View.t
val loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a Astlib.Loc.t, 'i, 'o) View.t

val loc_start'match : (Astlib.Position.t, 'i, 'o) View.t -> (Astlib.Location.t, 'i, 'o) View.t
val loc_end'match : (Astlib.Position.t, 'i, 'o) View.t -> (Astlib.Location.t, 'i, 'o) View.t
val loc_ghost'match : (bool, 'i, 'o) View.t -> (Astlib.Location.t, 'i, 'o) View.t

val pos_fname'match : (string, 'i, 'o) View.t -> (Astlib.Position.t, 'i, 'o) View.t
val pos_lnum'match : (int, 'i, 'o) View.t -> (Astlib.Position.t, 'i, 'o) View.t
val pos_bol'match : (int, 'i, 'o) View.t -> (Astlib.Position.t, 'i, 'o) View.t
val pos_cnum'match : (int, 'i, 'o) View.t -> (Astlib.Position.t, 'i, 'o) View.t
