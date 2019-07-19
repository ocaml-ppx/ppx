type 'a t = 'a option

val map : 'a t -> f:('a -> 'b) -> 'b t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val all : 'a t list -> 'a list t

module List : sig
  val map : 'a list -> f:('a -> 'b t) -> 'b list t
end

module Option : sig
  val map : 'a option -> f:('a -> 'b t) -> 'b option t
end
