module Types : sig
  type z
  type q
end

module Z : sig
  type t = z

  val zero : t

  val of_int : int -> t
  val to_string : ?base:int -> t -> string

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
end

module Q : sig
  type t = q
end
