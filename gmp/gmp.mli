module Types : sig
  type z
  type q
end

module Z : sig
  type t = Types.z

  val zero : t

  val of_int : int -> t
  val of_int64 : Int64.t -> t
  val of_float : float -> t
  val of_q : Types.q -> t
  val of_string : ?base:int -> string -> t

  val to_string : ?base:int -> t -> string

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val modulo : t -> t -> t
  val div_with_remainder : t -> t -> (t * t)

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val (mod) : t -> t -> t

  val compare : t -> t -> int
  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val (<) : t -> t -> bool
  val (>) : t -> t -> bool
  val (<=) : t -> t -> bool
  val (>=) : t -> t -> bool

  val min : t -> t -> t
  val max : t -> t -> t
end

module Q : sig
  type t = Types.q

  val of_ints : int * int -> t    
  val of_ints64 : Int64.t * Int64.t -> t
  val of_z : Types.z -> t
  val of_float : float -> t
  val of_string : ?base:int -> string -> t

  val to_string : ?base:int -> t -> string


  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t

  val compare : t -> t -> int
  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val (<) : t -> t -> bool
  val (>) : t -> t -> bool
  val (<=) : t -> t -> bool
  val (>=) : t -> t -> bool

  val min : t -> t -> t
  val max : t -> t -> t
end
