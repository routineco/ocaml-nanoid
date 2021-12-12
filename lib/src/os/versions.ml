module Intf = struct
  (** Random bytes generator *)
  module type RNG = sig
    (** [random_bytes size] returns a buffer and a offset at which [size] fresh
        random bytes are readable. *)
    val random_bytes : int -> bytes * int
  end

  (** Nano ID generator *)
  module type S = sig
    (** [nanoid ?size ()] generates a random Nano ID of size [size], which
        defaults to 21. *)
    val nanoid : ?size:int -> unit -> string
  end
end

module type VERSION_1_0 = sig
  (** @inline *)
  include module type of Intf

  (** [pseudo_seeded seed] is a [RNG] module that generates pseudo random
      numbers based on the given [seed]. It is subject to the same limitations
      as {!Cryptokit.Random.pseudo_rng} *)
  val pseudo_seeded : string -> (module RNG)

  (** [Make (Rng)] is a Nano ID generator based on random number generator [Rng] *)
  module Make (Rng : RNG) : S

  (** [Simple ()] is a Nano ID generator based on the {!pseudo_seeded} random
      number generator seeded with the current system time as given by
      {!Unix.gettimeofday}. *)
  module Simple () : S
end
