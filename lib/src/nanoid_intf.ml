(** Random bytes generator *)
module type RNG = sig
  val random_bytes : int -> bytes * int
  (** [random_bytes size] returns a buffer and a offset at which
     [size] fresh random bytes are readable. *)
end

(** Nano ID generator *)
module type S = sig
  val nanoid : ?size:int -> unit -> string
  (** [nanoid ?size ()] generates a random Nano ID of size [size],
     which defaults to 21. *)
end
