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
