(** Native, customizable implementation of {{:https://github.com/ai/nanoid} Nano
    ID}. Unless you specifically want to customize random number generation, you
    should probably use the simpler [nanoid] library, which uses the [Simple ()]
    implementation from this library. *)

(** @inline*)
include module type of Intf

(** [pseudo_seeded seed] is a [RNG] module that generates pseudo random numbers
    based on the given [seed]. It is subject to the same limitations as
    {!Cryptokit.Random.pseudo_rng} *)
val pseudo_seeded : string -> (module RNG)

(** [Make (Rng)] is a Nano ID generator based on random number generator [Rng] *)
module Make (Rng : RNG) : S

(** [Simple ()] is a Nano ID generator based on the {!pseudo_seeded} random
    number generator seeded with the current system time as given by
    {!Unix.gettimeofday}. *)
module Simple () : S
