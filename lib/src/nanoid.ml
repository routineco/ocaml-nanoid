include Nanoid_intf

(* This is the same rng buffering algorithm as the javascript implementation.
   While we have no certainty it's actually faster than calling Cryptokit every
   time, this hypothesis looks reasonable until more advanced benchmarks are
   made. *)
let pseudo_seeded seed =
  let multiplier = 32 in
  let module R = struct
    let rng = Cryptokit.Random.pseudo_rng seed

    let pool = ref None

    let random_bytes size =
      match !pool with
      | Some (buf, offset, len) when offset + size <= len ->
        pool := Some (buf, offset + size, len);
        (buf, offset)
      | Some (buf, _, len) when size <= len ->
        rng#random_bytes buf 0 len;
        pool := Some (buf, size, len);
        (buf, 0)
      | _ ->
        let len = multiplier * size in
        let buf = Bytes.create len in
        rng#random_bytes buf 0 len;
        pool := Some (buf, size, len);
        (buf, 0)
  end in
  (module R : RNG)

let alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

module Make (Rng : RNG) = struct
  let nanoid ?(size = 21) () =
    let bytes, offset = Rng.random_bytes size in
    String.init size (fun i ->
        alphabet.[Bytes.get_uint8 bytes (offset + i) land 0b00111111])
end

include
  Make ((val pseudo_seeded @@ Printf.sprintf "%.16f" @@ Unix.gettimeofday ()))
