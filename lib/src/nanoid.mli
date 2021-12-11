(** Implementation of {{:https://github.com/ai/nanoid} Nano ID}, a tiny, secure,
    URL-friendly, unique string ID generator popular in the javascript
    ecosystem.

    The interface mimics the Javascript interface as closely as possible with a
    single {!nanoid} entrypoint. Custom alphabets are not yet supported.

    Both native and js_of_ocaml environments are supported via a
    {{:https://dune.readthedocs.io/en/stable/variants.html)} virtual library}:
    [nanoid]. Libraries should depend on this library, and final executables
    should pick an implementation by depending either on [nanoid.os] or
    [nanoid.jsoo].

    The native implementation mimics the javascript one and relies by default on
    [cryptokit] pseudo random number generator seeded with the current unix time
    as given by [Unix.gettimeofday]. This should yield satisfying results for
    most application without ever blocking, unless you need strong cryptographic
    guarantees for security reasons. Use the [Nanoid_os] library to customize
    the random number generation. *)

include Versions.VERSION_1_0
