(** Representation of a state.
 *)

type t
(** The abstract type of values representing a state. *)

val make : int -> int -> t
(** Initializes unobserved state*)
