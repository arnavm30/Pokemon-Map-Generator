(** Representation of a state.
 *)

type t
(** The abstract type of values representing a state. *)

val make : int -> int -> int -> t
(** Initializes unobserved state*)

val make_test : int -> int -> int -> t
(** Initializes map for testing *)

val smallest_entropies : t -> float array -> (int * int) list
(** List of smallest entropies*)

val smallest_entropy : t -> float array -> int * int
(** Randomly chooses the smallest entropy, if there are multiple *)

val propogate : t -> unit
(** Collapse state once after choosing smallest entropy *)

val draw : t -> int -> int -> unit
(** draw map x y renders the map to screen with bottom left corner at (x,y)*)
