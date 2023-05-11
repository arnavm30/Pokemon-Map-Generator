(** Representation of a state. *)

type t
(** The abstract type of values representing a state. *)

val make : int -> int -> int -> t
(** [make x y l] is the initial unobserved state with dimensions [x] by [y] and 
    tiles initialized with [l] options *)

val make_test : int -> int -> Cells.t array -> t
(** [make_test x y l] is a state for testing purposes with dimensions [x] by [y] 
    and tiles initialized with [l] options *)

val smallest_entropies : t -> float array -> (int * int) list
(** [smallest_entropies state weights] is the list of tiles with the smallest 
    entropies from [state] *)

val smallest_entropy : t -> float array -> int * int
(** [smallest_entropy state weights] is the tile with the smallest 
    entropy in [state], chooses randomly if there are multiple possibilities *)

val propogate : t -> Cells.t array -> unit
(** [propogate state cells] collapses the tile with the smallest entropy in [state] *)

val draw : t -> int -> int -> Cells.t array -> unit
(** [draw state x y cells] renders the state to screen with bottom left corner 
    at ([x],[y])*)
