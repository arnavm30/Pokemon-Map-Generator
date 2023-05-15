(** Representation of a state. *)

type t = {
  grid : Cell.t array array;
  heap : Cell.t Pairing_heap.t;
  stack : (Cell.t * int) Stack.t;
  w : int;
  h : int;
  mutable uncollapsed : int;
}
(** The abstract type of values representing a state. *)

type result = FINISHED of t | CONTRADICTION

val make : int -> int -> int -> float array -> Adj_rules.t -> t
(** [make x y l] is the initial unobserved state with dimensions [x] by [y] and 
    tiles initialized with [l] options *)

val smallest_entropy : t -> Cell.t option
(** [smallest_entropy state weights] is the tile with the smallest 
    entropy in [state], chooses randomly if there are multiple possibilities *)

val collapse_cell : float array -> Cell.t -> t -> unit
(** [collapse weights cell state] collapses [cell] with the smallest entropy in [state] 
with [weights] *)

val propogate : int -> float array -> Adj_rules.t -> t -> result
(** [propogate num_tiles ws adj_rules state] eliminates tiles in cells of [state] 
    that can no longer be chosen  *)

val validate : Adj_rules.t -> t -> unit

val check_valid : Adj_rules.t -> t -> bool * string list
(** [validate rules st] is whether state [st] is valid given adjacency rules [rules]*)

val draw : t -> int * int -> Tile.t array -> unit
(** [draw state (x,y) cells] renders the state to screen with bottom left corner 
    at [(x,y)]*)

val print_neighbors : Cell.t -> t -> unit
val print_tiles : t -> unit
