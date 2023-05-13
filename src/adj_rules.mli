(** Representation of adjacency rules. *)

type t
(** The abstract type of values representing adjaceny rules. *)

type directions =
  | UP
  | DOWN
  | LEFT
  | RIGHT  (** The abstract type of directions *)

val empty : t
(** [empty] is empty adjancey rules. *)

val allow : int -> int -> directions -> t -> t
(** [allow i j dir adj_rules] adds the adjaceny rule [(i, j, dir)] to [adj_rules]
    where tile with index [j] can tile next to tile with index [i] in direction [dir] *)

val combine : t -> t -> t
(** [combine s1 s2] combines the adjaceny rules of [s1] and [s2] into one set *)

val fold_dirs : (directions -> 'a -> 'a) -> 'a -> 'a
(** [fold_dirs f acc] accumulates result of [f] as it operates on 
    directions UP, DOWN, LEFT, RIGHT in that order. *)

val fold_dirs_tile_indexed : (t -> directions -> 'b -> 'b) -> t -> 'b -> 'b
(** [fold_dirs_tile_indexed f t acc] accumulates result of [f] as it operates on 
    adjacency rules [t] in directions UP, DOWN, LEFT, RIGHT in that order. *)

val allow_all : int -> int -> t -> t
(** [allow_all i j s] is the adjacency rules of [s] appended with rule that allows 
    tile [j] to be next to tile [i] in all directions *)

val is_allowed : int -> int -> directions -> t -> bool
(** [is_allowed i j dir s] is whether adjacency rule [(i, j, dir)] is in 
    adjacency rules [s] *)

val get_all_allowed : int -> int -> t -> (int * int * directions) list
(** [get_all_allowed i t s] is list of all adjacency rules in [s] that tile [i] 
    can be tiled next to *)

val count_enablers_for_one : int -> int -> t -> Cell.directions
(** [count_enablers_for_one i t s]  *)

val count_init_enablers : int -> t -> Cell.directions array
(** [count_init_enablers t s] *)

val opposite_dir : directions -> directions
(** [opposite_dir dir] returns the opposite direction from [dir] *)

val print_to_string : t -> unit
(** [print_to_string s] prints the adjacency rules [s] to screen *)
