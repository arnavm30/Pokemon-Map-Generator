(** Representation of adjacency rules. *)

type t
(** The abstract type of values representing adjaceny rules. *)

(** The abstract type of directions *)
type directions = UP | DOWN | LEFT | RIGHT

val empty : int -> t
(** [empty] is empty adjancey rules. *)

val allow : int -> int -> directions -> t -> t
(** [allow i j dir adj_rules] adds the adjaceny rule [(i, j, dir)] to [adj_rules]
    where tile with index [j] can tile next to tile with index [i] in direction [dir] *)

val combine : t -> t -> t
(** [combine ht1 ht2] combines the adjaceny rules of [ht1] and [ht2] into one hash table *)

val is_allowed : int -> int -> directions -> t -> bool
(** [is_allowed i j dir ht] is whether adjacency rule [(i, j, dir)] is in 
    adjacency rules [s] *)

val count_init_enablers : int -> t -> Cell.directions array
(** [count_init_enablers t ht] is the count of enablers in [ht] each direction for each
    tile, 0 <= t = num_tiles. *)

val opposite_dir : directions -> directions
(** [opposite_dir dir] returns the opposite direction from [dir] *)

val get_all_rules : t -> (int * int * directions) list
(** [get_all_rules ht] returns list of adjacency rules of [ht] *)

val print_to_string : t -> unit
(** [print_to_string ht] prints the adjacency rules [ht] to screen *)

val string_of_dir : directions -> string
(** [string_of_dir dir] turns direction [dir] into a string *)
