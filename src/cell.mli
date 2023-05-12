type directions = {
  mutable up : int;
  mutable right : int;
  mutable down : int;
  mutable left : int;
}

type t = {
  mutable collapsed : bool;
  mutable options : float array;
  mutable sum_of_ones : int;
  mutable sum_of_weights : float;
  mutable sum_of_weight_log_weights : float;
  noise : float;
  tile_enablers : directions array;
  coords : int * int;
}

exception Contradiction

val cmp : t -> t -> int
val check_collapsed : t -> unit
val check_contradiction : t -> unit
val make : int -> float -> float -> directions array -> int * int -> t
val make_test : Tile.t array -> t
val collapse : float array -> t -> int list
val has_zero_direction : int -> t -> bool
val remove_tile : int -> t -> unit
(* val decr_dir : int -> Adj_rules.directions -> t -> unit *)
