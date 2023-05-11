type directions = { up : int; right : int; down : int; left : int }

type t = {
  mutable collapsed : bool;
  options : float array;
  mutable sum_of_ones : int;
  mutable sum_of_weights : float;
  mutable sum_of_weight_log_weights : float;
  noise : float;
  tile_enablers : directions;
}

val cmp : t -> t -> int
val check_collapsed : t -> unit
val make : int -> float -> float -> t
val make_test : Tile.t array -> t
val collapse : t -> int
