(** Representation of a cell within state. *)

type directions = {
  mutable up : int;
  mutable right : int;
  mutable down : int;
  mutable left : int;
}
(** The abstract type of directions. *)

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
(** The abstract type of values representing cells. *)

exception Contradiction
(** exception of when there's a contradiction for what should go in a cell *)

val cmp : t -> t -> int
(** [cmp a b] is comparison of entropy between [a] and [b], if entropy of [a] is
   less than or equal to entropy of [b] then return -1 else return 1 *)

val check_collapsed : t -> unit
(** [check_collapsed c] changes collapsed field of [c] to true if there's only
   one option left for [c] *)

val check_contradiction : t -> unit
(** [check_contradiction c] raises exception Contradiction if 0 options left for [c] *)

val make : int -> float -> float -> directions array -> int * int -> t
(** [make num_tiles sw swlw enablers coords] is cell with options and sum_of_ones = [num_tiles],
   sum_of_weight = [sw], sum_of_weight_log_weights = [swlw], tile_enablers = [enablers],
   and coords = [coords] *)

val make_test : Tile.t array -> t
(** [make_test tiles] is test cell of a randomn tile from [tiles] *)

val collapse : float array -> t -> int list
(** [collapse ws c] collapses cell [c] *)

val has_zero_direction : int -> t -> bool
(** [has_zero_direction tile c] is if tile [tile] from cell [c] has any of its
   directions = 0 *)

val remove_tile : int -> t -> unit
(** [remove_tile tile c] removes tile [tile] from this cell [c] *)
