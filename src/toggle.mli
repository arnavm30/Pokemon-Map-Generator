(** Representation of a toggle.

    This module handles functionality of toggles, including creating toggles 
    and reacting to events related to the toggle.
 *)

type t
(** The abstract type of values representing toggles. *)

val make : int -> int -> int -> Graphics.image -> int -> t
(** [make x y r img index] is the toggles for tile with image [img] and [index], 
      drawn in the window with bottom left corner at ([x],[y]) and radius [r] *)

val is_on : t -> bool
(** [is_on tog] is whehter toggle [tog] is on or not *)

val get_index : t -> int
(** [get_index tog] is the tile_index of toggle [tog] *)

val draw : t -> unit
(** [draw toggle] renders [toggle] to window as a filled circle *)

val mem : int * int -> t -> bool
(** [mem (x,y) toggle] is whether mouse point [(x,y)] is in toggle [toggle] *)

val press : t -> (bool -> unit) -> unit
(** [press t f] is toggling color of toggle [t] and executing [f] *)
