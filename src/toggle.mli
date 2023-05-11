(** Representation of a toggle.

    This module handles functionality of toggles, including creating toggles 
    and reacting to events related to the toggle.
 *)

type t
(** The abstract type of values representing toggles. *)

val make : int -> int -> int -> int -> Tile.t array -> t
(** [make x y w h cells] is the toggles for [cells] drawn in the window with 
      bottom left corner at ([x],[y]) with width [w], height [h] *)

val press : t -> (unit -> unit) -> unit
(** [press t f] is whether mouse button press is inside a toggle of [t], if so 
    then execute [f] on that toggle *)
