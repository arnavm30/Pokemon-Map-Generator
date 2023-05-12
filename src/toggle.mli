(** Representation of a toggle.

    This module handles functionality of toggles, including creating toggles 
    and reacting to events related to the toggle.
 *)

type t
(** The abstract type of values representing toggles. *)

val make : int -> int -> int -> Graphics.image -> t
(** [make x y w h cells] is the toggles for [cells] drawn in the window with 
      bottom left corner at ([x],[y]) with width [w], height [h] *)

val draw : t -> unit
(** [draw toggle] renders [toggle] to window as a filled circle *)

val mem : int * int -> t -> bool
(** [mem (x,y) toggle] is whether mouse point [(x,y)] is in toggle [toggle] *)

val press : t -> (bool -> unit) -> unit
(** [press t f] is toggling color of toggle [t] and executing [f] *)
