(** Representation of a button.

    This module handles functionality of buttons, including creating buttons 
    and reacting to events related to the button.
 *)

type t
(** The abstract type of values representing buttons. *)

val make : int -> int -> int -> int -> Graphics.color -> string -> t
(** [make x y w h c str] is the button drawn in the window with bottom left corner
    at ([x],[y]) with width [w], height [h], color [c], and text [str] across *)

val draw : t -> unit
(** [draw b] renders button [b] to window as a filled rectange *)

val mem : int * int -> t -> bool
(** [mem (x,y) b] is whether mouse point [(x,y)] is in button [b] *)

val press : t -> (unit -> unit) -> unit
(** [press b f] is execute [f] *)

val allow_press : t -> unit
val disallow_press : t -> unit
