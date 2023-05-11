(** Representation of a button.

    This module handles functionality of buttons, including creating buttons 
    and reacting to events related to the button.
 *)

type t
(** The abstract type of values representing buttons. *)

val make : int -> int -> int -> int -> Graphics.color -> string -> t
(** [make x y w h c str] is the button drawn in the window with bottom left corner
    at ([x],[y]) with width [w], height [h], color [c], and text [str] across*)

val press : t -> (unit -> unit) -> unit
(** [press b f] is whether mouse button press is inside button [b], if so then 
    execute [f] *)
