(** Representation of a panel to select item from a list.

    This module handles functionality of list panels, including creating them 
    and reacting to events related to the them.
 *)

type t
(** The abstract type of values representing list panels. *)

val make : int -> int -> int -> int -> string list -> int -> t
(** [make x y w h strs active] is the list panel drawn in the window with bottom 
    left corner at ([x],[y]) with width [w], height [h], color [c], and texts in 
    [strs] displayed where index [active] is darkened as active *)

val get_active_text : t -> string
(** [get_active_text p] is the text of the active option of the list panel [p] *)

val draw : t -> unit
(** [draw p] renders list panel [p] to window as a several filled rectange *)

val mem : int * int -> t -> bool
(** [mem (x,y) p] is whether mouse point [(x,y)] is in list panel [p] *)

val press : t -> int * int -> (unit -> unit) -> unit
(** [press p (x,y) f] will find which item in lsit panel [p] was selected based 
    on mouse coordinates [(x,y)] and execute [f] *)
