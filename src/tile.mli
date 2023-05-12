(** Representation of a cell. *)

type t
(** The abstract type of values representing a cell. *)

val make : Graphics.image -> string array -> t
(** [make img edges] is the cell with img [img] and edges [edges], and the rest 
    of the fields are empty lists *)

val get_img : t -> Graphics.image
(** [get_img cell] is the img field of [cell] *)

val analyze : t -> t array -> t
(** [analyze curr_cell cells] is the curr_cell with constraint fields up, left, 
    down right filled with the indexes of the cells it can be tiled next to *)

val from_json : Yojson.Basic.t -> t array
(** [from_json j] is the tiles that [j] represents. Requires: [j] is a valid
        JSON tiles representation. *)
