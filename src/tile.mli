(** Representation of a tile. *)

type t
(** The abstract type of values representing a tile. *)

val make : Graphics.image -> string array -> t
(** [make img edges] is the tile with img [img] and edges [edges], and the rest 
    of the fields are empty lists *)

val get_img : t -> Graphics.image
(** [get_img tile] is the img field of [tile] *)

val get_up : t -> int list
(** [get_up tile] is the up field of [tile], contains the index of all the tiles 
    that can go on top of [tile] *)

val get_right : t -> int list
(** [get_right tile] is the right field of [tile], contains the index of all the tiles 
    that can go to the right of [tile] *)

val get_down : t -> int list
(** [get_down tile] is the down field of [tile], contains the index of all the tiles 
    that can go below of [tile] *)

val get_left : t -> int list
(** [get_left tile] is the elft field of [tile], contains the index of all the tiles 
    that can go to the left of [tile] *)

val analyze : t -> t array -> t
(** [analyze curr_cell cells] is the curr_cell with constraint fields up, left, 
    down right filled with the indexes of the cells it can be tiled next to *)

val from_json : Yojson.Basic.t -> t array * (string * (string * int) list) list
(** [from_json j] is the (tiles, size placements) that [j] represents. 
    Requires: [j] is a valid JSON tiles representation. *)

val copy : t -> t
