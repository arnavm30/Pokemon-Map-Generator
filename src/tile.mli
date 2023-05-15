(** Representation of a tile. *)

type t
(** The abstract type of values representing a tile. *)

val make : Graphics.image -> float -> string list array -> t
(** [make img weight edges] is the tile with img [img], weight [weight], and 
    edges [edges], and the rest of the fields are empty lists *)

val get_img : t -> Graphics.image
(** [get_img tile] is the img field of [tile] *)

val analyze : t -> t array -> t
(** [analyze curr_cell cells] is the curr_cell with constraint fields up, left, 
    down right filled with the indexes of the cells it can be tiled next to *)

val create_adj_rules : t array -> Adj_rules.t
(** [create_adj_rules tiles] is the adjacency rules of tiles [tiles] *)

val create_weights : t array -> float array
(** [create_weights tiles] is the weights of tiles [tiles] *)

val from_json : Yojson.Basic.t -> t array * (string * (string * int) list) list
(** [from_json j] is the (tiles, size placements) that [j] represents. 
    Requires: [j] is a valid JSON tiles representation. *)

val copy : t -> t
(** [copy t] is new tile with same info as tile [t] *)
