(**  Main algorithm: *)

val wfc : int -> int -> int -> float array -> Adj_rules.t -> State.t
(** [wfc x y num_tiles ws adj_rules] runs wfc and outputs the resulting state *)

(* exposed for testing *)
val init : int -> int -> int -> float array -> Adj_rules.t -> State.t
(** [init x y num_tiles ws adj_rules] is the initial state of wf *)

val run : float array -> State.t -> State.result
(** [run ws st] runs [run_once] until all cells in [st] have collapsed
    or a contradiction occurs *)

val run_once : float array -> State.t -> State.result
(** [run_once ws st] is one cycle of wfc on [st] with weights [ws] *)
