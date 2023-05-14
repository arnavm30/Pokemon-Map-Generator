val sumf : float array -> float
(** [sumf flt_arr] is the float sum of [flt_arr] *)

val sum : int array -> int
(** [sum i_arr] is the int sum of [i_arr] *)

val ( +++. ) : float array -> float array -> float array
val ( ---. ) : float array -> float array -> float array
val ( ***. ) : float array -> float array -> float array
val ( ///. ) : float array -> float array -> float array

(* val entropy : float array -> float *)
(* [entropy f_arr] is the shannon entropy of [f_arr], array of float weights *)

val string_of_int_pair : int * int -> string
(** [string_of_int_pair (x,y)] is the pair [(x,y)] as a string *)

val string_of_int_list : int list -> string
(** [string_of_int_list lst] is the int list [lst] as a string *)
