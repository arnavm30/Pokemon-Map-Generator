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
