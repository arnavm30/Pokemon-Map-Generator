(** Utility functions relating to float arrays and to_string *)

val sumf : float array -> float
(** [sumf flt_arr] is the float sum of [flt_arr] *)

val sum : int array -> int
(** [sum i_arr] is the int sum of [i_arr] *)

val ( +++. ) : float array -> float array -> float array
(** [f_arr_1 +++. f_arr_2] is the element-wise sum of [f_arr_1] and [f_arr_2] *)

val ( ---. ) : float array -> float array -> float array
(** [f_arr_1 ---. f_arr_2] is the element-wise subtraction of [f_arr_2] from [f_arr_1] *)

val ( ***. ) : float array -> float array -> float array
(** [f_arr_1 ***. f_arr_2] is the element-wise product of [f_arr_1] and [f_arr_2] *)

val ( ///. ) : float array -> float array -> float array
(** [f_arr_1 ///. f_arr_2] is the element-wise division of [f_arr_1] over [f_arr_2] *)

val string_of_int_pair : int * int -> string
(** [string_of_int_pair (x,y)] is the pair [(x,y)] as a string *)

val string_of_int_list : int list -> string
(** [string_of_int_list lst] is the int list [lst] as a string *)
